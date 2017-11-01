{-# LANGUAGE RecordWildCards, TypeOperators, DataKinds, OverloadedStrings, DeriveGeneric, LambdaCase #-}
{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, TypeFamilies, TemplateHaskell, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Patreon where

import           Control.Applicative (empty, (<|>))
import           Control.Lens
import           Control.Monad (when, join)
import           Data.Aeson hiding ((.=))
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BS8
import           Data.Either.Combinators (fromRight)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import           Data.Monoid ((<>))
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime, diffUTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Vector as V
import           Database.Persist
import           Database.Persist.Sqlite
import           GHC.Generics (Generic)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.URI
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.Client
import           Web.Internal.FormUrlEncoded (ToForm(..), Form(..))

import DB hiding (refreshToken, Patron)
import qualified DB

campaignId :: Int
campaignId = 867697

data VNDJSON

instance Accept VNDJSON where
    contentType _ = "application/vnd.api+json"

instance FromJSON a => MimeUnrender VNDJSON a where
    mimeUnrender _ = eitherDecodeLenient

type PatreonAPI = "oauth2" :> "token" :> ReqBody '[FormUrlEncoded] TokenQuery :> Post '[JSON] TokenResponse
             :<|> "oauth2" :> "api" :> "campaigns" :> Header "Authorization" Text
                  :> Capture "campaign" Int :> "pledges"
                  :> QueryParam "page[count]" Int :> QueryParam "sort" String :> QueryParam "page[cursor]" Text
                  :> Get '[VNDJSON] PledgeResponse
                              
           

data TokenQuery = TokenQuery { grant_type :: Text
                             , refresh_token' :: Text
                             , client_id :: Text
                             , client_secret :: Text
                             }
instance ToForm TokenQuery where
    toForm TokenQuery{..} = Form $ HM.fromList [ ("grant_type", [grant_type])
                                               , ("refresh_token", [refresh_token'])
                                               , ("client_id", [client_id])
                                               , ("client_secret", [client_secret])
                                               ]

data TokenResponse = TokenResponse { access_token :: Text
                                   , refresh_token :: Text
                                   , expires_in :: Int
                                   , scope :: Text
                                   , token_type :: Text
                                   } deriving (Generic)

instance ToJSON TokenResponse
instance FromJSON TokenResponse

data Pledge = Pledge { pledgeAmount :: Int
                     , pledgeUser :: Text
                     , pledgePledgedSince :: Text
                     , pledgeDeclinedSince :: Maybe Text
                     , pledgeDiscord :: Maybe Text
                     } deriving (Show)

data PledgeResponse = PledgeResponse { pledgeResponseInfo :: ListWrapper PledgeJ
                                     , pledgeResponsePatronData :: ListWrapper Patron
                                     , pledgeResponseNextPledge :: Maybe Text
                                     } deriving (Show)

instance FromJSON PledgeResponse where
    parseJSON v@(Object o) = PledgeResponse <$> o .: "data"
                                            <*> o .: "included"
                                            <*> pure (v ^? key "links" . key "next" . _String)

data PledgeJ = PledgeJ { pledgeJAmount :: Int
                       , pledgeJUser :: Text
                       , pledgeJPledgedSince :: Text
                       , pledgeJDeclinedSince :: Maybe Text
                       } deriving (Show)

instance FromJSON PledgeJ where
    parseJSON v = do
        amount <- maybe empty pure $ v ^? key "attributes" . key "amount_cents" . _Integral
        user <- maybe empty pure $ v ^? key "relationships" . key "patron" . key "data" . key "id" . _String
        pledgedSince <- maybe empty pure $ v ^? key "attributes" . key "created_at" . _String
        let declinedSince = v ^? key "attributes" . key "declined_since" . _String
        pure $ PledgeJ amount user pledgedSince declinedSince

data Patron = Patron { patronId :: Text
                     , patronDiscord :: Text
                     } deriving (Show)

instance FromJSON Patron where
    parseJSON v = do
        uid <- maybe empty pure $ v ^? key "id" . _String
        discordId <- maybe empty pure $ v ^? key "attributes" . key "social_connections" . key "discord" . key "user_id" . _String
        pure $ Patron uid discordId


data ListWrapper a = ListWrapper {unListWrapper :: [a]}

deriving instance Show a => Show (ListWrapper a)

instance FromJSON a => FromJSON (ListWrapper a) where
    parseJSON (Array v) = ListWrapper <$> (fmap catMaybes . sequence $ (\x -> (Just <$> parseJSON x) <|> pure Nothing) <$> V.toList v)

instance Wrapped (ListWrapper a) where
    type Unwrapped (ListWrapper a) = [a]
    _Wrapped' = iso unListWrapper ListWrapper

makeFields ''PledgeJ
makeFields ''Pledge
makeFields ''Patron
makeFields ''PledgeResponse


patreonAPI :: Proxy PatreonAPI
patreonAPI = Proxy

refreshTokenC :: TokenQuery -> ClientM TokenResponse
pledgesC :: Maybe Text -> Int -> Maybe Int -> Maybe [Char] -> Maybe Text -> ClientM PledgeResponse
refreshTokenC :<|> pledgesC = client patreonAPI

runPatreon :: ClientM a -> IO (Either ServantError a)
runPatreon query = do
    manager <- newManager tlsManagerSettings
    res <- runClientM query (ClientEnv manager (BaseUrl Https "api.patreon.com" 443 ""))
    case res of
      Left err -> pure $ Left err
      Right a -> pure $ Right a

pledgeResponseToPledges :: PledgeResponse -> [Pledge]
pledgeResponseToPledges resp =
    let infos = resp ^. info . _Wrapped'
        patrons = resp ^. patronData . _Wrapped'
        discordInfo = discordLookup patrons
    in  do
        pledgeJ <- infos
        let discord = Map.lookup (pledgeJ ^. user) discordInfo
        pure $ Pledge (pledgeJ ^. amount) (pledgeJ ^. user) (pledgeJ ^. pledgedSince) (pledgeJ ^. declinedSince) discord
    where discordLookup :: [Patron] -> Map.Map Text Text
          discordLookup = Map.fromList . fmap (\(Patron uid did) -> (uid, did))

getDiscordPledge :: PatreonCredentials -> Text -> IO (Maybe Pledge)
getDiscordPledge creds discordId = getPledges creds >>= \case
    Left err -> pure $ Nothing
    Right pledges -> pure . listToMaybe . filter ((==Just discordId) . view discord) $ pledges

getPledges :: PatreonCredentials -> IO (Either ServantError [Pledge])
getPledges creds = getPledges' Nothing creds >>= \case
    Left err -> pure $ Left err
    Right responses -> pure . Right . join $ pledgeResponseToPledges <$> responses

getPledges' :: Maybe Text -> PatreonCredentials -> IO (Either ServantError [PledgeResponse])
getPledges' cursor creds = 
    runPatreon (pledgesC (("Bearer " <>) <$> creds ^. authToken) campaignId (Just 10) (Just "created") cursor) >>= \case
        Left err -> pure $ Left err
        Right response -> do
                case response ^. nextPledge of
                            Nothing -> pure $ Right [response]
                            Just next -> case parseCursor next of
                                           Nothing -> pure $ Right [response]
                                           Just nextCursor -> do
                                               nextPledges <- getPledges' (Just nextCursor) creds
                                               pure $ (response :) <$> nextPledges


refreshToken :: PatreonCredentials -> IO (Either ServantError PatreonCredentials)
refreshToken creds = do
    let query = TokenQuery "refresh_token" (creds ^. DB.refreshToken) (creds ^. clientId) (creds ^. clientSecret)
    runPatreon (refreshTokenC query) >>= \case
      Left err -> pure $ Left err
      Right TokenResponse{..} -> do
          now <- getCurrentTime
          let expireTime = addUTCTime (fromIntegral expires_in) now
          runSqlite database . asSqlBackend $ updateWhere [] [ RefreshToken =. refresh_token
                                                             , AuthToken =. Just access_token
                                                             , ValidUntil =. Just expireTime
                                                             ]
          pure $ Right $ creds & DB.refreshToken .~ refresh_token
                               & authToken .~ Just access_token
                               & validUntil .~ Just expireTime

refreshTokenIfNecessary :: PatreonCredentials -> IO PatreonCredentials
refreshTokenIfNecessary creds = do
    now <- getCurrentTime
    if diffUTCTime (fromMaybe (posixSecondsToUTCTime 0) $ creds ^. validUntil) now < 1*hours
       then fromRight creds <$> refreshToken creds
       else pure creds

    where seconds = 1
          minutes = 60*seconds
          hours = 60*minutes

parseCursor :: Text -> Maybe Text
parseCursor = join . join . fmap (lookup "page[cursor]" . parseQueryText) . listToMaybe . drop 1 . BS8.split '?' . TE.encodeUtf8
