{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import           Control.Applicative ((<|>))
import           Control.Lens
import           Control.Monad.Reader
import           Data.Maybe (isJust, listToMaybe, isNothing, catMaybes)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (getCurrentTime, UTCTime)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Network.Discord hiding (insert)
import           Pipes (Proxy, X)
import           System.Environment (getArgs)
import           System.Exit (exitSuccess)
import           System.Process (callProcess)

import DB
import           Patreon hiding (Patron)

createPatron :: MonadIO m => Text -> ReaderT SqlBackend m ()
createPatron discord = do
    patron <- getBy $ UniqueDiscordId discord
    when (not $ isJust patron) $ do
        insert $ Patron Nothing discord Nothing Nothing Nothing Nothing
        pure ()

setAccountName :: Text -> EntityField Patron (Maybe Text) -> EntityField Patron (Maybe Text) -> Message -> Proxy X () c c' DiscordM ()
setAccountName command fieldActive fieldStandby Message{..} =
    when (command `T.isPrefixOf` messageContent) $ do
        let author = T.pack . show $ userId messageAuthor
            authorMention = "<@" <> author <> ">"
            minecraftUser = T.drop 1 . T.dropWhile (/=' ') $ messageContent
        if minecraftUser == ""
           then fetch' $ CreateMessage messageChannel (authorMention <> "\nUsage: " <> command <> " <Your in-game nick here>") Nothing
           else do
               created <- liftIO . runSqlite database . asSqlBackend $ do
                   existing <- listToMaybe <$> selectList [DiscordId ==. author, fieldActive !=. Nothing] []
                   case existing of
                     Nothing -> do
                         createPatron author
                         updateWhere [DiscordId ==. author] [fieldActive =. Just minecraftUser]
                         pure True
                     Just patron -> do
                         updateWhere [DiscordId ==. author] [fieldStandby =. Just minecraftUser]
                         pure False
               if created
                 then fetch' $ CreateMessage messageChannel ("Your nickname has been set to " <> minecraftUser) Nothing
                 else fetch' $ CreateMessage messageChannel
                                 ("Your nickname has been set to " <> minecraftUser <> " and will become active next month") Nothing

whiteList :: Text -> Int -> [Pledge] -> IO ()
whiteList server minPledge pledges = do
    let eligiblePledges = filter (isNothing . view declinedSince) . filter ((>=minPledge) . view amount) $ pledges
        discordIds = catMaybes $ view discord <$> eligiblePledges
    patrons <- runSqlite database . asSqlBackend $ getPatron `mapM` discordIds >>= pure . fmap entityVal . catMaybes >>= mapM activateStandbys
    let players = catMaybes $ view minecraftPlayerActive <$> patrons

    void . sequence $ msm . ([T.unpack server, "wl", "add"] <>) . (:[]) . T.unpack <$> players

   where getPatron discordId = getBy $ UniqueDiscordId discordId
         activateStandbys patron = do
             let player = patron ^. minecraftPlayerStandby <|> patron ^. minecraftPlayerActive
                 spectator = patron ^. minecraftSpectatorStandby <|> patron ^. minecraftSpectatorActive
             updateWhere [DiscordId ==. patron ^. discordId] [ MinecraftPlayerActive =. player
                                                             , MinecraftSpectatorActive =. spectator
                                                             ]
             pure $ patron & minecraftPlayerActive .~ player
                           & minecraftPlayerStandby .~ Nothing
                           & minecraftSpectatorActive .~ spectator
                           & minecraftSpectatorStandby .~ Nothing
         msm = callProcess "echo"

whiteListPlayers :: [Pledge] -> IO ()
whiteListPlayers pledges = whiteList "patreon" 500 pledges >> whiteList "anarchy" 1000 pledges
        

helpStr :: Text
helpStr = T.unlines $
    [ "Commands:"
    , "!help - Display this help"
    , "!player <nick> - Sets your **main** Minecraft nickname"
    , "!spectator <nick> - Sets your **spectating** Minecraft nickname"
    , "**Please note:** You will not be able to change your nickname for one month after you set it, so make sure it is correct!"
    ]

discordBot :: DiscordCredentials -> IO ()
discordBot credentials = do
    runBot (Bot $ credentials ^. token . to T.unpack) $ do
        with ReadyEvent $ \(Init v u _ _ _) ->
            liftIO . putStrLn $ "Connected to gateway " ++ show v ++ " as user " ++ show u

        with MessageCreateEvent $ \message@Message{..} -> do
            when ("!help" `T.isPrefixOf` messageContent) $ do
                fetch' $ CreateMessage messageChannel helpStr Nothing
            setAccountName "!player" MinecraftPlayerActive MinecraftPlayerStandby message
            setAccountName "!spectator" MinecraftSpectatorActive MinecraftSpectatorStandby message

main :: IO ()
main = do
    args <- getArgs
    credentials <- runSqlite database $ do
        runMigration migrateAll

        discordCreds <- head <$> selectList [] [LimitTo 1, Asc Token]
        patronCreds <- head <$> selectList [] [LimitTo 1, Asc ClientId]

        pure $ Credentials (entityVal discordCreds) (entityVal patronCreds)

    if "--whitelist" `elem` args
      then do
          Right pledges <- getPledges $ credentials ^. patreon
          whiteListPlayers pledges
      else discordBot $ credentials ^. discord


mockPledges :: [Pledge]
mockPledges = [ Pledge { pledgeAmount = 1000
                       , pledgeUser = "123"
                       , pledgePledgedSince = "2017-09-15T12:01:10+00:00"
                       , pledgeDeclinedSince = Nothing
                       , pledgeDiscord = Just "155437731072311296"
                       }
              ]



test :: IO ()
test = do
    credentials <- runSqlite database $ do
        runMigration migrateAll

        discordCreds <- head <$> selectList [] [LimitTo 1, Asc Token]
        patronCreds <- head <$> selectList [] [LimitTo 1, Asc ClientId]

        pure $ Credentials (entityVal discordCreds) (entityVal patronCreds)
    pledges <- Patreon.getPledges (credentials ^. patreon)
    print pledges
