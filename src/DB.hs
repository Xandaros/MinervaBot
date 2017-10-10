{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, OverloadedStrings #-}
module DB where

import Control.Lens
import Control.Monad.Reader
import Database.Persist.Sqlite
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH

database :: Text
database = "PatreonBot.db"

share [ mkPersist sqlSettings {mpsGenerateLenses = True, mpsPrefixFields = False}
      , mkMigrate "migrateAll"] [persistLowerCase|
Patron
    patreonId Text Maybe
    discordId Text
    minecraftPlayerActive Text Maybe
    minecraftSpectatorActive Text Maybe
    minecraftPlayerStandby Text Maybe
    minecraftSpectatorStandby Text Maybe
    UniqueDiscordId discordId
    deriving Show
PatreonCredentials
    clientId Text
    clientSecret Text
    refreshToken Text
    authToken Text Maybe
    validUntil UTCTime Maybe
    deriving Show
DiscordCredentials
    token Text
    deriving Show
|]

declareFields([d|
    data Credentials = Credentials { credentialsDiscord :: DiscordCredentials 
                                   , credentialsPatreon :: PatreonCredentials
                                   }
    |])


asSqlBackend :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackend = id
