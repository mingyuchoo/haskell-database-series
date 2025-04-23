{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Domain.UserModel
    ( User(..)
    ) where

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Time
import           Database.SQLite.Simple
import           GHC.Generics

-- Core domain entity
data User = User { userId       :: Maybe Int
                 , userName     :: String
                 , userEmail    :: String
                 , userPassword :: String
                 , createdAt    :: UTCTime
                 , updatedAt    :: UTCTime
                 }
     deriving (Generic, Show, FromJSON, ToJSON)

-- Database row mapping
instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
    toRow (User id name email password created updated) =
        toRow (id, name, email, password, created, updated)
