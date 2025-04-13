{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Lib
    ( startApp
    , User(..)
    ) where

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Time
import           Database.SQLite.Simple
import           GHC.Generics
import           Network.HTTP.Types
import           Network.Wai.Middleware.Cors
import           Web.Scotty
import           Flow ((<|))


data User = User { userId       :: Maybe Int
                 , userName     :: String
                 , userEmail    :: String
                 , userPassword :: String
                 , createdAt    :: UTCTime
                 , updatedAt    :: UTCTime
                 }
     deriving (Generic, Show, FromJSON, ToJSON)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
    toRow (User id name email password created updated) =
        toRow (id, name, email, password, created, updated)

-- Initialize database
initDB :: IO Connection
initDB = do
    conn <- open "users.db"
    execute_ conn createUsersTable
    return conn
    where
        createUsersTable = Query <|
            "CREATE TABLE IF NOT EXISTS users (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \name TEXT NOT NULL,\
            \email TEXT NOT NULL UNIQUE,\
            \password TEXT NOT NULL,\
            \created_at DATETIME NOT NULL,\
            \updated_at DATETIME NOT NULL)"

-- Create a new user
createUser :: Connection -> String -> String -> String -> IO User
createUser conn name email password = do
    now <- getCurrentTime
    let user = User Nothing name email password now now
    execute conn "INSERT INTO users (name, email, password, created_at, updated_at) VALUES (?, ?, ?, ?, ?)"
           (userName user, userEmail user, userPassword user, createdAt user, updatedAt user)
    rowId <- lastInsertRowId conn
    return <| user { userId = Just (fromIntegral rowId) }

-- Get all users
getAllUsers :: Connection -> IO [User]
getAllUsers conn =
    query_ conn "SELECT id, name, email, password, created_at, updated_at FROM users"

-- Get user by ID
getUserById :: Connection -> Int -> IO (Maybe User)
getUserById conn uid = do
    users <- query conn "SELECT id, name, email, password, created_at, updated_at FROM users WHERE id = ?" (Only uid)
    return <| case users of
        [user] -> Just user
        _      -> Nothing

-- Update user
updateUser :: Connection -> Int -> String -> String -> String -> IO Bool
updateUser conn uid name email password = do
    now <- getCurrentTime
    executeNamed conn
        "UPDATE users SET name = :name, email = :email, password = :pwd, updated_at = :updated WHERE id = :uid"
        [ ":name" := name
        , ":email" := email
        , ":pwd" := password
        , ":updated" := now
        , ":uid" := uid
        ]
    return True

-- Delete user
deleteUser :: Connection -> Int -> IO Bool
deleteUser conn uid = do
    executeNamed conn "DELETE FROM users WHERE id = :uid" [":uid" := uid]
    return True

-- Web server
startApp :: IO ()
startApp = do
    conn <- initDB
    scotty 3000 <| do
        -- Enable CORS
        middleware simpleCors

        -- GET /users - List all users
        get "/users" <| do
            users <- liftAndCatchIO <| getAllUsers conn
            json users

        -- GET /users/:id - Get user by ID
        get "/users/:id" <| do
            uid <- param "id"
            maybeUser <- liftAndCatchIO <| getUserById conn uid
            case maybeUser of
                Just user -> json user
                Nothing -> status status404

        -- POST /users - Create new user
        post "/users" <| do
            name <- param "name"
            email <- param "email"
            password <- param "password"
            user <- liftAndCatchIO <| createUser conn name email password
            status status201
            json user

        -- PUT /users/:id - Update user
        put "/users/:id" <| do
            uid <- param "id"
            name <- param "name"
            email <- param "email"
            password <- param "password"
            success <- liftAndCatchIO <| updateUser conn uid name email password
            if success
                then status status200
                else status status404

        -- DELETE /users/:id - Delete user
        delete "/users/:id" <| do
            uid <- param "id"
            success <- liftAndCatchIO <| deleteUser conn uid
            if success
                then status status204
                else status status404
