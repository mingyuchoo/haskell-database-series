{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    where

import           Control.Applicative
import           Data.Time
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           GHC.Generics

data User = User { userId       :: Maybe Int
                 , userName     :: String
                 , userEmail    :: String
                 , userPassword :: String
                 , createdAt    :: UTCTime
                 , updatedAt    :: UTCTime
                 }
     deriving (Generic, Show)

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
        createUsersTable = Query $
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
    return $ user { userId = Just (fromIntegral rowId) }

-- Get all users
getAllUsers :: Connection -> IO [User]
getAllUsers conn =
    query_ conn "SELECT id, name, email, password, created_at, updated_at FROM users"

-- Get user by ID
getUserById :: Connection -> Int -> IO (Maybe User)
getUserById conn uid = do
    users <- query conn "SELECT id, name, email, password, created_at, updated_at FROM users WHERE id = ?" (Only uid)
    return $ case users of
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

-- Example usage
someFunc :: IO ()
someFunc = do
    conn <- initDB

    -- Create some users
    user1 <- createUser conn "John Doe" "john@example.com" "password123"
    user2 <- createUser conn "Jane Smith" "jane@example.com" "password456"

    -- Get all users
    users <- getAllUsers conn
    putStrLn "All users:"
    mapM_ print users

    -- Get user by ID
    case userId user1 of
        Just uid -> do
            maybeUser <- getUserById conn uid
            putStrLn "Found user:"
            print maybeUser

            -- Update user
            success <- updateUser conn uid "John Doe Updated" "john.updated@example.com" "newpassword"
            putStrLn $ "Update success: " ++ show success

            -- Delete user
            deleted <- deleteUser conn uid
            putStrLn $ "Delete success: " ++ show deleted

        Nothing -> putStrLn "Failed to get user ID"

    close conn
