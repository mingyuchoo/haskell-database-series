{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repository.SQLiteUserRepository
    ( SQLiteUserRepository(..)
    , initDB
    , createUser
    , getAllUsers
    , getUserById
    , updateUser
    , deleteUser
    ) where

import           Domain.Model                (User(..))
import           Application.UserService     (UserService(..))
import           Data.Time                   (getCurrentTime)
import           Database.SQLite.Simple
import           Flow                        ((<|))

-- SQLite implementation of UserService
data SQLiteUserRepository = SQLiteUserRepository Connection

-- Initialize database
initDB :: IO Connection
initDB = do
    conn <- open "users.db"
    execute_ conn createUsersTable
    return conn
    where
        createUsersTable = Query <|
            "CREATE TABLE IF NOT EXISTS users (    \
            \id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \name TEXT NOT NULL,                   \
            \email TEXT NOT NULL UNIQUE,           \
            \password TEXT NOT NULL,               \
            \created_at DATETIME NOT NULL,         \
            \updated_at DATETIME NOT NULL)"

-- UserService implementation for SQLite
instance UserService SQLiteUserRepository where
    -- Create a new user
    createUser (SQLiteUserRepository conn) name email password = do
        now <- getCurrentTime
        let user = User Nothing name email password now now
        execute conn "INSERT INTO users (name, email, password, created_at, updated_at) VALUES (?, ?, ?, ?, ?)"
               (userName user, userEmail user, userPassword user, createdAt user, updatedAt user)
        rowId <- lastInsertRowId conn
        return <| user { userId = Just (fromIntegral rowId) }

    -- Get all users
    getAllUsers (SQLiteUserRepository conn) =
        query_ conn "SELECT id, name, email, password, created_at, updated_at FROM users"

    -- Get user by ID
    getUserById (SQLiteUserRepository conn) uid = do
        users <- query conn "SELECT id, name, email, password, created_at, updated_at FROM users WHERE id = ?" (Only uid)
        return <| case users of
            [user] -> Just user
            _      -> Nothing

    -- Update user
    updateUser (SQLiteUserRepository conn) uid name email password = do
        now <- getCurrentTime
        executeNamed conn
            "UPDATE users SET name = :name, email = :email, password = :pwd, updated_at = :updated WHERE id = :uid"
            [ ":name" := name
            , ":email" := email
            , ":pwd" := password
            , ":updated" := now
            , ":uid" := uid
            ]
        -- Get the updated user to return it
        maybeUser <- getUserById (SQLiteUserRepository conn) uid
        case maybeUser of
            Just user -> return True
            Nothing -> return False

    -- Delete user
    deleteUser (SQLiteUserRepository conn) uid = do
        executeNamed conn "DELETE FROM users WHERE id = :uid" [":uid" := uid]
        return True
