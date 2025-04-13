{-# LANGUAGE OverloadedStrings #-}

module Application.UserService
    ( UserService(..)
    , createUser
    , getAllUsers
    , getUserById
    , updateUser
    , deleteUser
    )
    where

import           Domain.Model                (User(..))
import           Data.Time                   (getCurrentTime)
import           Flow                        ((<|))

-- Service interface for user operations
class UserService a where
    createUser :: a -> String -> String -> String -> IO User
    getAllUsers :: a -> IO [User]
    getUserById :: a -> Int -> IO (Maybe User)
    updateUser :: a -> Int -> String -> String -> String -> IO Bool
    deleteUser :: a -> Int -> IO Bool
