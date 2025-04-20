{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( User (..)
    , startApp
    ) where

-- Domain Layer
import           Domain.Model                                   (User (..))

-- Application Layer
import           Application.UserService
    ( UserService (..)
    )

-- Infrastructure Layer
import           Infrastructure.Repository.SQLiteUserRepository
    ( SQLiteUserRepository (..)
    , initDB
    )
import           Infrastructure.Web.Server                      (startServer)

-- Main application entry point that composes all layers
startApp :: IO ()
startApp = do
    -- Initialize the database connection
    conn <- initDB

    -- Create the repository with the connection
    let repository = SQLiteUserRepository conn

    -- Start the web server with the repository
    startServer repository
