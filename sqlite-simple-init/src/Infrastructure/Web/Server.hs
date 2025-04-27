{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Web.Server
    ( startServer
    ) where

import           Application.UserService     (UserService (..))

import           Control.Exception           (SomeException, try)

import           Data.Aeson                  (object, (.=))

import           Domain.UserModel            (User (..))

import           Flow                        ((<|))

import           Network.HTTP.Types
import           Network.Wai.Middleware.Cors

import           Web.Scotty

-- Web server implementation
startServer :: UserService a => a -> IO ()
startServer userService = scotty 3000 <| do
    -- Enable CORS
    middleware simpleCors

    -- Serve static files
    get "/favicon.ico" <| do
        setHeader "Content-Type" "image/x-icon"
        file "static/favicon.ico"

    get "/css/:filename" <| do
        filename <- param "filename"
        setHeader "Content-Type" "text/css"
        file ("static/css/" <> filename)

    get "/js/:filename" <| do
        filename <- param "filename"
        setHeader "Content-Type" "application/javascript"
        file ("static/js/" <> filename)

    get "/index.html" <| do
        setHeader "Content-Type" "text/html"
        file "static/index.html"

    -- Redirect root to index.html
    get "/" <| do
        redirect "/index.html"

    -- GET /users - List all users
    get "/users" <| do
        users <- liftAndCatchIO <| getAllUsers userService
        json users

    -- GET /users/:id - Get user by ID
    get "/users/:id" <| do
        uid <- param "id"
        maybeUser <- liftAndCatchIO <| getUserById userService uid
        case maybeUser of
            Just user -> json user
            Nothing   -> status status404

    -- POST /users - Create new user
    post "/users" <| do
        name <- param "name"
        email <- param "email"
        password <- param "password"
        result <- liftAndCatchIO <| try <| createUser userService name email password
        case result of
            Right user -> do
                status status201
                json user
            Left (e :: SomeException) -> do
                status status500
                json <| object ["error" .= (show e :: String)]

    -- PUT /users/:id - Update user
    put "/users/:id" <| do
        uid <- param "id"
        name <- param "name"
        email <- param "email"
        password <- param "password"
        result <- liftAndCatchIO <| try <| updateUser userService uid name email password
        case result of
            Right success ->
                if success
                    then do
                        -- Get the updated user to return it
                        userResult <- liftAndCatchIO <| try <| getUserById userService uid
                        case userResult of
                            Right maybeUser ->
                                case maybeUser of
                                    Just user -> do
                                        status status200
                                        json user
                                    Nothing -> do
                                        status status404
                                        json <| object ["error" .= ("User not found after update" :: String)]
                            Left (e :: SomeException) -> do
                                status status500
                                json <| object ["error" .= (show e :: String)]
                    else do
                        status status404
                        json <| object ["error" .= ("User not found" :: String)]
            Left (e :: SomeException) -> do
                status status500
                json <| object ["error" .= (show e :: String)]

    -- DELETE /users/:id - Delete user
    delete "/users/:id" <| do
        uid <- param "id"
        result <- liftAndCatchIO <| try <| deleteUser userService uid
        case result of
            Right success ->
                if success
                    then status status204
                    else do
                        status status404
                        json <| object ["error" .= ("User not found" :: String)]
            Left (e :: SomeException) -> do
                status status500
                json <| object ["error" .= (show e :: String)]
