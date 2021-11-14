{-# LANGUAGE OverloadedStrings #-}

module Lib
    where

import           Control.Applicative
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow

data TestField = TestField { id   :: Int
                           , name :: String
                           } deriving (Show)

instance FromRow TestField where
    fromRow = TestField <$> field <*> field

instance ToRow TestField where
    toRow (TestField id name) = toRow (id, name)

someFunc :: IO ()
someFunc = do
    conn <- open "test.db"

    execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, name TEXT)"

    execute conn "INSERT INTO test (name) VALUES (?)"
        (Only ("Jacob" :: String))

    execute conn "INSERT INTO test (id, name) VALUES (?, ?)"
        (TestField 100 "Henry")

    rowId <- lastInsertRowId conn
    executeNamed conn "UPDATE test SET name = :name WHERE id = :id"
        [ ":name" := ("Thomas (updated)":: String)
        , ":id"   := rowId
        ]

    row <- query_ conn "SELECT * FROM test" :: IO [TestField]
    mapM_ print row

    execute conn "DELETE FROM test WHERE id = ?"
        (Only rowId)

    close conn
