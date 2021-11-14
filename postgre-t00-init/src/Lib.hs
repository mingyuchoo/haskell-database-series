{-# LANGUAGE OverloadedStrings #-}

module Lib
    where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow


data TestField = TestField { id   :: Int
                           , name :: String
                           } deriving (Show)

instance FromRow TestField where
    fromRow = TestField <$> field <*> field

instance ToRow TestField where
    toRow (TestField id name) = toRow (id, name)


localPG :: ConnectInfo
localPG = defaultConnectInfo { connectHost = "127.0.0.1"
                             , connectDatabase = "postgres"
                             , connectUser = "postgres"
                             , connectPassword = "postgres"
                             }

someFunc :: IO ()
someFunc = do
    conn <- connect localPG
    cid <- createTest conn
    putStrLn $ "New Test: " ++ (show cid)

    ret <- updateTest conn
    putStrLn $ "Ret value: " ++ (show ret)

    row <- retrieveTest conn
    mapM_ print row

    ret <- deleteTest conn
    putStrLn $ "Ret value: " ++ (show ret)


createTest :: Connection -> IO [Only Int]
createTest conn = query conn "INSERT INTO test (name) VALUES (?) RETURNING id" $ (Only ("Jacob" :: String))

updateTest :: Connection -> IO Bool
updateTest conn = do
    n <- execute conn "UPDATE test SET name = ? WHERE id = ?" $ (("Tomas" :: String), (1 :: Int))
    return $ n > 0

retrieveTest :: Connection -> IO  [TestField]
retrieveTest conn = query conn "SELECT id, name FROM test WHERE id = ?" $ (Only (1 :: Int))

deleteTest :: Connection -> IO Bool
deleteTest conn = do
    n <- execute conn "DELETE FROM test WHERE id = ?" $ (Only (1 :: Int))
    return $ n > 0

