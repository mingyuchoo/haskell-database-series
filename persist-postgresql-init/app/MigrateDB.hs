module Main
    where

import           Database           (localConnString, migrateDB)

import qualified DatabaseEsq        as E

import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args || head args /= "esq"
    then migrateDB localConnString
    else E.migrateDB localConnString
