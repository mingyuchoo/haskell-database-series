module Main
    where

import qualified BasicServer        as B

import qualified CacheServer        as C

import qualified ServerEsq          as E

import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Running Basic Server" >> B.runServer
    else if head args == "cache"
      then putStrLn "Running Cache Server" >> C.runServer
        else putStrLn "Running Esqueleto Server" >> E.runServer
