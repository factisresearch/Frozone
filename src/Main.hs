module Main where

import Frozone.Types
import Frozone.Server

import Safe
import System.Environment

main :: IO ()
main =
    do putStrLn "Welcome to Frozone"
       args <- getArgs
       case args of
         (sqliteFile : storageDir : httpPort : []) ->
             case readMay httpPort of
               Just port ->
                   let cfg =
                           FrozoneConfig
                           { fc_sqliteFile = sqliteFile
                           , fc_storageDir = storageDir
                           , fc_httpPort = port
                           }
                   in runServer cfg
               Nothing ->
                   do putStrLn "Invalid httpPort number!"
                      usageP
         _ ->
             usageP
    where
      usageP =
          putStrLn "Usage: ./Frozone [sqliteFile] [storageDir] [httpPort]"
