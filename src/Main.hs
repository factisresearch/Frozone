module Main where

import Frozone.Server

import System.Environment
import qualified Data.Yaml as YML

main :: IO ()
main =
    do putStrLn "Welcome to Frozone"
       args <- getArgs
       case args of
         (cfgFile : []) ->
             do ymlResp <- YML.decodeFileEither cfgFile
                case ymlResp of
                  Left err ->
                      do print err
                         usageP
                  Right cfg ->
                      runServer cfg
         _ ->
             usageP
    where
      usageP =
          putStrLn "Usage: ./Frozone [cfgFile]"
