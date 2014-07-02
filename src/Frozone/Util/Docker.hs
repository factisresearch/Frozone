{-# LANGUAGE OverloadedStrings #-}
module Frozone.Util.Docker where

import Data.Maybe
import qualified Data.Text as T

data Dockerfile
   = Dockerfile
   { d_from :: (DockerImage, Maybe DockerTag)
   , d_maintainer :: Maybe T.Text
   , d_cmds :: [DockerCommand]
   }
   deriving (Show, Eq)

type DockerImage = T.Text
type DockerTag = T.Text
type Executable = T.Text
type Param = T.Text

data DockerCmdExec
   = DockerCmdExec Executable [Param]
   | DockerCmdEntryParams [Param]
   | DockerCmdShell T.Text
    deriving (Show, Eq)

data DockerEntryPointDef
   = DockerEntryPointShell T.Text
   | DockerEntryPointExec Executable [Param]
    deriving (Show, Eq)

data DockerCommand
   = DockerAdd FilePath FilePath
   | DockerCopy FilePath FilePath
   | DockerRun T.Text
   | DockerCmd DockerCmdExec
   | DockerExpose [Int]
   | DockerEnv T.Text T.Text
   | DockerEntryPoint DockerEntryPointDef
   | DockerVolume FilePath
   | DockerUser T.Text
   | DockerWorkDir FilePath
   | DockerOnBuild DockerCommand
    deriving (Show, Eq)

serializeDockerfile :: Dockerfile -> T.Text
serializeDockerfile dockerfile =
    T.concat $ catMaybes [ Just $ serializeFrom (d_from dockerfile)
                         , fmap (\m -> T.concat [ dockerLine "MAINTAINER" [m], "\n" ]) (d_maintainer dockerfile)
                         , Just $ T.intercalate "\n" (map serializeCmd $ d_cmds dockerfile)
                         ]
    where
      dockerLine cmd args =
          T.intercalate " " (cmd : args)

      execArgs args =
          T.concat
          [ "["
          , T.intercalate ", " $ map (T.pack . show) args
          , "]"
          ]

      serializeFrom (img, mTag) =
          let fullImage = T.concat [img, fromMaybe "" $ fmap (\t -> T.concat [":", t]) mTag]
          in T.concat [ dockerLine "FROM" [fullImage], "\n" ]

      serializeCmd cmd =
          case cmd of
            DockerAdd src dest ->
                dockerLine "ADD" [T.pack src, T.pack dest]
            DockerCopy src dest ->
                dockerLine "COPY" [T.pack src, T.pack dest]
            DockerRun shell ->
                dockerLine "RUN" [shell]
            DockerCmd cmdSpec ->
                case cmdSpec of
                 DockerCmdExec prog args ->
                     dockerLine "CMD" [execArgs (prog:args)]
                 DockerCmdEntryParams params ->
                     dockerLine "CMD" [execArgs params]
                 DockerCmdShell shell ->
                     dockerLine "CMD" [shell]
            DockerExpose ports ->
                dockerLine "EXPOSE" (map (T.pack . show) ports)
            DockerEnv k v ->
                dockerLine "ENV" [k, v]
            DockerEntryPoint epd ->
                case epd of
                  DockerEntryPointShell shell ->
                      dockerLine "ENTRYPOINT" [shell]
                  DockerEntryPointExec prog args ->
                      dockerLine "ENTRYPOINT" [execArgs (prog:args)]
            DockerVolume vol ->
                dockerLine "VOLUME" [T.pack vol]
            DockerUser usr ->
                dockerLine "USER" [usr]
            DockerWorkDir wd ->
                dockerLine "WORKDIR" [T.pack wd]
            DockerOnBuild onbuild ->
                dockerLine "ONBUILD" [serializeCmd onbuild]
