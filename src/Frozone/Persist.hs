module Frozone.Persist where

import qualified Frozone.Model as Model

type Model = Model.UserManagementData


startPersistence :: IO ()
startPersistence = return ()



class Persistable a where
    read :: IO a
    write :: a -> IO ()

instance Persistable Model.UserManagementData where
    --read :: IO Model
    read = putStrLn "not yet implemented!" >> return Model.emptyFrozone

    --write :: Model -> IO ()
    write _ = putStrLn "not yet implemented!"

{-
data Persist
    = Persist
    { 
    }
-}
