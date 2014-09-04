module Frozone.Util.Testing where

import Frozone.Util.ErrorHandling
import Control.Monad.Error


assertNoError :: ErrorT String IO a -> IO a
assertNoError errX =
    do eitherX <- runErrorT errX
       handleEither eitherX fail return

assertError :: ErrorT String IO a -> IO ()
assertError errX =
    do eitherX <- runErrorT errX
       handleEither eitherX (const $ return ()) (fail "error expected!")
