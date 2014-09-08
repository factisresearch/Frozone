module Frozone.Util.Testing where

import Frozone.Util.ErrorHandling
import Control.Monad.Error


assertSUCCESS :: ErrorT String IO a -> IO a
assertSUCCESS errX =
    do eitherX <- runErrorT errX
       handleEither eitherX fail return

assertERROR :: ErrorT String IO a -> IO ()
assertERROR errX =
    do eitherX <- runErrorT errX
       handleEither eitherX (const $ return ()) (fail "error expected!")
