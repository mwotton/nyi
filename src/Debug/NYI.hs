-- | When working, we often want to leave something for later while we implement other
--   parts. Usually `error` or `undefined` are used for these parts, but these have
--   problems: if we don't have warnings on, they might sail through unnoticed. If
--   we do, on the other hand, we might not be able to build images that require
--   a clean build.
--
--   Instead, this module exposes a function for collecting all the usages of `nyi`
--   so a test can verify that there are none left. This allows both exploratory
--   hacking and validates that nothing slips through the test suite.
--
--   Some code derived from the placeholders package.

{-# LANGUAGE TemplateHaskell #-}
module Debug.NYI(nyi,todo,allNotImplemented,PlaceholderException(..)) where

import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import Language.Haskell.TH (Q, Exp, Loc(..), litE, stringL, location, report, runIO)
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)
import Debug.Trace
-- import Data.Map.Strict(insert)

-- | Thrown when attempting to evaluate a placeholder at runtime.
data PlaceholderException = PlaceholderException String
    deriving (Show, Typeable)

instance Exception PlaceholderException

allNotImplemented :: IO [(String, Loc)]
allNotImplemented = readIORef notImplementedCache

{-# NOINLINE notImplementedCache #-}
notImplementedCache :: IORef [(String,Loc)]
notImplementedCache = unsafePerformIO $ newIORef mempty

{-# NOINLINE nyi #-}
nyi = placeholderNoWarning "Not yet implemented"
{-# NOINLINE todo #-}
todo = placeholderNoWarning

placeholderNoWarning :: String -> Q Exp
placeholderNoWarning msg = do
  loc <- location
  traceM $ show ("called", loc, msg)
  _ <- runIO $ atomicModifyIORef notImplementedCache (\current -> ((msg,loc):current,()))
  let runtimeMsg = formatMessage msg loc
  [| do

      throw $ PlaceholderException $(litE $ stringL runtimeMsg) |]

-- below is some code sourced from https://hackage.haskell.org/package/placeholders-0.1
formatMessage :: String -> Loc -> String
formatMessage msg loc = msg ++ " at " ++ formatLoc loc

formatLoc :: Loc -> String
formatLoc loc = let file = loc_filename loc
                    (line, col) = loc_start loc
                in concat [file, ":", show line, ":", show col]
