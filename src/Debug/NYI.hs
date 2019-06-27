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
--   It would be preferable to be allowed to use inline `nyi`, but I haven't found
--   a way to extract a Dec from an Exp and have it be floated up.
--
--   Some code derived from the placeholders package, much more derived from
--   https://gitlab.haskell.org/ghc/ghc/wikis/template-haskell/annotations

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Debug.NYI(nyi,todo,allNotImplemented,Loc(..)) where

import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax(Dec,lift,mkName)
import Language.Haskell.TH.Instances()
import Language.Haskell.TH (varP,  AnnLookup(AnnLookupModule), AnnTarget(ModuleAnnotation), ExpQ, Loc(..), ModuleInfo(..), Q
                           , litE, location, pragAnnD, reifyAnnotations, reifyModule, stringL, thisModule)
import qualified Data.Set as Set

type NYI = (String, Loc)

nyi :: String -> Q [Dec]
nyi fn = placeholderNoWarning fn "Not yet implemented"

todo :: String -> String -> Q [Dec]
todo = placeholderNoWarning

allNotImplemented :: ExpQ
allNotImplemented = do
  anns <- traverseAnnotations
  [| anns |]

traverseAnnotations :: Q [NYI]
traverseAnnotations = do
  ModuleInfo children <- reifyModule =<< thisModule
  go children Set.empty []
  where
    go []     _visited acc = return acc
    go (x:xs) visited  acc
      | x `Set.member` visited = go xs visited acc
      | otherwise = do
          ModuleInfo newMods <- reifyModule x
          newAnns <- reifyAnnotations $ AnnLookupModule x
          go (newMods ++ xs) (x `Set.insert` visited) (newAnns ++ acc)

-- | Thrown when attempting to evaluate a placeholder at runtime.
data PlaceholderException = PlaceholderException String
    deriving stock (Show, Typeable)
    deriving anyclass Exception

placeholderNoWarning :: String -> String -> Q [Dec]
placeholderNoWarning funcName msg = do
  loc <- location
  anno <- (:[]) <$> pragAnnD ModuleAnnotation (lift (msg, loc))
  let runtimeMsg = msg ++ " at " ++ formatLoc loc
  let name = mkName funcName
  -- TODO: work out how to get a signature in here too.
  thrower <- [d|$(varP name) = throw $ PlaceholderException $(litE $ stringL runtimeMsg) |]
  pure (thrower <> anno)

  where
    formatLoc :: Loc -> String
    formatLoc loc =
      let file = loc_filename loc
          (line, col) = loc_start loc
      in concat [file, ":", show line, ":", show col]
