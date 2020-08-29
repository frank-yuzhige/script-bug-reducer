{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Make.Reducer (
  Reducer (..),
  initReducer,
  execReducer,
  genNewMakefile,
  reduceFeedback,
  terminateReduce,
) where

import Make.Make

import qualified Data.Map      as M
import qualified Data.Text     as T
import           Data.Makefile
import           Data.Map  (Map)
import           Data.Text (Text)
import           Data.List ((\\), nub)
import           Control.Monad
import           Control.Monad.Trans.Except


data Reducer = Reducer {
  getMake              :: Makefile,
  getAllTargets        :: [Target],
  getSuspiciousTargets :: [Target],
  getConfirmedTargets  :: [Target],
  getCCVar             :: Text,
  getCC                :: Text,
  getXCCVar            :: Text,
  getXCC               :: Text
} deriving (Eq, Show)

initReducer :: Text -> Text -> Text -> Makefile -> Reducer
initReducer ccvar xccvar xcc m@(Makefile entries) = Reducer m ts ts [] ccvar cc xccvar xcc
  where
    ts = nub [t 
             | e <- entries
             , case e of Rule {} -> True; _ -> False
             , let Rule t _ cmds = e
             , any (cmdContainsVar ccvar) cmds
             ]
    cc = "gcc"

execReducer :: (Makefile -> IO Bool) -> Reducer -> IO (Either String Reducer)
execReducer testMakefile reducer = do
  let next = genNewMakefile reducer
  case next of
    Left  err        -> return $ Left err
    Right (mf, inss) -> reduceFeedback reducer inss <$> testMakefile mf

genNewMakefile :: Reducer -> Either String (Makefile, [Target])
genNewMakefile reducer = (, inss) <$> buildMake make mods
  where
    make   = getMake reducer
    alls   = getAllTargets reducer
    suss   = getSuspiciousTargets reducer
    inss   = [ t | (t, c) <- suss `zip` cycle [True, False], c ]
    cvar   = getCCVar  reducer
    xccvar = getXCCVar reducer
    comp   = getCC     reducer
    xcc    = getXCC    reducer
    mods   = (AddAssignment xccvar xcc) : [ ReplaceVarsInTarget t cvar xccvar | t <- inss ]

reduceFeedback :: Reducer -> [Target] -> Bool -> Either String Reducer
reduceFeedback reducer inss isPass
  | null newSuss = Left "error: cannot locate errorneous target"
  | otherwise    = Right newReducer
  where
    suss       = getSuspiciousTargets reducer
    newSuss    = if isPass then inss else suss \\ inss
    newReducer = reducer { getSuspiciousTargets = newSuss }

terminateReduce :: Reducer -> Maybe Target
terminateReduce reducer = case getSuspiciousTargets reducer of
  [x] -> Just x
  _   -> Nothing

cmdContainsVar :: Text -> Command -> Bool
cmdContainsVar var (Command cmd) = any (== makeVar var) (T.words cmd) 