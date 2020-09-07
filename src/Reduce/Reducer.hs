module Reduce.Reducer where

import Data.List ( (\\) )
import Reduce.Bash
import Language.Bash.Syntax

data Reducer = Reducer {
  getScript  :: List,
  getCandIxs :: [CmdIx],
  getSuspIxs :: [CmdIx],
  getTrans   :: ShellCommand -> ShellCommand,
  getCaches  :: [CmdIx]
}

initReducer :: (ShellCommand -> Bool) -> (ShellCommand -> ShellCommand) -> List -> Reducer
initReducer p f l = Reducer l xs xs f []
  where
    xs = getIxsList p l

genNewBash :: Reducer -> Either String (List, [CmdIx])
genNewBash r = return (foldr gen script as, inss)
  where
    script = getScript  r
    as     = getCandIxs r
    cs     = getCaches  r
    ss     = getSuspIxs r
    f      = getTrans   r
    inss   = [ t | (t, c) <- ss `zip` cycle [True, False], c ]
    gen ix = flip modifyIxList f' ix
      where 
        f' c@(Command sc rs)
          | ix `elem` cs            = Command colon  []  -- If ix is cached and is not rediring, replace it with a colon
          | ix `elem` inss          = Command (f sc) rs  -- If ix is being inspecting, apply the transformation
          | otherwise               = c                  -- Otherwise, do nothing 

reduceFeedback :: Reducer -> [CmdIx] -> Bool -> Either String Reducer
reduceFeedback reducer inss isPass
  | null newSuss = Left "error: cannot locate errorneous target"
  | otherwise    = Right newReducer
  where
    suss       = getSuspIxs reducer
    newSuss    = if isPass then inss else suss \\ inss
    newCaches  = getCandIxs reducer \\ newSuss
    newReducer = reducer { getSuspIxs = newSuss, getCaches = newCaches }

terminateReduce :: Reducer -> Maybe CmdIx
terminateReduce reducer = case getSuspIxs reducer of
  [x] -> Just x
  _   -> Nothing
