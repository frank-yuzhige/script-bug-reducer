module Reduce.Reducer where

import Data.List
import Reduce.Bash
import Language.Bash.Syntax

data Reducer = Reducer {
  getScript  :: List,
  getCandIxs :: [CmdIx],
  getSuspIxs :: [CmdIx],
  getTrans   :: ShellCommand -> ShellCommand
}

initReducer :: (ShellCommand -> Bool) -> (ShellCommand -> ShellCommand) -> List -> Reducer
initReducer p f l = Reducer l xs xs f
  where
    xs = getIxsList p l

genNewBash :: Reducer -> Either String (List, [CmdIx])
genNewBash r = return $ (foldr (flip modifyIxList f) script inss, inss)
  where
    script = getScript  r
    cs     = getCandIxs r
    ss     = getSuspIxs r
    f      = getTrans   r
    inss   = [ t | (t, c) <- ss `zip` cycle [True, False], c ]

reduceFeedback :: Reducer -> [CmdIx] -> Bool -> Either String Reducer
reduceFeedback reducer inss isPass
  | null newSuss = Left "error: cannot locate errorneous target"
  | otherwise    = Right newReducer
  where
    suss       = getSuspIxs reducer
    newSuss    = if isPass then inss else suss \\ inss
    newReducer = reducer { getSuspIxs = newSuss }

terminateReduce :: Reducer -> Maybe CmdIx
terminateReduce reducer = case getSuspIxs reducer of
  [x] -> Just x
  _   -> Nothing
