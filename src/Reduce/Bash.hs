module Reduce.Bash (
  CmdIx,
  toCmdIx,
  replaceAllExec,
  packBashWord,
  replaceExecAt
) where


import Data.Maybe
import Language.Bash.Syntax
import Language.Bash.Word hiding (Word)
import qualified Language.Bash.Word as B (Word, Span)  

data SubElement = PlainText String | Variable String 
                deriving (Eq, Show)

data CmdIx = CmdIx Int Int Int (Maybe (Int, CmdIx))
           deriving (Eq, Show)

toCmdIx :: [Int] -> CmdIx
toCmdIx (a: b: c: next)
  | null next = ret Nothing
  | otherwise = ret (Just (n, toCmdIx ns))
  where
    ret      = CmdIx a b c
    (n : ns) = next

-- toBash :: SubElement -> B

packBashWord :: String -> B.Word
packBashWord = map Char

replaceAllExec :: B.Word -> B.Word -> List -> List
replaceAllExec ex1 ex2 (List stmts) = List [ Statement (f andor) term | (Statement andor term) <- stmts ]
  where
    f = mapPipeline (changeCmdInPipeline (fmap (changeShellCommand (replaceExec ex1 ex2)))) 

replaceExecAt :: CmdIx -> B.Word -> B.Word -> List -> List
replaceExecAt ix = (modifyIxList ix .) . replaceExec

mapList :: (Statement -> Statement) -> List -> List
mapList f (List stmts) = List $ map f stmts

modifyIxList :: CmdIx -> (ShellCommand -> ShellCommand) -> List -> List
modifyIxList ix@(CmdIx si _ _ _) f (List stmts) = List [ if i == si then modifyIxPipe ix f s else s | (s, i) <- stmts `zip` [0..]] 

modifyIxPipe :: CmdIx -> (ShellCommand -> ShellCommand) -> Statement -> Statement
modifyIxPipe ix@(CmdIx _ pi _ _) f (Statement andor lt) = Statement (go pi andor) lt
  where
    go 0  a         = pipeApply (modifyIxCmd ix f) a
    go si (And p n) = And p $ go (si - 1) n
    go si (Or  p n) = Or  p $ go (si - 1) n

modifyIxCmd :: CmdIx -> (ShellCommand -> ShellCommand) -> Pipeline -> Pipeline
modifyIxCmd ix@(CmdIx _ _ ci mn) f (Pipeline t tp i cmds) = Pipeline t tp i cmds'
  where
    cmds' = [ if i == ci then 
                case mn of
                  Nothing     -> Command (f sc) rs
                  Just (i, n) -> Command (continue i n f sc) rs 
              else 
                c 
            | (c@(Command sc rs), i) <- cmds `zip` [0..]
            ] 
    continue i next f sc = case sc of 
      FunctionDef str list | i == 0 -> FunctionDef str (modifyIxList next f list)

pipeNext :: AndOr -> AndOr
pipeNext (Or  _ p) = p
pipeNext (And _ p) = p

pipeApply :: (Pipeline -> Pipeline) -> AndOr -> AndOr
pipeApply f (Or  p n) = Or   (f p) n
pipeApply f (And p n) = And  (f p) n
pipeApply f (Last  p) = Last (f p)

mapPipeline :: (Pipeline -> Pipeline) -> AndOr -> AndOr
mapPipeline f (Last p)   = Last (f p)
mapPipeline f (Or  p ps) = Or   (f p) (mapPipeline f ps)
mapPipeline f (And p ps) = And  (f p) (mapPipeline f ps)

changeCmdInPipeline :: ([Command] -> [Command]) -> Pipeline -> Pipeline
changeCmdInPipeline f (Pipeline t tp i cmds) = Pipeline t tp i (f cmds)

changeShellCommand :: (ShellCommand -> ShellCommand) -> Command -> Command
changeShellCommand f (Command sc rs) = Command (f sc) rs

replaceExec :: B.Word -> B.Word -> ShellCommand -> ShellCommand
replaceExec orig new (SimpleCommand as (exec : args))
  | orig == exec = SimpleCommand as (new : args)
replaceExec _ _ cmd = cmd 

replaceExecByIxs :: [CmdIx] -> B.Word -> B.Word -> List -> List
replaceExecByIxs ixs orig new list = undefined