{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Reduce.Bash (
  CmdIx,
  replaceAllExec,
  modifyIxList,
  checkExec,
  getIxsList,
  showByIxList,
  packBashWord,
  replaceExec
) where


import Data.Maybe
import Language.Bash.Syntax
import Language.Bash.Pretty
import Language.Bash.Word hiding (Word)
import qualified Language.Bash.Word as B (Word, Span)  

data SubElement = PlainText String | Variable String 
                deriving (Eq, Show)

type CmdIx = [Int]

-- toBash :: SubElement -> B

packBashWord :: String -> B.Word
packBashWord = map Char

replaceAllExec :: String -> String -> List -> List
replaceAllExec ex1 ex2 (List stmts) = List [ Statement (f andor) term | (Statement andor term) <- stmts ]
  where
    f = mapPipeline (changeCmdInPipeline (fmap (changeShellCommand (replaceExec (packBashWord ex1) (packBashWord ex2))))) 

replaceExecAt :: CmdIx -> B.Word -> B.Word -> List -> List
replaceExecAt ix = (modifyIxList ix .) . replaceExec

mapList :: (Statement -> Statement) -> List -> List
mapList f (List stmts) = List $ map f stmts

showByIxList :: CmdIx -> List -> String
showByIxList [] l = prettyText l
showByIxList (si: ix) (List stmts) = showByIxStmt ix (stmts !! si) 

showByIxStmt :: CmdIx -> Statement -> String
showByIxStmt [] s                          = prettyText s
showByIxStmt (pi: ix) (Statement andor lt) = showByIxPipe ix $ getPipe (andor !!* pi)

showByIxPipe :: CmdIx -> Pipeline -> String
showByIxPipe [] s = prettyText s
showByIxPipe (si: ix) (Pipeline t tp i cmds) = showByIxCmd ix $ (cmds !! si)

showByIxCmd :: CmdIx -> Command -> String
showByIxCmd [] s = prettyText s
showByIxCmd (i: ns) (Command sc rs) = case sc of
  FunctionDef str list  | i == 0 -> showByIxList ns list
  ArithFor    str list  | i == 0 -> showByIxList ns list
  Coproc      str cmd   | i == 0 -> showByIxCmd  ns cmd
  Subshell    list      | i == 0 -> showByIxList ns list
  Group       list      | i == 0 -> showByIxList ns list 
  If          l0 l1 ml2 | i == 0 -> showByIxList ns l0
                        | i == 1 -> showByIxList ns l1
                        | i == 2 -> case ml2 of Just l2 -> showByIxList ns l2
  Until       l0 l1     | i == 0 -> showByIxList ns l0  
                        | i == 1 -> showByIxList ns l1  
  While       l0 l1     | i == 0 -> showByIxList ns l0
                        | i == 1 -> showByIxList ns l1
  _                              -> error "Invalid cmd id"

modifyIxList :: CmdIx -> (ShellCommand -> ShellCommand) -> List -> List
modifyIxList (si: ix) f (List stmts) = List [ if i == si then modifyIxStmt ix f s else s | (s, i) <- stmts `zip` [0..]] 

modifyIxStmt :: CmdIx -> (ShellCommand -> ShellCommand) -> Statement -> Statement
modifyIxStmt (pi: ix) f (Statement andor lt) = Statement (go pi andor) lt
  where
    go 0  a         = pipeApply (modifyIxPipe ix f) a
    go si (And p n) = And p $ go (si - 1) n
    go si (Or  p n) = Or  p $ go (si - 1) n

modifyIxPipe :: CmdIx -> (ShellCommand -> ShellCommand) -> Pipeline -> Pipeline
modifyIxPipe (ci: mn) f (Pipeline t tp i cmds) = Pipeline t tp i cmds'
  where
    cmds' = [ if i == ci then modifyIxCmd mn f c else c | (c, i) <- cmds `zip` [0..] ] 

modifyIxCmd :: CmdIx -> (ShellCommand -> ShellCommand) -> Command -> Command
modifyIxCmd mn f c@(Command sc rs) = case mn of
    []       -> Command (f sc) rs
    (li: ns) -> Command (continue li ns f sc) rs 
  where
    continue i next f sc = case sc of 
      FunctionDef str list  | i == 0 -> FunctionDef str (goList list)
      ArithFor    str list  | i == 0 -> ArithFor    str (goList list)  
      Coproc      str cmd   | i == 0 -> Coproc      str (modifyIxCmd next f cmd)
      Subshell    list      | i == 0 -> Subshell    (goList list)  
      Group       list      | i == 0 -> Group       (goList list)  
      If          l0 l1 ml2 | i == 0 -> If          (goList l0) l1          ml2
                            | i == 1 -> If          l0          (goList l1) ml2
                            | i == 2 -> If          l0          l1          (goList <$> ml2)
      Until       l0 l1     | i == 0 -> Until       (goList l0) l1   
                            | i == 1 -> Until       l0          (goList l1)   
      While       l0 l1     | i == 0 -> While       (goList l0) l1   
                            | i == 1 -> While       l0          (goList l1)   
      -- TODO: finish all 
      _ -> error "Invalid cmdix"
      where
        goList = modifyIxList next f 


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

replaceExecByIxs :: CmdIx -> B.Word -> B.Word -> List -> List
replaceExecByIxs ixs orig new list = modifyIxList ixs (replaceExec orig new) list

checkExec :: String -> ShellCommand -> Bool
checkExec e (SimpleCommand _ (exec: _))
  | exec == packBashWord e = True
checkExec _ _ = False

getIxsList :: (ShellCommand -> Bool) -> List -> [CmdIx]
getIxsList p (List stmts) = concat [ map (i :) $ getIxsStmt p s | (s, i) <- stmts `zip` [0..]]

getIxsStmt :: (ShellCommand -> Bool) -> Statement -> [CmdIx]
getIxsStmt p (Statement andor _) = concat $ go 0 andor
  where
    go i (And pp n) = (map (i :) $ getIxsPipe p pp) : go (i + 1) n
    go i (Or  pp n) = (map (i :) $ getIxsPipe p pp) : go (i + 1) n
    go i (Last  pp) = [map (i :) $ getIxsPipe p pp]

getIxsPipe :: (ShellCommand -> Bool) -> Pipeline -> [CmdIx]
getIxsPipe p (Pipeline t tp i cmds) = concat [ map (i :) (getIxsCmd p c) | (c, i) <- cmds `zip` [0..]]

getIxsCmd :: (ShellCommand -> Bool) -> Command -> [CmdIx]
getIxsCmd p (Command sc rs) = check $ case sc of
  FunctionDef str list  -> map (0 :) (getIxsList p list)
  ArithFor    str list  -> map (0 :) (getIxsList p list)
  Coproc      str cmd   -> map (0 :) (getIxsCmd  p cmd)
  Subshell    list      -> map (0 :) (getIxsList p list)
  Group       list      -> map (0 :) (getIxsList p list)
  If          l0 l1 ml2 -> concat [ map (i :) (getIxsList p l) | (l, i) <- ([l0, l1] ++ maybeToList ml2) `zip` [0..] ]
  Until       l0 l1     -> concat [ map (i :) (getIxsList p l) | (l, i) <- [l0, l1] `zip` [0..] ]
  While       l0 l1     -> concat [ map (i :) (getIxsList p l) | (l, i) <- [l0, l1] `zip` [0..] ]  
  _                     -> []
  where
    check = if p sc then ([] :) else id
  

(!!*) :: AndOr -> Int -> AndOr
a !!* 0 = a
(And _ n) !!* i = n !!* (i - 1)
(Or  _ n) !!* i = n !!* (i - 1)

getPipe :: AndOr -> Pipeline
getPipe (And p _) = p
getPipe (Or  p _) = p
getPipe (Last  p) = p