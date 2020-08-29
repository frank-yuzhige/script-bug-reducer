{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Make.Make (
  MakeMod (..),
  buildMake,
  makeVar,
) where

import Data.Makefile

import qualified Data.Text as T
import           Data.Text (Text)
import           Control.Monad
import           Text.Printf

type CommandIx = (Target, Int)

data MakeMod = AddAssignment        Text      Text
             | ReplaceVarsInTarget  Target    Text Text
             | RewriteAllInCommand  CommandIx Text Text  

buildMake :: Makefile -> [MakeMod] -> Either String Makefile
buildMake = foldM applyModToMake

applyModToMake :: Makefile -> MakeMod -> Either String Makefile
applyModToMake makefile = \case
  AddAssignment       v  s     -> addAssignment       makefile v s
  ReplaceVarsInTarget t  v1 v2 -> replaceVarsInTarget makefile t v1 v2 
  RewriteAllInCommand ci o  n  -> rewriteAllInCommand makefile ci o n

addAssignment :: Makefile -> T.Text -> T.Text -> Either String Makefile
addAssignment (Makefile entries) var val
  | entries `containsVar` var = Left $ printf "Variable %s is already defined in Makefile" (makeVar var)
  | otherwise                 = Right (Makefile (newAssignment : entries))
  where
    newAssignment = Assignment RecursiveAssign var val
    
replaceVarsInTarget :: Makefile -> Target -> T.Text -> T.Text -> Either String Makefile
replaceVarsInTarget (Makefile entries) target v1 v2
  | entries `containsTarget` target = Right $ Makefile $ modifyFirst entries (findTarget target) changeTarget 
  | otherwise                       = Left "Error! Could not find target"
  where
    changeTarget (Rule t ds cmds) = Rule t ds cmds'
      where
        cmds' = [ Command $ T.unwords [ if w == makeVar v1 then makeVar v2 else w | w <- ws ] 
                | (Command cmd) <- cmds
                , let ws = T.words cmd 
                ]
        -- TODO: need more fine-grained bash variable replacement 

rewriteAllInCommand :: Makefile -> CommandIx -> Text -> Text -> Either String Makefile
rewriteAllInCommand = undefined
  

containsVar :: [Entry] -> T.Text -> Bool
containsVar entries var = var `elem` vs 
  where 
    vs = [ v 
         | e <- entries
         , case e of Assignment {} -> True; _ -> False
         , let Assignment _ v _ = e 
         ]

containsTarget :: [Entry] -> Target -> Bool
containsTarget entries target = target `elem` ts 
  where 
    ts = [ t 
         | e <- entries
         , case e of Rule {} -> True; _ -> False
         , let Rule t _ _ = e 
         ]

modifyFirst :: [a] -> (a -> Bool) -> (a -> a) -> [a]
modifyFirst xs p f = ps ++ (f q : qs)
  where
    (ps, (q: qs)) = break p xs

findTarget :: Target -> Entry -> Bool
findTarget target = \case 
  Rule t _ _ | t == target -> True
  _                        -> False

makeVar :: T.Text -> T.Text
makeVar var = T.pack $ printf "$(%s)" var