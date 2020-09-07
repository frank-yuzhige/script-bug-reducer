module Reduce.Tagless where
  
import Reduce.Bash
import Language.Bash.Syntax

class VisitList v where
  list :: (VisitStmt a) => [a] -> v

class VisitStmt v where
  statement :: (VisitAndOr a, VisitListTerm lt) => a -> lt -> v

class VisitAndOr v where
  and  :: (VisitPipe a, VisitAndOr b) => a -> b -> v
  or   :: (VisitPipe a, VisitAndOr b) => a -> b -> v
  last :: (VisitPipe a)               => a -> v

class VisitPipe v where
  pipeline :: (VisitCmd c) => Bool -> Bool -> Bool -> c -> v

class VisitCmd v where
  command :: VisitShellCmd s => s -> Redir -> v

class VisitShellCmd v where
  simple        :: [Assign] -> [Word] -> v
  assignBuiltin :: Word -> [Either Assign Word] -> v
  funDef        :: VisitList l => String -> l -> v
  coproc        :: VisitCmd c => String -> c -> v
  subshell      :: VisitList l => l -> v
  group         :: VisitList l => l -> v
  arith         :: String -> v
  scIf          :: VisitList l => l -> l -> Maybe l -> v
  scUntil       :: VisitList l => l -> l -> v
  scWhile       :: VisitList l => l -> l -> v


class VisitListTerm v where
   