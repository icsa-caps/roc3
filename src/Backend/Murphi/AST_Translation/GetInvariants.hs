module GetInvariants where

-----------------------------------------------------------------

import qualified Ast as F
import qualified MurphiAST as B
import TransGen

-----------------------------------------------------------------
-----------------------------------------------------------------


getInvariants :: F.Ast -> B.Invariants
getInvariants fAst = B.Invariants ("The invariants part of both ASTs"++
                                  " is not complete yet.")





-----------------------------------------------------------------
-----------------------------------------------------------------
