module GetInvariants where

-----------------------------------------------------------------

import qualified Ast as Front
import qualified Backend.Murphi.MurphiAST as Back
import TransGen

-----------------------------------------------------------------
-----------------------------------------------------------------


getInvariants :: F.Ast -> B.Invariants
getInvariants fAst = B.Invariants ("The invariants part of both ASTs"++
                                  " is not complete yet.")





-----------------------------------------------------------------
-----------------------------------------------------------------
