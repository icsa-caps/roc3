module TransMurphi(transform) where

-----------------------------------------------------------------

import qualified Ast as F
import qualified MurphiAST as B

import GetConstants
import GetTypes
import GetVariables
import GetCommonFunctions
import GetMachineFunctions
import GetRules
import GetStartstate
import GetInvariants

-----------------------------------------------------------------
-----------------------------------------------------------------

-- final functions that transforms one AST to the other
transform :: F.Ast -> B.Program
transform fAst = let
                     constants   = getConstants        fAst
                     types       = getTypes            fAst
                     variables   = getVariables        fAst
                     commonFunc  = getCommonFunctions  fAst
                     machineFunc = getMachineFunctions fAst
                     rules       = getRules            fAst
                     startstate  = getStartstate       fAst
                     invariants  = getInvariants       fAst
                 in
                     B.Program constants
                               types
                               variables
                               commonFunc
                               machineFunc
                               rules
                               startstate
                               invariants


-----------------------------------------------------------------
-----------------------------------------------------------------
