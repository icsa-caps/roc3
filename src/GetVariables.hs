module GetVariables(getVariables) where

------------------------------------------------------------------

import qualified Ast as F
import qualified Backend.Murphi.MurphiAST as B
import TransGen

------------------------------------------------------------------
------------------------------------------------------------------

-- final functions that transforms one AST to the other
--data Variables  = Variables {
--                             machines      :: [MachineType],
--                             orderedNets   :: [OrderedNet],
--                             unorderedNets :: [UnorderedNet]
--                            }

getVariables:: F.Ast -> B.Variables
getVariables frontAST = B.Variables machines orderedNets unorderedNets
  where
    machines      = getMachineNames frontAST
    orderedNets   = getOrdNetNames frontAST
    unorderedNets = getUnordNetNames frontAST

------------------------------------------------------------------
------------------------------------------------------------------
