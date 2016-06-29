module GetConstants(getConstants) where

-----------------------------------------------------------------

import qualified Ast as Front
import qualified Backend.Murphi.MurphiAST as Back

-----------------------------------------------------------------
-----------------------------------------------------------------

-- final function to retrive the constants section
-- Constants = machineSizesC :: [(MachineType, Size)]
--             vcs           :: [VCName]
getConstants :: Front.Ast -> Constants
getConstants frontAST = Constants machinesSizes vcs
  where
    machines      = getMachineNames frontAST
    sizes         = getMachineSizes frontAST
    machinesSizes = zip macines sizes
    ---------------------------------------
    vcs = getVCNames frontAST

-----------------------------------------------------------------
-----------------------------------------------------------------
