module GetConstants(getConstants) where

-----------------------------------------------------------------

import qualified Ast as Front
import qualified MurphiAST as B
import TransGen

-----------------------------------------------------------------
-----------------------------------------------------------------

-- final function to retrive the constants section
-- Constants = machineSizesC :: [(MachineType, Size)]
--             vcs           :: [VCName]
getConstants :: Front.Ast -> B.Constants
getConstants frontAST = B.Constants machinesSizes vcs
  where
    machines      = getMachineNames frontAST
    sizes         = getMachineSizes frontAST
    machinesSizes = zip machines sizes
    ---------------------------------------
    vcs = getVCNames frontAST

-----------------------------------------------------------------
-----------------------------------------------------------------
