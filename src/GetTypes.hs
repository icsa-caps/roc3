module GetTypes(getTypes) where

-----------------------------------------------------------------

import TransGen
import TransMsg
import qualified Ast as F
import qualified MurphiAST as B
import Data.List  -- for nub

-----------------------------------------------------------------
-----------------------------------------------------------------

-- final functions that transforms one AST to the other
--data Types  = Types
--             {
--                machinesSym :: [(MachineType, Size, Symmetry)],
--                msgType         :: [String],
--                msgArgs         :: MsgArgs,
--                machineStates   :: [ (MachineType,
--                                     [State],       -- states of each machine
--                                     [TypeDecl]) ]  -- fields of the machine
--             }


getTypes :: F.Ast -> B.Types
getTypes fAst = B.Types machinesSym msgType msgArgs machineStates
  where

    msgType = getMTypes fAst
    msgArgs = stdMsgArgs fAst

    machines    = getMachineNames fAst
    sizes       = getMachineSizes fAst
    sym         = getSymmetries fAst
    machinesSym = zip3 machines sizes sym

    fields = getMachineFields fAst
    states = getMStates fAst
    machineStates = zip3 machines states fields




getMStates :: F.Ast -> [[B.State]]
getMStates fAst = let machines = F.machines fAst
                  in  map mstatesSingle machines
  where
    -- get the states of a single machine, no duplicates
    mstatesSingle :: F.Machine -> [B.State]
    mstatesSingle machine = let mfunction   = F.machineFunction machine
                                machineName = F.machineType machine
                                fstates   = map (\(s,_,_,_) -> s)
                                                mfunction
                            in  nub $ map (transState machineName) fstates


-----------------------------------------------------------------
-----------------------------------------------------------------
