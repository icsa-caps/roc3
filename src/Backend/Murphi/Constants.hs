-----------------------------------------------------------------
------------ tomurphi implementation for constants --------------
-----------------------------------------------------------------

module Constants where

-----------------------------------------------------------------

import  MurphiAST
import qualified MurphiClass as Cl
import Data.Char
import Data.List.Split -- for tokenizing strings
                       -- splitOn is used in pushBy

-- general helper functions
import MurphiGenHelper

-- helper tomurphi implementations
import tomurphiHelper

-----------------------------------------------------------------
-----------------------------------------------------------------



instance Cl.MurphiClass Constants where

 --tomurphi :: Constants -> String
 tomurphi (Constants machineSizes vcs) = "const\nMachine Sizes\n" ++ machinesSizes
                                        ++ "\n\n"
                                        ++ "Network parameters\n" ++ netParams
                                        ++ "NET_MAX: " ++ show netMax
  where

    onlyMachines     = map fst machineSizes
    onlySizes        = map snd machineSizes
    machineSize      = map (++"Size") onlyMachines
    machinesSizes    = mapconcatln declGen (zip machineSize onlySizes)

    ---- <machineName>Size : <num>;
    --------------------------------

    vcVal     = zip (map ("VC_" ++) vcs) [0,1..]

    vcDecl    = mapconcatln declGen vcVal

    numChs    = length vcs

    netParams = vcDecl ++ "\n" ++
                "NUM_VCs : " ++ show numChs ++ ";\n"


    netMax = length machineSizes -- just #machines

   ---- VC_<VCName> : <num>;
   ---- NUM_VCs : <num>;

-----------------------------------------------------------------