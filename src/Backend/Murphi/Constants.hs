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
import GenHelper

-- helper tomurphi implementations
import TomurphiHelper

-----------------------------------------------------------------
-----------------------------------------------------------------



instance Cl.MurphiClass Constants where

 tomurphi (Constants machineSizes vcs) = "const\n\n" ++
                                         "-- Machine Sizes\n" ++ machinesSizes
                                        ++ "\n\n"
                                        ++ "-- Network parameters\n" ++ netParams
                                        ++ "NET_MAX: " ++ show netMax ++ ";"
  where

    onlyMachines   = map fst machineSizes
    onlySizes      = map snd machineSizes
    sizeNames      = map (machineSizeStr) onlyMachines
    machinesSizes  = let decls = map declGen (zip sizeNames  onlySizes)
                     in concatWith "" decls -- each decl ends with '\n'

    ---- <machineName>Size : <num>;
    --------------------------------

    vcVal     = zip (vcs) [0,1..]

    vcDecl    = mapconcatln declGen vcVal

    numChs    = length vcs

    netParams = vcDecl ++ "\n" ++
                "NUM_VCs : " ++ show numChs ++ ";\n"


    netMax = length machineSizes -- just #machines

   ---- VC_<VCName> : <num>;
   ---- NUM_VCs : <num>;

-----------------------------------------------------------------
