-----------------------------------------------------------------
------------ tomurphi implementation for variables --------------
-- the variables are machines (arrays of machine states)
-- ordered networks (arrays of messages)
-- unordered networks (multisets)
-----------------------------------------------------------------

module Variables where

-----------------------------------------------------------------

import  MurphiAST
import qualified MurphiClass as Cl
import Data.Char

-- general helper functions
import GenHelper

-- helper tomurphi implementations
import TomurphiHelper

-----------------------------------------------------------------
-----------------------------------------------------------------

instance Cl.MurphiClass Variables where

 tomurphi variables = "-- machines\n"       ++ finalMachines ++ "\n" ++
                      "-- ordered Nets\n"   ++ finalOrd      ++ "\n" ++
                      "-- unordered Nets\n" ++ finalUnord    ++ "\n"
  where
   machineNames = machines variables
   formatMachine :: MachineName -> String
   formatMachine machine = toMachineArrayStr machine ++
                          ": array [" ++ indexTypeStr machine ++ "]"
                           ++ " of " ++ toMachineStateStr machine ++ ";"
   finalMachines = mapconcatln formatMachine machineNames

   -----------------------------

   ordNets = orderedNets variables
   formatOrd :: OrderedNetName -> String
   formatOrd net = net ++ ": array [Node] of array [0..NET_MAX-1] of Message;"

   counts :: OrderedNetName -> String
   counts net = net ++ "count: array [Node] of 0..NET_MAX;"

   combineArrayCount :: OrderedNetName -> String
   combineArrayCount net = formatOrd net ++ "\n" ++ counts net

   finalOrd = mapconcatln combineArrayCount ordNets

   -----------------------------

   unordNets = unorderedNets variables
   formatUnord :: UnorderedNetName -> String
   formatUnord net = net ++ ": array [Node] of multiset [NET_MAX] of Message;"

   finalUnord = mapconcatln formatUnord unordNets


----------------------------------------------------------------
----------------------------------------------------------------
