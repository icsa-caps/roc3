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
   formatMachine machine = toMachineArrayStr machine ++ ": array [" ++ indexNameStr machine ++ "]"
                           ++ " of " ++ toMachineStateStr machine ++ ";"
   finalMachines = mapconcatln formatMachine machineNames

   -----------------------------

   ordNets = orderedNets variables
   formatOrd :: String -> String
   formatOrd net = net ++ ": array [Node] of array [0..NET_MAX-1] of Message;"

   counts :: String -> String
   counts net = net ++ "count: array [Node] of 0..NET_MAX;"

   combineArrayCount :: String -> String
   combineArrayCount net = formatOrd net ++ "\n" ++ counts net

   finalOrd = let  ordNetNames = map netName $ map (Left) ordNets
              in   mapconcatln combineArrayCount ordNetNames

   -----------------------------

   unordNets = unorderedNets variables
   formatUnord :: String -> String
   formatUnord net = net ++ ": array [Node] of multiset [NET_MAX] of Message;"

   finalUnord = let unordNetNames = map netName $ map (Right) unordNets
                in  mapconcatln formatUnord unordNetNames


----------------------------------------------------------------
