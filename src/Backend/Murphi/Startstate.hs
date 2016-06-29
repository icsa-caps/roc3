-----------------------------------------------------------------
---------- tomurphi implementation for the startstate -----------
-----------------------------------------------------------------

module Startstate where

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


instance Cl.MurphiClass Startstate where
 tomurphi (Startstate machines orderedNets unorderedNets)
        = "-- initialise machines\n"           ++
          machinesInit      ++ "\n"             ++
          "-- initialise unordered networks\n" ++
          unorderedNetInit ++ "\n"             ++
          "-- initialise ordered networks\n"   ++
          orderedNetInit   ++ "\n"

  where
   -----------------------------
   -----------------------------
   -- initialise all machines
   machinesInit = mapconcatln initMachine machines



   -----------------------------
   -- initialise all fields of all ocurences of a machine
   initMachine :: ( MachineType, State, [FieldStart] ) -> String
   initMachine (machine, startstate, fields)
     = "-- " ++ machine ++ " initialisation\n" ++
       "for " ++ formalIndexStr machine ++ ":" ++
       indexTypeStr machine ++ " do\n" ++
       "  " ++ initState machine startstate ++ "\n" ++
       pushBy 2 (initFields machine fields) ++ "\n" ++
       "endfor;\n"

   -----------------------------

   -- machine fields

   -- print the initialisation of a field of a particular machine
   initField :: MachineType -> FieldStart -> String
   initField machine (field, val)
     = let thisField =  indexedFormalStr machine ++ "." ++ field
       in case val of (Just value) ->  thisField ++ " := " ++ value ++ ";"
                      (Nothing)    -> "undefine " ++ thisField ++ ";"

   -- print the initialisation of a list of fields of a machine
   initFields :: MachineType -> [FieldStart] -> String
   initFields machine = (mapconcatln . initField) machine

   -----------------------------

   -- initialise a machine state
   initState :: MachineType -> State -> String
   initState machine state = indexedFormalStr machine ++
                             ".state := " ++ state ++ ";\n"

   -----------------------------
   -----------------------------

   -- unordered networks

   -- we simply have to undefine all unordered nets
   unorderedNetInit = mapconcatln (\net -> "undefine " ++ net ++ ";")
                                  unorderedNets

   -----------------------------
   -----------------------------

   -- ordered nets

   -- intialise all unordered nets
   -- undefine the networks and set the counts to 0
   orderedNetInit = undefineOrderedNets orderedNets ++ "\n\n" ++
                    allZeroCounts orderedNets ++ "\n"


   -----------------------------

   -- undefine all ordered nets
   undefineOrderedNets :: [OrderedNetName] -> String
   undefineOrderedNets = mapconcatln (\net -> "undefine " ++ net ++ ";")


   -- set count of an ordered net to 0
   zeroCount :: OrderedNetName -> String
   zeroCount net = "for n:Node do\n" ++
                    net ++ "count[n] := 0;\n" ++
                    "endfor;\n"

   -----------------------------

   -- set the counts of all ordered networks to 0
   allZeroCounts :: [OrderedNetName] -> String
   allZeroCounts = mapconcatln zeroCount

   -----------------------------
   -----------------------------

 -----------------------------------------------------------------
