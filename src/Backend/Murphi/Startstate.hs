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

-- type Synonyms

type Value = String

-----------------------------------------------------------------
-----------------------------------------------------------------

instance Cl.MurphiClass Startstate where
 tomurphi (Startstate machines orderedNets unorderedNets)
        = let
              machinesInit
                = "-- initialise machines\n\n" ++
                  mapconcatln initMachine machines
                  ++ "\n-------------------------------\n\n"

              -- unordered networks
              -- we simply have to undefine all unordered nets
              unorderedNetInit = if unorderedNets == [] then ""
                                 else
                                      "-- initialise unordered networks\n" ++
                                      mapconcatln (\net ->
                                                     "undefine " ++ net ++ ";")
                                                  unorderedNets
                                      ++ "\n"
                                      ++ "\n-------------------------------\n\n"


              -- ordered nets
              -- intialise all unordered nets
              -- undefine the networks and set the counts to 0
              orderedNetInit = if orderedNets == [] then ""
                               else
                                    "-- initialise ordered networks\n"   ++
                                    undefineOrderedNets orderedNets ++ "\n\n" ++
                                    "-- set counts to zero\n" ++
                                    allZeroCounts orderedNets ++ "\n"
                                    ++ "\n-------------------------------\n\n"
          in
              -- all new lines are inside the variables
              "startstate\n\n" ++
              machinesInit     ++
              unorderedNetInit ++
              orderedNetInit

  where
   -----------------------------
   -----------------------------

   -- initialise all fields of all ocurences of a machine
   initMachine :: ( MachineType, State, [FieldStart] ) -> String
   initMachine (machine, startstate, fields)
     = let
           fieldsInitialisation =
             if fields == [] then ""
             else (pushBy 2 (initFields machine fields) ++ "\n")
       in
           "-- " ++ machine ++ " initialisation\n" ++
           "for " ++ formalIndexStr machine ++ ":" ++
           indexTypeStr machine ++ " do\n" ++
           "  " ++ initState machine startstate ++ "\n" ++
           fieldsInitialisation ++
           "endfor;\n"

   -----------------------------

   -- machine fields

   -- print the initialisation of a list of fields of a machine
   initFields :: MachineType -> [FieldStart] -> String
   initFields machine = (mapconcatln . initField) machine

   -- print the initialisation of a field of a particular machine
   initField :: MachineType -> FieldStart -> String
   initField machine (field, val)
     = let (Decl name _) = field
           thisField =  indexedFormalStr machine ++ "." ++ name
       in  case val of
            (Just value) ->  setValue machine (field, (Just value))
            (Nothing)    -> "undefine " ++ thisField ++ ";"

   -----------------------------

   -- intialise a field. Particular care is taken with arrays
   setValue :: MachineType -> FieldStart -> String
   setValue machine ( (Decl name decltype) , (Just value))
     = case decltype of
                           -- remove let
            (Array _ _) -> let decl = (Decl name decltype)
                           in  initialiseArray machine ( decl, (Just value ))
            (Set _ _)   -> error "can't initialise a set!"
            (_)         -> indexedFormalStr machine ++ "." ++ name
                            ++ " := " ++ value ++ ";"

   -----------------------------

   initialiseArray :: MachineType -> FieldStart -> String
   initialiseArray machine ((Decl name decltype), Just (value))
     = case decltype of
           (Array indexType innerType) -> let alias = indexedFormalStr machine ++ "." ++ name ++ "[index0]"
                                          in  "for index0:" ++
                                               complexIndexType indexType ++
                                               "do\n" ++
                                               (pushBy 2 (ith_intialisation
                                                          alias
                                                          1
                                                          innerType
                                                          value))

           _                           -> error "initialiseArray used on other type"
   -----------------------------
   -- initialises an array at i-th depth. Needed for arrays of arrays,
   -- because we need to use different indices
   ith_intialisation :: String -> Int -> Type -> Value -> String
   ith_intialisation name depth decltype value
    = case decltype of
          (Array indexType innerType) -> "for index" ++ show depth ++
                                          ":" ++ complexIndexType indexType
                                           ++ "do\n" ++
                                           pushBy 2 ( ith_intialisation
                                                      (name ++ "[index]"
                                                        ++ show depth)
                                                      (depth+1)
                                                      innerType
                                                      value )

   -- bottom of recursion
          _                           -> name ++ " := " ++ value ++ ";\n"

   ------ helper function
   complexIndexType :: (Either Size MachineType) -> String
   complexIndexType (Left size)     = "0 to " ++ show size ++ "-1"
   complexIndexType (Right machine) = indexTypeStr machine
   -----------------------------
   -----------------------------

   -- initialise a machine state
   initState :: MachineType -> State -> String
   initState machine state = indexedFormalStr machine ++
                             ".state := " ++ state ++ ";"

   -----------------------------
   -----------------------------




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
