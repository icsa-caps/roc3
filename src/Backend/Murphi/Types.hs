-----------------------------------------------------------------
-------------- tomurphi implementation for types ----------------
-----------------------------------------------------------------

module Types where

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

instance Cl.MurphiClass Types where

 tomurphi types = "type\n" ++
                  "-- for indexing\n"   ++ finalIndexVars  ++ "\n"
                                        ++ finalNodes      ++ "\n"
                                        ++ vcType          ++ "\n" ++
                  "-- Message Type\n"   ++ finalMsgType    ++ "\n" ++
                  "-- Message\n"        ++ message         ++ "\n" ++
                  "-- Machine States\n" ++ finalMstates    ++ "\n"

  where
    -----------------------------
    -- machine indices



    -- print the indices of all machines
    finalIndexVars = mapconcatln machineIndex (machinesSym types)

    -- print the index used for this machine, symmetric or non-symmetric
    machineIndex :: (MachineType, Size, Symmetry) -> String
    machineIndex (machine, num, symmetry)
        = case symmetry of (Symmetric)    -> machineScalarset machine
                           (Nonsymmetric) -> machineEnums (machine, num)

    -- for symmetric machines, print scalarsets
    machineScalarset :: MachineType -> String
    machineScalarset (machine)
      = decl machine "scalarset(" ++ machineSizeStr machine ++ ")"

    -- for non-symmetric machines, print enums
    -- the values are <machine name><num>
    -- e.g. dir1, dir2, dir3
    machineEnums :: (MachineType, Size) -> String
    machineEnums (machine, num)
      = let front = replicate num machine -- need num copies of machine
            -- match each copy with a number
            pairs = zip front [0,1..num-1]
            -- turn each pair into the desired string/Enum value
            enumValues = map ( \(machine, num) -> machine ++ show num ) pairs
        in  printEnum (machine) enumValues

    -----------------------------

    -- node i.e. the union of the machines
    finalNodes  = "Node: union { " ++
                   concatcomma ( map fst3 (machinesSym types) )
                  ++ " };\n"

    -----------------------------

    vcType = "VC_Type : 0..NUM_VCs -1;\n"

    ----------------------------

    -- kinds of msgs (e.g. Ack, Fwd etc.)
    msgTypes     = msgType types -- just strings of all the possible mtypes
    finalMsgType = printEnum "MessageType" msgTypes


    -----------------------------

    -- fields/arguments of msgs (e.g. src)
    msgFields    = msgArgs types
    finalMsgArgs = mapconcatln Cl.tomurphi msgFields
    message      = "Message:\n" ++
                   "  record\n" ++
                   "    mtype : MessageType;\n" ++
                   "    src : Node\n" ++
                   (pushBy 4 finalMsgArgs) ++ "\n" ++
                   "end;\n"

    -----------------------------

    -- machine state
    -- record of state, which is an Enum of the possible states,
    -- and the fields of the machine

    mstates = machineStates types

    printMstate :: (MachineName, [StateName], [TypeDecl]) -> String
    printMstate (machine, states, fields)
                 = toMachineStateStr machine ++ ":\n record\n" ++
                   pushBy 2 ( printEnum "state" states ++
                              concatWith ",\n" (map Cl.tomurphi fields))
                   ++ "\n end;\n"

    finalMstates = concatln $ map printMstate mstates

-----------------------------------------------------------------
