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
import MurphiGenHelper

-- helper tomurphi implementations
import tomurphiHelper

-----------------------------------------------------------------
-----------------------------------------------------------------

instance Cl.MurphiClass Types where

 tomurphi types = "type\n" ++
                  "-- for indexing\n"   ++ finalIndexVars  ++ "\n"
                                        ++ finalNodes      ++ "\n" ++
                  vcType ++ "\n" ++
                  "-- Message Type\n"   ++ finalMsgType    ++ "\n" ++
                  "-- Message\n"        ++ message         ++ "\n" ++
                  "-- Machine States\n" ++ finalMstates    ++ "\n"

  where
    -----------------------------
    -- machine indices

    -- print the indices of all machines
    finalIndexVars = map machineIndex (machinesForType types)

    -- print the index used for this machine, symmetric or non-symmetric
    machineIndex :: Machine -> String
    machineIndex machine = case machine of (Symmetric _ _)   -> machineScalarset machine
                                           (Nonsym _ _)      -> machineEnums machine

    -- for symmetric machines, print scalarsets
    machineScalarset :: Machine -> String
    machineScalarset (Symmetric machine size)
      = decl toMachineScalar (Symmetric machine size)
             "scalarset(" ++ show size ++ ")"

    -- for non-symmetric machines, print enums
    -- the values are <machine name><num>
    -- e.g. dir1, dir2, dir3
    machineEnums :: Machine -> String
    machineEnums (Nonsym machine num)
      = let front = replicate num machine -- need num copies of machine
            -- match each copy with a number
            pairs = zip front [0,1..num-1]
            -- turn each pair into the desired string/enum
            enumValues = map ( \(machine, num) -> machine ++ show num ) pairs
        in  printEnum machine enumValues

    -----------------------------

    -- node i.e. the union of the machines
    finalNodes  = "Node: union { " ++
                   concatcomma (map indexName (machinesForType types))
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
    message      = "Message:\n record\n  mtype : MessageType;\n  src : Node\n"
                   ++ (pushBy 2 finalMsgArgs) ++ "\n end;\n"

    -----------------------------

    -- machine state record
    mstates      = machineStates types

    printMstate :: (MachineName,[StateName],[TypeDecl]) -> String


    printMstate (machine, states, fields)
                 = toMachineState machine ++ ":\n record\n" ++
                   pushBy 2 ( printEnum "state" states ++
                              concatWith ",\n" (map Cl.tomurphi fields))
                   ++ "\n end;\n"
    finalMstates = concatln $ map printMstate mstates

-----------------------------------------------------------------
