-- This module has the functions that print the target AST to
-- murphi source code

-- we are simply implementing tomurphi for each datatype




module Murphi where

import qualified TargetAST as Back
import qualified Ast       as Front
import MurphiPrint

-----------------------------------------------------------------

-- general helper functions


decl :: String -> String -> String
decl iden val  = iden ++ " : " ++ val ++ ";\n"

declGen :: (Show a) => (String,a) -> String
declGen (iden,val) = decl iden (show val)

concatWith :: (Show a) => String -> [a] -> String
concatWith split = foldr1 (++ split ++) $ map show

concatln :: (Show a) => [a] -> String
concatln arg = concatWith "\n" arg $ ++ "\n"

concatcomma :: (Show a) => [a] -> String
concatcomma = concatWith ", "

fstCap :: String -> String
fstCap (ch:str) = (toUpper ch) : str

printEnum :: Name -> [Val] -> String
printEnum name values =  name ++ ": enum {\n" ++ (concatWith ",\n" values) ++ "};\n"
-----------------------------------------------------------------

-- type declarations (used throughout the syntax tree)

instance MurphiPrint TypeDecl where
 tomurphi (Decl var varType) = show var ++ ": " ++ tomurphi varType ++ ";\n"




instance MurphiPrint Type where
 tomurphi Boolean                   = "boolean"
 tomurphi (Integer lo hi)           = show lo ++ ".." ++ show hi
 tomurphi (Enum values)             = "enum { " ++
                                      ( concatWith ",\n" values )
                                      ++ " }"
 tomurphi (Array index otherType)   = let formatInd = fstCap index
                                      in   " array [ " ++ formatInd ++ " ]"
                                           ++ " of " ++ toMurphi otherType


------------------------------

type MachineName = String
type Name        = String

-----------------------------------------------------------------

instance MurphiPrint Program where

 tomurphi (Program constants
              types
              variables
              commonFunctions
              machineFunctions
              rules
              startstate
              invariants )

    = "-- Constants\n"          ++ tomurphi constants         ++
      "-- Types\n"              ++ tomurphi types             ++
      "-- Variables\n"          ++ tomurphi variables         ++
      "-- Common Functions\n"   ++ tomurphi commonFunctions   ++
      "-- Machine Functions\n"  ++ tomurphi machineFunctions  ++
      "-- Rules\n"              ++ tomurphi rules             ++
      "-- Startstate\n"         ++ tomurphi startstate        ++
      "-- Invariants"           ++ tomurphi invariants

-----------------------------------------------------------------



-- Constants


instance MurphiPrint Constants where

 tomurphi (Constants machineSizes vcs) = "const\nMachine Sizes\n" ++ machineSizes
                                        ++ "\n\n"
                                        ++ "Network parameters\n" ++ netParams
  where

    onlyMachineNames = map fst machineSizes
    onlySizes        = map snd machineSizes
    machineSize      = map (++"Size") onlyMachines
    machineSizes     =  let indiv = map declGen (zip machineSize onlySizes)
                        in concatln indiv

    ---- <machineName>Size : <num>;
    --------------------------------

    vcVal     = zip (map ("VC_" ++) vcs) [0,1..]

    vcDecl    = let indiv = map declGen vcVal
                in concatln indiv

    numChs    = length vcs

    netParams = vcDecl ++ "\n" ++
                "NUM_VCs : " show numChs ++ ";\n"

   ---- VC_<VCName> : <num>;
   ---- NUM_VCs : <num>;

-----------------------------------------------------------------


-- Types

tomurphi types = "-- type\n" ++
                 "-- for indexing\n"   ++ finalScalarsets ++ "\n"
                                       ++ finalNodes      ++ "\n" ++
                 vcType ++ "\n"
                 "-- Message Type\n"   ++ finalMsgType    ++ "\n" ++
                 "-- Message\n"        ++ message         ++ "\n" ++
                 "-- Machine States\n" ++ finalMstates    ++ "\n"
  where
   machinesSizes   = machineSizesT types

   sclarasets      = let sizes = map snd machineSizes
                      in map ( \size -> "scalarset(" ++ show size ++ ")" ) sizes

   finalScalarsets = let machines       = map fst machinesSizes
                       formatMachines = map fstCap machines
                       finalPairs     = zip formatMachines scalarsets
                       strs           = map declGen finalPairs
                   in concatln strs

   ---- Machine1 : scalarset(<size>); (fst letter cap)


   -----------------------------
   nodes        = nodes types
   finalNodes   = "Node: union { " ++ concatcomma nodes ++ " };\n"

   -----------------------------
   vcType       = "VC_Type : 0..NUM_VCs -1;\n"
   ----
   msgType      = msgType types -- just strings of all the possible mtypes
   finalMsgType = printEnum "MessageType" msgType


   -----------------------------
   msgArgs      = msgArgs types
   finalMsgArgs = let invid = map declGen msgArgs
                  in concatln indiv
   message      = "Message:\n Record\n  mtype : MessageType;\n  src : Node\n"
                  ++ finalMsgArgs ++ "end;\n"

   -----------------------------
   mstates      = machineStates types

   printMstate :: (MachineName,[StateName],[typeDecl]) -> String
   printMstate (machine, states, types)
                = machine ++ "State:\n record\n" ++
                  printEnum "state" states ++
                  concatWith ",\n" (map toMurphi types)
                  ++ "end;\n"
   finalMstates = concatWith "\n" $ map printMstate mstates


-----------------------------------------------------------------

Instance MurphiPrint Variables where
 tomurphi = undefined

 ----------------------------------------------------------------

 Instance MurphiPrint CommonFunctions where
  tomurphi = undefined

----------------------------------------------------------------

Instance MurphiPrint MachineFunctions where
 tomurphi = undefined

----------------------------------------------------------------

Instance MurphiPrint Rules where
 tomurphi = undefined

----------------------------------------------------------------

Instance MurphiPrint Startstate where
 tomurphi = undefined

----------------------------------------------------------------

Instance MurphiPrint Invariants where
 tomurphi = undefined

----------------------------------------------------------------
