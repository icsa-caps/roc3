
-- This module has the functions that print the target AST to
-- murphi source code


-- allow type synonyms to implement typeclasses
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}



module Murphi where

import  TargetAST
--import qualified Ast       as Front
import MurphiPrint
import Data.Char

-----------------------------------------------------------------


-----------------------------------------------------------------
-- general helper functions


decl :: String -> String -> String
decl iden val  = iden ++ " : " ++ val ++ ";\n"

declGen :: (Show a) => (String,a) -> String
declGen (iden,val) = decl iden (show val)

concatWith :: String -> [String] -> String
concatWith split = foldr1 (\x y -> x ++ split ++ y)

concatln ::  [String] -> String
concatln arg = (concatWith "\n" arg )++ "\n"

concatcomma :: [String] -> String
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
                                           ++ " of " ++ tomurphi otherType


------------------------------

type MachineName = String


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

    = "-- Constants {{{\n"          ++ tomurphi constants         ++ "-- }}}" ++
      "-- Types {{{\n"              ++ tomurphi types             ++ "-- }}}" ++
      "-- Variables {{{\n"          ++ tomurphi variables         ++ "-- }}}" ++
      "-- Common Functions {{{\n"   ++ tomurphi commonFunctions   ++ "-- }}}" ++
      "-- Machine Functions {{{\n"  ++ tomurphi machineFunctions  ++ "-- }}}" ++
      "-- Rules {{{\n"              ++ tomurphi rules             ++ "-- }}}" ++
      "-- Startstate {{{\n"         ++ tomurphi startstate        ++ "-- }}}" ++
      "-- Invariants {{{\n"         ++ tomurphi invariants        ++ "-- }}}"

-----------------------------------------------------------------



-- Constants
instance MurphiPrint Constants where

 tomurphi (Constants machineSizes vcs) = "const\nMachine Sizes\n" ++ machinesSizes
                                        ++ "\n\n"
                                        ++ "Network parameters\n" ++ netParams
                                        ++ "NET_MAX: " ++ show netMax
  where

    onlyMachines     = map fst machineSizes
    onlySizes        = map snd machineSizes
    machineSize      = map (++"Size") onlyMachines
    machinesSizes    = let indiv = map declGen (zip machineSize onlySizes)
                       in  concatln indiv

    ---- <machineName>Size : <num>;
    --------------------------------

    vcVal     = zip (map ("VC_" ++) vcs) [0,1..]

    vcDecl    = let indiv = map declGen vcVal
                in  concatln indiv

    numChs    = length vcs

    netParams = vcDecl ++ "\n" ++
                "NUM_VCs : " ++ show numChs ++ ";\n"


    netMax = length machineSizes -- just #machines

   ---- VC_<VCName> : <num>;
   ---- NUM_VCs : <num>;

-----------------------------------------------------------------


-- Types
instance MurphiPrint Types where

 tomurphi types = "type\n" ++
                  "-- for indexing\n"   ++ finalScalarsets ++ "\n"
                                        ++ finalNodes      ++ "\n" ++
                  vcType ++ "\n" ++
                  "-- Message Type\n"   ++ finalMsgType    ++ "\n" ++
                  "-- Message\n"        ++ message         ++ "\n" ++
                  "-- Machine States\n" ++ finalMstates    ++ "\n" ++
                  " --"

   where
    machinesSizes   = machineSizesT types

    scalarsets      = let sizes = map snd machinesSizes
                      in  map ( \size -> "scalarset(" ++ show size ++ ")" ) sizes

    finalScalarsets = let machines     = map fst machinesSizes
                          formatMachines = map (++ "Ind") machines
                          finalPairs     = zip formatMachines scalarsets
                          strs           = map declGen finalPairs
                      in  concatln strs

    ---- <machine1>Ind : scalarset(<size>); (fst letter cap)


    -----------------------------
    allMachines  = nodes types
    finalNodes   = "Node: union { " ++ concatcomma allMachines ++ " };\n"

    -----------------------------
    vcType       = "VC_Type : 0..NUM_VCs -1;\n"

    ----------------------------
    -- kinds of msgs (e.g. Ack, Fwd etc.)
    msgTypes     = msgType types -- just strings of all the possible mtypes
    finalMsgType = printEnum "MessageType" msgTypes


    -----------------------------
    -- fields/arguments of msgs (e.g. src)
    msgFields    = msgArgs types -- :: TypeDecl from TargetAST
    finalMsgArgs = let indiv = map tomurphi msgFields -- so we use tomurphi
                   in  concatln indiv
    message      = "Message:\n Record\n  mtype : MessageType;\n  src : Node\n"
                   ++ finalMsgArgs ++ "end;\n"

    -----------------------------
    mstates      = machineStates types

    printMstate :: (MachineName,[StateName],[TypeDecl]) -> String
    printMstate (machine, states, types)
                 = machine ++ "State:\n record\n" ++
                   printEnum "state" states ++
                   concatWith ",\n" (map tomurphi types)
                   ++ "end;\n"
    finalMstates = concatWith "\n" $ map printMstate mstates

-----------------------------------------------------------------

instance MurphiPrint Variables where
 tomurphi variables = "var\n" ++
                      "-- machines\n"     ++ finalMachines ++ "\n" ++
                      "--ordered Nets"    ++ finalOrd      ++ "\n" ++
                      "-- unordered Nets" ++ finalUnord    ++ "\n"
  where
   machineNames = machines variables
   formatMachine :: MachineName -> String
   formatMachine machine = machine ++ "s" ++ " array [" ++ machine ++ "Ind" ++ "]"
                           ++ " of " ++ machine ++ "State;"
   finalMachines = let indiv = map formatMachine machineNames
                   in  concatln indiv

   -----------------------------

   ordNets = orderedNets variables
   formatOrd :: String -> String
   formatOrd net = net ++ ": array[Node] of  array[0..NET_MAX-1] of Message;"

   counts :: String -> String
   counts net = net ++ "count: array[Node] of 0..NET_MAX;"

   combine :: String -> String
   combine net = formatOrd net ++ "\n" ++ counts net

   finalOrd = let  ordNetNames = map netName $ map (Left) ordNets
                   indiv = map combine ordNetNames
              in  concatln indiv
   -----------------------------

   unordNets = unorderedNets variables
   formatUnord :: String -> String
   formatUnord net = net ++ ": array[Node] of multiset [NET_MAX] of Message;"

   finalUnord = let unordNetNames = map netName $ map (Right) unordNets
                    indiv = map formatUnord unordNetNames
                in  concatln indiv

 ----------------------------------------------------------------

-----------------------------------------------------------------

instance MurphiPrint CommonFunctions where
 tomurphi = undefined

----------------------------------------------------------------

instance MurphiPrint MachineFunctions where
 tomurphi = undefined

----------------------------------------------------------------

instance MurphiPrint Rules where
 tomurphi = undefined

----------------------------------------------------------------

instance MurphiPrint Startstate where
 tomurphi = undefined

----------------------------------------------------------------

instance MurphiPrint Invariants where
 tomurphi = undefined

----------------------------------------------------------------
