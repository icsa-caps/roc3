
-- functions that print the target AST to murphi source code


-- allow type synonyms to implement typeclasses
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}



module Murphi where

import  MurphiAST
import qualified MurphiClass as Cl
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

instance Cl.MurphiClass TypeDecl where
 --tomurphi :: TypeDecl -> String
 tomurphi (Decl var varType) = show var ++ ": " ++ Cl.tomurphi varType ++ ";\n"


instance Cl.MurphiClass Type where

 --tomurphi :: Type -> String
 tomurphi Boolean                   = "boolean"

 tomurphi (Integer lo hi)           = show lo ++ ".." ++ show hi

 tomurphi (Enum values)             = "enum { " ++
                                      ( concatWith ",\n" values )
                                      ++ " }"

 tomurphi (Array index otherType)   = let formatInd = fstCap index
                                      in   " array [ " ++ formatInd ++ " ]"
                                           ++ " of " ++ Cl.tomurphi otherType

 tomurphi (Set (Left machine) otherType) = "multiset[" ++ machine ++ "Size"
                                          ++ "] of " ++ Cl.tomurphi otherType
 tomurphi (Set (Right size) otherType)   = "multiset[" ++ show size
                                          ++ "] of " ++ Cl.tomurphi otherType


------------------------------

type MachineName = String

-----------------------------------------------------------------

instance Cl.MurphiClass Program where

 --tomurphi :: Program -> String
 tomurphi (Program constants
              types
              variables
              commonFunctions
              machineFunctions
              rules
              startstate
              invariants )

    = "-- Constants {{{\n"          ++ Cl.tomurphi constants         ++ "-- }}}" ++
      "-- Types {{{\n"              ++ Cl.tomurphi types             ++ "-- }}}" ++
      "-- Variables {{{\n"          ++ Cl.tomurphi variables         ++ "-- }}}" ++
      "-- Common Functions {{{\n"   ++ Cl.tomurphi commonFunctions   ++ "-- }}}" ++
      "-- Machine Functions {{{\n"  ++ Cl.tomurphi machineFunctions  ++ "-- }}}" ++
      "-- Rules {{{\n"              ++ Cl.tomurphi rules             ++ "-- }}}" ++
      "-- Startstate {{{\n"         ++ Cl.tomurphi startstate        ++ "-- }}}" ++
      "-- Invariants {{{\n"         ++ Cl.tomurphi invariants        ++ "-- }}}"

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



instance Cl.MurphiClass Types where

 --tomurphi :: Types -> String
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
    msgFields    = msgArgs types                      -- :: TypeDecl from MurphiAST
    finalMsgArgs = let indiv = map Cl.tomurphi msgFields -- so we use tomurphi
                   in  concatln indiv
    message      = "Message:\n Record\n  mtype : MessageType;\n  src : Node\n"
                   ++ finalMsgArgs ++ "end;\n"

    -----------------------------
    mstates      = machineStates types

    printMstate :: (MachineName,[StateName],[TypeDecl]) -> String
    printMstate (machine, states, types)
                 = machine ++ "State:\n record\n" ++
                   printEnum "state" states ++
                   concatWith ",\n" (map show types)
                   ++ "end;\n"
    finalMstates = concatWith "\n" $ map printMstate mstates

-----------------------------------------------------------------



instance Cl.MurphiClass Variables where

 tomurphi :: Variables -> String
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



instance Cl.MurphiClass CommonFunctions where
 tomurphi = undefined

----------------------------------------------------------------



instance Cl.MurphiClass MachineFunctions where
 tomurphi = undefined

----------------------------------------------------------------



instance Cl.MurphiClass Rules where
 tomurphi = undefined

----------------------------------------------------------------



instance Cl.MurphiClass Startstate where
 tomurphi = undefined

----------------------------------------------------------------



instance Cl.MurphiClass Invariants where
 tomurphi = undefined

----------------------------------------------------------------

-----------------------------------------------------------------
