
-- functions that print the target AST to murphi source code


-- allow type synonyms to implement typeclasses
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}



module MurphiPrint where

import  MurphiAST
import qualified MurphiClass as Cl
import Data.Char
import Data.List.Split -- for tokenizing strings
                       -- splitOn is used in pushBy

-----------------------------------------------------------------


-----------------------------------------------------------------
-- general helper functions


decl :: String -> String -> String
decl iden val  = iden ++ " : " ++ val ++ ";\n"

declGen :: (Show a) => (String, a) -> String
declGen (iden,val) = decl iden (show val)

concatWith :: String -> [String] -> String
concatWith split = foldr1 (\x y -> x ++ split ++ y)

concatln ::  [String] -> String
concatln arg = (concatWith "\n" arg ) ++ "\n"

concatcomma :: [String] -> String
concatcomma = concatWith ", "

fstCap :: String -> String
fstCap (ch:str) = (toUpper ch) : str

printEnum :: Name -> [Val] -> String
printEnum name values = name ++ ": enum { " ++
                        (concatWith (",\n" ++ alignSpace) values) ++
                        " };\n"
 where
  numSpaces  = length (name ++ ": enum { ")
  alignSpace = replicate numSpaces ' '



mapconcatln :: (a -> String) -> [a] -> String
mapconcatln f list = let indiv = map f list
                     in  concatln indiv

disjunction :: [String] -> String
disjunction = concatWith " | "

-- moves each line by the specified number of spaces
pushBy :: Int -> String -> String
pushBy num = let spaces = replicate num ' '
             in  concatln . map (spaces ++ ) . takeWhile (/= "") . splitOn "\n"
             -- takeWhile is for dismissing the empty new lines

-----------------------------------------------------------------
-- type declarations (used throughout the syntax tree)

instance Cl.MurphiClass TypeDecl where
 --tomurphi :: TypeDecl -> String
 tomurphi (Decl var varType) = var ++ ": " ++ Cl.tomurphi varType ++ ";"
--------------------------------

-- for assignment statements
assign :: TypeDecl -> String
assign (Decl var varType) = show var ++ ":= " ++ Cl.tomurphi varType ++ ";"


instance Cl.MurphiClass Type where

 --tomurphi :: Type -> String
 tomurphi Boolean                   = "boolean"

 tomurphi (Integer lo hi)           = show lo ++ ".." ++ show hi

 tomurphi (Enum values)             = "enum { " ++
                                      ( concatWith ",\n" values )
                                      ++ " }"

 tomurphi (Node machine)            = machine

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
    machinesSizes    = mapconcatln declGen (zip machineSize onlySizes)

    ---- <machineName>Size : <num>;
    --------------------------------

    vcVal     = zip (map ("VC_" ++) vcs) [0,1..]

    vcDecl    = mapconcatln declGen vcVal

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
                  "-- Machine States\n" ++ finalMstates    ++ "\n"

   where
    machinesSizes   = machineSizesT types

    scalarsets      = let sizes = map snd machinesSizes
                      in  map ( \size -> "scalarset(" ++ show size ++ ")" ) sizes

    finalScalarsets = let machines       = map fst machinesSizes
                          formatMachines = map (++ "Ind") machines
                          finalPairs     = zip formatMachines scalarsets
                          -- use uncurry decl instead of declGen (that takes pairs)
                          -- because the latter has show to the second argument
                          -- and strings are printed with ""
                          strs           = map (uncurry decl) finalPairs
                      in  concatln strs

    ---- <machine1>Ind : scalarset(<size>); (fst letter cap)


    -----------------------------
    allMachines  = nodes types
    finalNodes   = "Node: union { " ++ concatcomma allMachines ++ " };\n"

    -----------------------------
    vcType       = "VC_Type : 0..NUM_VCs -1;\n" -- no error generated
                                                -- even though vcType is
                                                -- function in MurphiAST
    ----------------------------
    -- kinds of msgs (e.g. Ack, Fwd etc.)
    msgTypes     = msgType types -- just strings of all the possible mtypes
    finalMsgType = printEnum "MessageType" msgTypes


    -----------------------------
    -- fields/arguments of msgs (e.g. src)
    msgFields    = msgArgs types
    finalMsgArgs = mapconcatln Cl.tomurphi msgFields
    message      = "Message:\n Record\n  mtype : MessageType;\n  src : Node\n"
                   ++ (pushBy 2 finalMsgArgs) ++ " end;\n"

    -----------------------------
    mstates      = machineStates types

    printMstate :: (MachineName,[StateName],[TypeDecl]) -> String


    printMstate (machine, states, fields)
                 = machine ++ "State:\n record\n" ++
                   pushBy 2 ( printEnum "state" states ++
                              concatWith ",\n" (map Cl.tomurphi fields))
                   ++ " end;\n"
    finalMstates = concatln $ map printMstate mstates

-----------------------------------------------------------------



instance Cl.MurphiClass Variables where

 tomurphi :: Variables -> String
 tomurphi variables = "var\n" ++
                      "-- machines\n"       ++ finalMachines ++ "\n" ++
                      "--ordered Nets\n"    ++ finalOrd      ++ "\n" ++
                      "-- unordered Nets\n" ++ finalUnord    ++ "\n"
  where
   machineNames = machines variables
   formatMachine :: MachineName -> String
   formatMachine machine = machine ++ "s: " ++ " array [" ++ machine ++ "Ind" ++ "]"
                           ++ " of " ++ machine ++ "State;"
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

-----------------------------------------------------------------



instance Cl.MurphiClass CommonFunctions where
 tomurphi (FuncParams orderedNetNames sendInfo  ) =
    finalSend      ++ "\n" ++
    finalAdvanceQ  ++ "\n" ++
    errorFunctions ++ "\n"

  where
   countName :: NetName -> String
   countName netName = netName ++ "count"

   -- advance ordered network queue
   advanceQ netName = let count = countName netName
                      in "procedure Advance" ++ netName ++ "(n:Node);\n" ++
                          "begin\n" ++
                          " Assert (" ++  count ++ "[n] > 0) \"Trying to advance empty Q\";\n" ++
                          " for i := 0 to " ++ count ++ "[n]-1 do\n" ++
                            "  if i < " ++ count ++ "[n]-1 then\n   " ++
                              netName ++ "[n][i] := " ++ netName ++ "[n][i+1];\n" ++
                            "  else\n" ++
                              "   undefine " ++ netName ++ "[n][i];\n" ++
                            "  endif;\n" ++
                          " endfor;\n" ++
                          " " ++ count ++ "[n] := " ++ count ++ "[n] - 1;\n" ++
                          "end;\n"

   finalAdvanceQ = mapconcatln advanceQ orderedNetNames

   -----------------------------

   -- Send
   msgArgs = fst sendInfo
   netVCs  = snd sendInfo



   sendTop = "Procedure Send(mtype: MessageType;\n" ++
             pushBy 15 (mapconcatln Cl.tomurphi msgArgs) ++
             "               " ++ "dst: Node" ++ ");"

   sendNext = "var\n  msg: Message;\nbegin\n"

   sendEnd = "end;\n"

   msgFieldAssign :: MsgArg -> String
   msgFieldAssign msgArg = "msg." ++ assign msgArg

   assignments = mapconcatln msgFieldAssign msgArgs

   finalSend = sendTop     ++ "\n" ++
               sendNext    ++ "\n" ++
               assignments ++ "\n" ++
               printedNets ++ "\n" ++
               sendEnd

   -----------------------------
   nets = map fst netVCs
   vcs  = map snd netVCs

   -- all the if-then clauses for checking for each network if the msg is in
   -- the one of the VCs of the network
   printedNets = printAddNet nets vcConds

   -- adding msg to Network
   printAddNet (net:nets) (cond:conds) =
    " if " ++ cond ++ " then\n  " ++ addToNet net ++ "\n" ++ printAddNetRest nets conds

   printAddNetRest [net] [cond] = " else\n  " ++ addToNet net ++ "\n endif;"
   printAddNetRest (net : nets) (cond : conds)
     = " elsif " ++ cond ++ " then\n  " ++ addToNet net ++"\n" ++ printAddNetRest nets conds

   -- list of conditions for adding to a msg to a net
   -- each element of list is the disjunction that is the condition for this net
   vcConds = let temp = map (map ("vc = " ++)) vcs
            in  map disjunction temp

   -- adding a msg to an arbitrary net
   addToNet :: NetName -> String
   addToNet net | isOrdered net  = addOrd net
                | otherwise      = addUnord net

   -- adding a msg to an ordered net
   addOrd net = let count = countName net
                in  "Assert("++ count ++ "[dst] < NET_MAX)" ++
                    " \"Too many msgs on " ++ net ++ "Q\";\n" ++
                    "  " ++ net ++ "[dst][" ++ count ++ "[dst]] := msg;\n" ++
                    "  " ++ count ++ "[dst] := " ++ count ++ "[dst] + 1;\n"

   -- adding a msg to an unordered net
   addUnord net =
    "Assert (MultiSetCount(i:" ++ net ++ "[dst], true) < NET_MAX)" ++
    "\"Too many messages\";\n" ++
    "  MultiSetAdd(msg, " ++ net ++ "[dst]);\n"

   -----------------------------
   -- helper function
   isOrdered :: NetName -> Bool
   isOrdered net = net `elem` orderedNetNames

   -----------------------------
   -- error functions
   errorFunctions = "procedure ErrorUnhandledMsg();\n" ++
                    "begin\n" ++
                    "  error \"Unhandled message type!\";\n" ++
                    "end;\n\n"++

                    "procedure ErrorUnhandledState();\n" ++
                    "begin\n" ++
                    "  error \"Unhandled state!\";\n"++
                    "end;"


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
