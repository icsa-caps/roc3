
-- functions that print the target AST to murphi source code


-- allow type synonyms and composite types to implement typeclasses
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
-- general helper functions and type synonyms

type MachineName = String


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

mapconcatlnComma :: (a -> String) -> [a] -> String
mapconcatlnComma f list = let indiv = map f list
                          in  concatlnWith ",\n" indiv


disjunction :: [String] -> String
disjunction = concatWith " | "


--------------------------------

-- the variable that corresponds to an array of a type of machine
-- e.g. in the original MSI murphi implementation "l1caches" for l1 cache
-- here we just add an 's' in the end
toVarM :: MachineType -> String
toVarM machine = machine ++ "s"

-- gives the name of the variable used for indexing this type of machine
-- i.e. the name of the corresponding scalarset
machineIndex :: MachineType -> String
machineIndex machine = machine ++ "Index"

-- gives the name of the record for this machine (the state of the machine)
toMachineState :: MachineType -> String
toMachineState machine = machine ++ "State"

--------------------------------

-- moves each line by the specified number of spaces
pushBy :: Int -> String -> String
pushBy num = let spaces = replicate num ' '
             in  concatln . map (spaces ++ ) . takeWhile (/= "") . splitOn "\n"
             -- takeWhile is for dismissing the empty new lines

--------------------------------

-- extracting elements of triplets
fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thrd3 :: (a,b,c) -> c
thrd (a,b,c) = c

-- extract info from a Message
mtype :: Message -> String
mtype Message mtype _ = mtype

msgparameters :: Message -> [Maybe (Param, Owner)]
msgparameters Message _ params = params

--------------------------------

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



-----------------------------------------------------------------

-- responses (in receive functions)
instance Cl.MurphiClass Respond where
 -- state :: String
 tomurphi (ToState state)      = "node.state := " ++ state ++ ";"
 -- Message :: Message MsgType [Maybe Field]
 -- src,dst :: Field
 tomurphi (Send Message (mtype params) src dst)
    = "Send(" ++ Cl.tomurphi src ++ ",\n" ++
      "     " ++ Cl.tomurphi dst ++ ",\n" ++
      pushBy 6 (mapconcatlnComma Cl.tomuphi params) ++ ");"

 -- var,value :: Field
 tomurphi (Assign var value)     = Cl.tomurphi var ++ " := " ++ Cl.tomurphi value ++ ";"

 -- elem :: Either Field Val
 tomurphi (Add setName elem)   = "AddTo" ++ setName ++ "List(" ++ Cl.tomurphi elem ++ ");"
 tomurphi (Del setName elem)   = "RemoveFrom" ++ setName ++ "List(" ++ Cl.tomurphi elem ++ ");"

--------------------------------

instance Cl.MurphiClass Field where
 tomurphi (Field var Global) = var
 tomurphi (Field var Msg)    = "msg." ++ var
 tomurphi (Field var (Machine machine)) = toMachineState machine ++ "." ++ var


instance Cl.MurphiClass (Either Field Val) where
 tomurphi (Left field) = Cl.tomurphi field
 tomurphi (Right val)  = val

instance Cl.MurphiClass Maybe Field where
 tomurphi Nothing = "UNDEFINED"
 tomuprhi (Just (field)) = Cl.tomurphi field



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
                          formatMachines = map (machineIndex) machines
                          finalPairs     = zip formatMachines scalarsets
                          -- use uncurry decl instead of declGen (that takes pairs)
                          -- because the latter has show to the second argument
                          -- and strings are printed with ""
                          strs           = map (uncurry decl) finalPairs
                      in  concatln strs


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
    message      = "Message:\n record\n  mtype : MessageType;\n  src : Node\n"
                   ++ (pushBy 2 finalMsgArgs) ++ " end;\n"

    -----------------------------
    mstates      = machineStates types

    printMstate :: (MachineName,[StateName],[TypeDecl]) -> String


    printMstate (machine, states, fields)
                 = toMachineState machine ++ ":\n record\n" ++
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
   formatMachine machine = toVarM machine ++ " array [" ++ machineIndex machine ++ "]"
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
             "               " ++ "src: Node;\n" ++
             "               " ++ "dst: Node;\n" ++
             pushBy 15 (mapconcatln Cl.tomurphi msgArgs) ++ ");"

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
 tomurphi (MachineFunction machineSetsTypeFunc) = undefined
  where
   sets     = map fst3 machineSetsTypeFunc
   machines = map snd3 machineSetsTypeFunc
   receive  = map thrd3 machineSetsTypeFunc

   -----------------------------
   -- print functions for adding and removing elements from a field that is
   -- a set (e.g. list of sharers in MSI)
   setFunctions :: MachineType -> TypeDecl -> String
   setFunctionsunction machine set = addToSet set machine ++ "\n" ++
                                     removeFromSet set machine
   addToSet :: MachineType ->  TypeDecl -> String
   addToSet (Decl set (Set _ type)) machine
     = let thisSet = toVarM machine ++ "." ++ set
       in  "procedure addTo" ++ name ++ "List" ++
           "(x: " ++ Cl.tomurphi type ++ ");\nbegin\n" ++
           " if MultiSetCount( i:" ++ thisSet ++ ", " ++
           thisSet ++ "[i] = x ) != 0\n" ++
           " then\n" ++ "  MultiSetAdd(x," ++ thisSet ++ " );\n" ++
           " endif;\nend;\n"

   addToSet _            = error "Used MurphiPrint.removeFromSet on a non-set"

   removeFromSet :: MachineType -> TypeDecl -> String
   removeFromSet machine (Decl set (Set _ type))
    = let thisSet = toVarM machine ++ "." ++ set
      in  "procedure RemoveFrom" ++ set ++ "List" ++
          "( x: " ++ Cl.tomurphi type ++ " );\n" ++
          "begin\n" ++
          " MultiSetRemovePred(i:" ++ thisSet ++ "," ++  thisSet ++ "[i] = x);\n" ++
          "end;\n"

   removeFromSet _            = error "Used MurphiPrint.removeFromSet on a non-set"

   finalSetFunctions :: MachineType -> [TypeDecl] -> String
   finalSetFunctions machine sets = mapconcatln $ setFunctions machine sets

   -----------------------------

   machineReceive machine
    = "function " ++ machine ++ "Receive(msg:Message; index: "
      ++ machineIndex machine ++") : boolean;\n" ++
      "begin\n" ++
      " alias node: " ++ toVarM machine ++ "[index] do\n" ++
      "  switch node.state\n" ++
      "---------------------- other stuff -----------------------" ++
      "   else\n" ++
      "    ErrorUnhandledState();\n\n" ++
      "  endswitch;\n" ++
      " endalias;\n" ++
      " -- Message processed\n" ++
      " return true;\n" ++
      "end;\n"




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
