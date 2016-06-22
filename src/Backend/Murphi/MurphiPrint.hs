
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
concatWith _ []       = ""  -- foldr1 doesnt support empty lists
concatWith split strs = foldr1 (\x y -> x ++ split ++ y) strs


concatln ::  [String] -> String
concatln arg = (concatWith "\n" arg )


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
                          in  concatWith ",\n" indiv


disjunction :: [String] -> String
disjunction = concatWith " | "


-- printing if-elsif-else-statements
printIfElse :: [String] -> [String] -> String
printIfElse (cond:conditions) (bod:bodies) = "if " ++ cond ++ " then\n  " ++
                                             bod ++ "\n\n" ++
                                             printElsif conditions bodies


-- printing elsif to else statements
printElsif :: [String] -> [String] -> String
printElsif [cond] [body]                   = "else\n  " ++ body ++ "endif;"
printElsif (cond:conditions) (body:bodies)  = "elsif " ++ cond ++ " then\n  " ++
                                             body ++ "\n\n" ++
                                             printElsif conditions bodies
--------------------------------

-- gives the name of the variable used for indexing this type of machine
-- i.e. the name of the corresponseing scalarset
toMachineIndex :: MachineType -> String
toMachineIndex machine = machine ++ "Index"

-- the name of the array of this type of machines
toMachineArray :: MachineType -> String
toMachineArray machine = machine ++ "s"

machineArrayAtIndex :: MachineType -> Index -> String
machineArrayAtIndex machine index = toMachineArray machine ++
                                   "[" ++ show index ++ "]"



-- gives the name of the record for this machine (the state of the machine)
toMachineState :: MachineType -> String
toMachineState machine = machine ++ "State"



--------------------------------


-- moves each line by the specified number of spaces
pushBy :: Int -> String -> String
pushBy num = let spaces = replicate num ' '
             in  concatln . map (spaces ++ ) . splitOn "\n"
             -- filter (/=0) is for dismissing the empty new lines


--------------------------------

-- extracting elements of triplets
fst3 :: (a,b,c,d) -> a
fst3 (a,b,c,d) = a

snd3 :: (a,b,c,d) -> b
snd3 (a,b,c,d) = b

thrd3 :: (a,b,c,d) -> c
thrd3 (a,b,c,d) = c


-- extract info from a Message
getMtype :: Message -> String
getMtype (Message mtype _ ) = mtype

getMsgParams :: Message -> [Maybe Field]
getMsgParams (Message _ params) = params

--------------------------------

-- how many msgs are in this buffer?
countName :: NetName -> String
countName netName = netName ++ "count"


-----------------------------------------------------------------
--------------- helper tomurphi implementations -----------------
-----------------------------------------------------------------



-- type declarations (used throughout the syntax tree)

instance Cl.MurphiClass TypeDecl where
 --tomurphi :: TypeDecl -> String
 tomurphi (Decl var varType) = var ++ ": " ++ Cl.tomurphi varType ++ ";"
--------------------------------

-- for assignment statements
assign :: TypeDecl -> String
assign (Decl var varType) = var ++ ":= " ++ Cl.tomurphi varType ++ ";"


instance Cl.MurphiClass Type where

 tomurphi Boolean                   = "boolean"

 tomurphi (Integer lo hi)           = show lo ++ ".." ++ show hi

 tomurphi (Enum values)             = "enum { " ++
                                      ( concatWith ",\n" values )
                                      ++ " }"

 tomurphi (Node machine)            = machine

 tomurphi (Array index otherType)   = let formatInd = fstCap (show index)
                                      in   " array [ " ++ formatInd ++ " ]"
                                           ++ " of " ++ Cl.tomurphi otherType

 tomurphi (Set (Left machine) otherType) = "multiset[" ++ machine ++ "Size"
                                          ++ "] of " ++ Cl.tomurphi otherType
 tomurphi (Set (Right size) otherType)   = "multiset[" ++ show size
                                          ++ "] of " ++ Cl.tomurphi otherType

--------------------------------

-- responses (in receive functions)

instance Cl.MurphiClass Response where

 -- state :: String
 tomurphi (ToState machine index state) = machineArrayAtIndex machine index ++
                                          ".state = " ++ state ++ ";"


 -- Message :: Message MsgType [Maybe Field], src,dst :: Field
 tomurphi (Send (Message mtype params) src dst)
    = "Send(" ++ Cl.tomurphi src ++ ",\n" ++
      "     " ++ Cl.tomurphi dst ++ ",\n" ++
      pushBy 5 (mapconcatlnComma Cl.tomurphi params) ++ ");"


 tomurphi (Assign var value)    = Cl.tomurphi var ++ " := " ++ Cl.tomurphi value ++ ";"


 -- elem :: Field
 tomurphi (Add (Machine machine index) setName elem)
   = "AddTo" ++ toMachineArray machine ++ fstCap setName ++ "List(" ++
     Cl.tomurphi elem ++ ", " ++ show index ++ ");"

 tomurphi (Del (Machine machine index) setName elem)
   = "RemoveFrom" ++ toMachineArray machine ++ fstCap setName ++ "List(" ++
     Cl.tomurphi elem ++ ", " ++ show index ++ ");"

 tomurphi (Stall) = "return false;" -- message is not processed

--------------------------------

instance Cl.MurphiClass Field where
 tomurphi (Field variable owner) = Cl.tomurphi owner ++ Cl.tomurphi variable

--------------------------------


variableName :: Variable -> String
variableName (Simple varName)              = varName
variableName (ArrayElem arrayName _)       = arrayName
variableName (MachineArray machineName _)  = machineName

fieldName :: Field -> String
fieldName (Field variable owner) = variableName variable



instance Cl.MurphiClass Owner where
  tomurphi Msg = "msg."
  tomurphi Global = ""
  tomurphi Local  = ""
  tomurphi ThisNode = "node."
  tomurphi (Machine machine index) = machineArrayAtIndex machine index ++ "."

--------------------------------

instance Cl.MurphiClass Variable where
 tomurphi (Simple varName) = varName
 tomurphi (ArrayElem arrayName index) = arrayName ++ "[" ++ show index ++ "]"
 tomurphi (MachineArray machine index) = machineArrayAtIndex machine index

--------------------------------

instance Cl.MurphiClass (Either Field Val) where
 tomurphi (Left field) = Cl.tomurphi field
 tomurphi (Right val)  = val

--------------------------------

instance Cl.MurphiClass (Maybe Field) where
 tomurphi Nothing = "UNDEFINED"
 tomurphi (Just (field)) = Cl.tomurphi field

--------------------------------

instance Cl.MurphiClass Guard where
 tomurphi (Receive mtype) = "msg.mtype = " ++ mtype
 -- make sure the correct alias or variable (<machine>State) is passed for node
 tomurphi (AtStateAlias node state)       = node ++ ".state = " ++ state
 tomurphi (AtState machine index state)   = machineArrayAtIndex machine index ++
                                            ".state = " ++ state

--------------------------------

instance Cl.MurphiClass LocalVariables where
  tomurphi [] = ""
  tomurphi localVariables = "var\n" ++
                            pushBy 2 (mapconcatln Cl.tomurphi localVariables)

--------------------------------


instance Cl.MurphiClass SelfIssueRule where
  tomurphi (SelfIssueRule rulename guard responses)
    = "rule \"" ++ rulename ++ "\"\n" ++
      pushBy 2 (Cl.tomurphi guard) ++ "\n" ++
      "=>\n" ++
      pushBy 2 (mapconcatln Cl.tomurphi responses) ++ "\n" ++
      "endrule;"




-----------------------------------------------------------------
----------------- Main tomurphi implementations -----------------
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
                          formatMachines = map (toMachineIndex) machines
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
                   ++ (pushBy 2 finalMsgArgs) ++ "\n end;\n"

    -----------------------------
    mstates      = machineStates types

    printMstate :: (MachineName,[StateName],[TypeDecl]) -> String


    printMstate (machine, states, fields)
                 = toMachineState machine ++ ":\n record\n" ++
                   pushBy 2 ( printEnum "state" states ++
                              concatWith ",\n" (map Cl.tomurphi fields))
                   ++ "\n end;\n"
    finalMstates = concatln $ map printMstate mstates

-----------------------------------------------------------------



instance Cl.MurphiClass Variables where

 tomurphi :: Variables -> String
 tomurphi variables = "-- machines\n"       ++ finalMachines ++ "\n" ++
                      "-- ordered Nets\n"    ++ finalOrd      ++ "\n" ++
                      "-- unordered Nets\n" ++ finalUnord    ++ "\n"
  where
   machineNames = machines variables
   formatMachine :: MachineName -> String
   formatMachine machine = toMachineArray machine ++ ": array [" ++ toMachineIndex machine ++ "]"
                           ++ " of " ++ toMachineState machine ++ ";"
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
 tomurphi (FuncParams orderedNetNames sendInfo broadcastInfo  ) =
    finalSend      ++ "\n" ++
    finalAdvanceQ  ++ "\n" ++
    finalBroadcast ++ "\n" ++
    errorFunctions ++ "\n"

  where

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

   sendNext = "var\n  msg: Message;\n\nbegin\n"

   sendEnd = "\nend;\n"

   msgFieldAssign :: MsgArg -> String
   msgFieldAssign (Decl argname argtype) = "msg." ++ argname ++ ":= " ++ argname

   assignments = mapconcatln msgFieldAssign msgArgs

   finalSend = sendTop     ++ "\n" ++
               sendNext    ++ "\n" ++
               pushBy 5 assignments ++ "\n" ++
               pushBy 5 printedNets ++ "\n" ++
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


   -- list of conditions for adding a msg to a net
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
   -- broadcasting functions

   -- broadcast for a single set and msg
   singleBroadcast :: (SetField, Message) -> String
   singleBroadcast (SetField field elemType, msg)
     = let setName = fieldName field
           (Message mtype _) = msg
           srcField = Field (Simple "src") Local
           dstField = Field (Simple "dst") Local
       in  "procedure Cast" ++ fstCap mtype ++ fstCap setName ++
           "(src:Node);\n" ++  -- Node = union of machines,
                                  -- only machines can broadcast msgs
           "begin\n" ++
           "  for n:Node do\n" ++
           "    if  ( IsMember(n, " ++ Cl.tomurphi elemType ++  ") &\n" ++
           "       MultiSetCount(i:" ++ Cl.tomurphi field ++ ", "
           ++ Cl.tomurphi field ++ "[i] = n) != 0 )\n" ++
           "    then\n" ++
           ( pushBy 6 (Cl.tomurphi (Send msg srcField dstField)) ) ++ "\n" ++
           "    endif;\n" ++
           "  endfor;\n" ++
           "end;\n"

   -- all broadcast functions
   finalBroadcast = mapconcatln singleBroadcast broadcastInfo

   -----------------------------

   -- error functions
   errorFunctions = "procedure ErrorUnhandledMsg();\n" ++
                    "begin\n" ++
                    "  error \"Unhandled message type!\";\n" ++
                    "end;\n\n"
                    ++
                    "procedure ErrorUnhandledState();\n" ++
                    "begin\n" ++
                    "  error \"Unhandled state!\";\n"++
                    "end;"


----------------------------------------------------------------



instance Cl.MurphiClass MachineFunctions where

 tomurphi (MachineFunctions machine_Sets_ReceiveFuncion_LocalVars ) =
   mapconcatln setReceiveSingle machine_Sets_ReceiveFuncion_LocalVars

  where

   -----------------------------

   -- add/remove from set

   setFunctions :: MachineType -> TypeDecl -> String
   setFunctions machine set = addToSet machine set ++ "\n" ++
                              removeFromSet machine set


   addToSet :: MachineType -> TypeDecl -> String
   addToSet machine (Decl setName (Set _ elemType))
     = let thisSet = toMachineArray machine ++ "[index]." ++ setName
       in  "procedure addTo" ++ toMachineArray machine ++ setName ++ "List" ++
           "(x: " ++ Cl.tomurphi elemType ++ ", index: " ++ toMachineIndex machine ++
           ");\nbegin\n" ++
           " if MultiSetCount(i:" ++ thisSet ++ ", " ++
           thisSet ++ "[i] = x) != 0\n" ++
           " then\n" ++ "   MultiSetAdd(x," ++ thisSet ++ ");\n" ++
           " endif;\nend;\n"

   addToSet _  _ = error "Used MurphiPrint.removeFromSet on a non-set"


   -- must know also the index of the machine
   removeFromSet :: MachineType -> TypeDecl -> String
   removeFromSet machine (Decl setName (Set _ elemType))
    = let thisSet = toMachineArray machine ++ "[index]." ++ setName
      in  "procedure RemoveFrom" ++ toMachineArray machine ++ setName ++ "List" ++
          "( x: " ++ Cl.tomurphi elemType ++ ", index:" ++ toMachineIndex machine ++
          " );\nbegin" ++
          " MultiSetRemovePred(i:" ++ thisSet ++ "," ++  thisSet ++ "[i] = x);\n"
          ++ "end;\n"

   removeFromSet _ _  = error "Used MurphiPrint.removeFromSet on a non-set"

   finalSetFunctions :: MachineType -> [TypeDecl] -> String
   finalSetFunctions machine sets = mapconcatln (setFunctions machine) sets

   -----------------------------

   -----------------------------
   -- Printing responses and guards e.g taking different cases (if-then) for
   -- mtype and responding
   allResponses :: [Response] -> String
   allResponses responses = mapconcatln Cl.tomurphi responses

   guardedResponses :: (Maybe Guard, [Response]) -> String
   guardedResponses (Just guard, responses) = Cl.tomurphi guard ++ " then\n" ++
                                              pushBy 3 (allResponses responses)
   -- if no guard, just print the responses
   guardedResponses (Nothing, responses) = allResponses responses

   elsifResponses :: [(Maybe Guard, [Response])] -> String
   elsifResponses []           = ""
   elsifResponses guardsResponses =
     let indiv = map guardedResponses guardsResponses
     in  mapconcatln ( "\nelsif " ++ ) indiv ++ "\n"

   ------------
   -- If no guard, then there is only one case to consider
   -- and we print the responses
   finalGuardsResps [(Nothing, responses)] = allResponses responses

   finalGuardsResps ( (Just guard,responses) : rest )
    = "if " ++ guardedResponses (Just guard, responses) ++ "\n" ++
      elsifResponses rest  ++ "\n" ++
      "else\n" ++
      "   ErrorUnhandledMsg();" -- one ln in ouput however many (even 0) I put here


   -----------------------------

   -- taking different cases for each state
   caseState :: ( State, [ ( Maybe Guard, [Response]) ] ) -> String
   caseState (state, guardsResps) = "\nCase " ++ state ++ ":\n" ++
                                    (pushBy 2 (finalGuardsResps guardsResps))

   --Take different cases for all states
   caseAllStates :: [( State, [ (Maybe Guard, [Response]) ] )] -> String
   caseAllStates = mapconcatln caseState

   -----------------------------

   -- printing the receive function
   finalMachineReceive :: MachineType -> ReceiveFunction -> LocalVariables -> String
   finalMachineReceive machine statesGuardsReps localVariables
    = "function " ++ machine ++ "Receive(msg:Message; index: "
      ++ toMachineIndex machine ++") : boolean;\n" ++
      Cl.tomurphi localVariables ++
      "begin\n" ++
      pushBy 3 (caseAllStates statesGuardsReps) ++
      "\n   else\n" ++
      "      ErrorUnhandledState();\n" ++
      "  endswitch;\n\n" ++
      " -- Message processed\n" ++
      " return true;\n" ++
      "end;\n"


   -----------------------------
   -- set + receive functions
   setReceiveSingle :: ( MachineType, Sets, ReceiveFunction, LocalVariables ) -> String
   setReceiveSingle (machine, sets, stateGuardsReps, localVariables) =
     "-- " ++ machine ++ " functions {{{\n" ++
     "-- Add/remove from sets\n" ++
     finalSetFunctions machine sets ++ "\n" ++
     "---------------------------------------------------------\n" ++
     " -- Receive function \n" ++
     finalMachineReceive machine stateGuardsReps localVariables ++
     "\n-- }}}\n"

----------------------------------------------------------------



instance Cl.MurphiClass Rules where
 tomurphi (Rules selfIssued receiveOrdNets receiveUnordNets)
   = finalSelfIssued ++
     "-- ordered networks receive rules" ++
     finalReceiveOrdNets ++
     "-- unordered networks receive rules" ++
     finalReceiveUnordNets

  where
   -----------------------------
   -- self-issued rules

   -- self-issued rules of one machine
   singleMachineSelfIssueRules :: (MachineType, [SelfIssueRule]) -> String
   singleMachineSelfIssueRules (machine, rules)
     = "-- " ++ machine ++ " self-issued rules\n"++
       "ruleset index:" ++ toMachineIndex machine ++ " do\n" ++
       "  alias node:" ++ toMachineArray machine ++ "[index] do\n\n" ++
       pushBy 4 (mapconcatln Cl.tomurphi rules) ++ "\n" ++
       "  endalias;\n" ++
       "endruleset;\n"

   -- self-issued rules for all machines
   allSelfIssued :: [(MachineType, [SelfIssueRule])] -> String
   allSelfIssued = mapconcatln singleMachineSelfIssueRules

   finalSelfIssued = allSelfIssued selfIssued

   -----------------------------
   -- unordered network rules

   -- a single unordered net receive rule
   singleUnordNet :: ReceiveUnordNet -> String
   singleUnordNet (ReceiveUnordNet ruleName netName vcs machines)
     = let disjunctVCs = disjunction $ map ("msg.vc = " ++ ) vcs
           allIsMember  = map (isMember "n") machines
           allProcess  = map processMessage machines
           casesMachines = printIfElse allIsMember allProcess
       in  "ruleset n:Node do\n" ++
           "  choose midx:" ++ netName ++ "[n] do\n" ++
           "    alias chan:net[n]\n" ++
           "      alias msg:chan[midx]\n\n" ++

            "       rule " ++ ruleName ++ "\n" ++
            pushBy 10 (disjunctVCs)    ++ "\n" ++
                    "=>\n" ++
            pushBy 10 casesMachines    ++ "\n" ++
            "       endrule;\n\n" ++

            "     endalias;\n"    ++
            "   endalias;\n"      ++
            "  endchoose;\n"      ++
            "endruleset;\n"

   -- receive rules for a list of unordered nets
   allUnordNet :: [ReceiveUnordNet] -> String
   allUnordNet = mapconcatln singleUnordNet

   -- final
   finalReceiveUnordNets = allUnordNet receiveUnordNets

    -----------
   -- helper functions
   isMember :: String -> MachineName -> String
   isMember alias machine = "IsMember(" ++ alias ++ ", " ++ machine ++ ")"

   processMessage :: MachineName -> String
   processMessage machine = "if " ++ machine ++ "Receive(msg,n) then \n" ++
                             "  MultisetRemove(midx, chan);\n" ++
                             "endif;\n"
   -----------


   -----------------------------
   -- ordered network rules

   -- a single ordered net receive rule
   singleOrdNet :: ReceiveOrdNet -> String
   singleOrdNet (ReceiveOrdNet ruleName netName vcs machines)
     = let disjunctVCs = disjunction $ map ("msg.vc = " ++ ) vcs
           allIsMember  = map (isMember "n") machines
           allDeQ  = map (deQ netName) machines
           casesMachines = printIfElse allIsMember allDeQ
       in  "ruleset n:Node do\n" ++
           "  choose midx:" ++ netName ++ "[n] do\n" ++
           "    alias chan:net[n]\n" ++
           "      alias msg:chan[midx]\n\n" ++

           "       rule " ++ ruleName ++ "\n" ++
           "         " ++ countName netName ++ "[n] > 0\n" ++
           "       =>\n" ++
           pushBy 9 casesMachines ++ "\n" ++
           "       endrule;\n\n" ++

           "     endalias;\n"    ++
           "   endalias;\n"      ++
           "  endchoose;\n"      ++
           "endruleset;\n"

   -- helper function
   deQ :: NetName -> MachineName -> String
   deQ net machine = "if " ++ machine ++ "Receive then\n" ++
                     "   Advance" ++ net ++ "(n);\n" ++
                     "endif;"


   -- receive rules for a list of ordered nets
   allOrdNet :: [ReceiveOrdNet] -> String
   allOrdNet = mapconcatln singleOrdNet

   -- applying it
   finalReceiveOrdNets = allOrdNet receiveOrdNets
   -----------------------------

----------------------------------------------------------------



instance Cl.MurphiClass Startstate where
 tomurphi = undefined

----------------------------------------------------------------



instance Cl.MurphiClass Invariants where
 tomurphi = undefined

----------------------------------------------------------------

-----------------------------------------------------------------
