-----------------------------------------------------------------
--------------- helper tomurphi implementations -----------------
-----------------------------------------------------------------

-- allow type synonyms and composite types to implement typeclasses
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module TomurphiHelper where

--------------------------------

import MurphiAST
import qualified MurphiClass as Cl
import GenHelper
import Data.Maybe -- for fromJust

--------------------------------

-----------------------------------------------------------------
-----------------------------------------------------------------

-- When we refer to machines,
-- if they are symmetric:
-- we either need the index variable we use in functions or the array of
-- machine states indexed at the index variable
-- And if they are non-symmetric, we refer to either
-- a specific value of the Enum index variable, or to the array of states
-- indexed at this value of the Enum

-- We have one method for each in GenHelper and we use these two
-- instead of tomurphi

-- For the first use generalIndex and for the second indexedArray

-- We may use "node" to index a machine.
-- Here a machine is indexed only by its standard local index;
-- for node use it as apropriate in the outer tomurphi implementation
-----------------------------------------------------------------
-----------------------------------------------------------------


instance Cl.MurphiClass Machine where
  tomurphi :: Machine -> String
  tomurphi (AnyType machine)         = error "called Cl.tomurphi on AnyType machine"
  tomurphi (Sym machineType)         = machineType
  tomurphi (Nonsym machineType num)  = machineType ++ show num
  tomurphi (Synonym var)             = var


-----------------------------------------------------------------

instance Cl.MurphiClass TypeDecl where
 tomurphi (Decl var varType) = var ++ ": " ++ Cl.tomurphi varType ++ ";"


--------------------------------


instance Cl.MurphiClass Type where

  tomurphi Boolean                   = "boolean"

  tomurphi (Integer lo hi)           = show lo ++ ".." ++ show hi

  tomurphi (Enum values)             = "enum { " ++
                                      ( concatWith ",\n" values )
                                      ++ " }"

  tomurphi (Node machine)            = indexTypeStr machine

  tomurphi (Array (Right machine) otherType)
    = "array [ " ++ indexTypeStr machine ++ " ]"
      ++ " of " ++ Cl.tomurphi otherType

  tomurphi (Array (Left size) otherType)
    = "array [" ++ "0.." ++ show size ++ "-1] of " ++ Cl.tomurphi otherType

  tomurphi (Set (Right machine) otherType) = "multiset[" ++ machine ++ "Size"
                                          ++ "] of " ++ Cl.tomurphi otherType
  tomurphi (Set (Left size) otherType)   = "multiset[" ++ show size
                                          ++ "] of " ++ Cl.tomurphi otherType


-----------------------------------------------------------------


-- responses (in receive functions and self-issued rules)

instance Cl.MurphiClass Response where

 -- state :: String
 tomurphi (ToState machine state) = indexedMachineGen machine ++
                                    ".state := " ++ state ++ ";"


 -- Message :: Message MsgType [Maybe Field], src, dst :: Field
 tomurphi (Send (Message mtype params) src dst vc)
    = let
          -- if src and dst are explicit machines and not variables for machines,
          -- we need to print only the index of this machine,
          -- not the array indexed
          src1 = Cl.tomurphi $ onlyIndex src
          dst1 = Cl.tomurphi $ onlyIndex dst

          mtypeAssign = "newMsg.mtype := " ++ mtype ++ ";\n"
          srcAssign   = "newMsg.src   := "   ++ src1  ++ ";\n"
          dstAssign   = "newMsg.dst   := "   ++ dst1  ++ ";\n"
          vcAssign    = "newMsg.vc    := "    ++ vc    ++ ";\n"

          otherAssign = map msgParamAssign params

          setAllNewMsg = concat $ [mtypeAssign] ++ [srcAssign] ++
                                  [dstAssign]   ++ [vcAssign]  ++
                                  otherAssign
      in
          setAllNewMsg ++ "Send(newMsg);\n"


 -- the arguments of the broadcasting function are the msg we send
 -- this is constructed before the broadcast. We call this msg "newMsg".
 -- It's a local variable.
 tomurphi (Broadcast (Message mtype params) src dstSet vc)
   = let
          -- get the owner of the set and its name
          (Field (Simple setName) (Owner machine) ) = dstSet
          index = generalIndex machine

          -- if src is an explicit machine and not a variable for machine,
          -- we need to print only the index of this machine,
          -- not the array indexed
          src1 = Cl.tomurphi $ onlyIndex src

          -- constuct the msg to be sent
          mtypeAssign = "newMsg.mtype := "    ++ mtype ++ ";\n"
          srcAssign   = "newMsg.src   := "    ++ src1  ++ ";\n"
          vcAssign    = "newMsg.vc    := "    ++ vc    ++ ";\n"

          otherAssign = map msgParamAssign params

          setAllNewMsg = concat $ [mtypeAssign] ++ [srcAssign] ++
                                  [vcAssign]  ++
                                  otherAssign

     in
         setAllNewMsg ++
         "BroadcastTo" ++ fstCap setName ++
         "(newMsg," ++ index ++ ");\n"
-------------------------------------------------------

 tomurphi (Clear field) = "undefine " ++ Cl.tomurphi field ++ ";"

 tomurphi (Assign var value)    =
     -- check if var :: field has as owner Msg. If so, we should print
     -- newMsg. := etc. instead of msg. := etc.
     -- when a response assigns a value to a msg field, it is to send a
     -- msg. The message we send is newMsg. No changes are made to msg
   let (Field field owner) = var
       ownerIsMsg (Field f o) = (o == Msg)
       ownerIsMsg _ = False
   in  if (ownerIsMsg var) then "newMsg." ++ Cl.tomurphi field
                             ++ " := " ++ Cl.tomurphi value ++ ";"
       else Cl.tomurphi var ++ " := "
            ++ Cl.tomurphi value ++ ";"

 tomurphi (AssignInt field intExp)
    =
        let (Field var owner) = field
            ownerIsMsg (Field f o) = (o == Msg)
            ownerIsMsg _ = False
        in  if (ownerIsMsg field) then "newMsg." ++ Cl.tomurphi var
                                  ++ " := " ++ Cl.tomurphi intExp ++ ";"
            else  Cl.tomurphi field ++ " := "
                 ++ Cl.tomurphi intExp ++ ";"




 -- elem :: Field
 tomurphi (Add (Owner machine) setName elem)
   = let
         -- if elem is an explicit machine and not a variable for machine,
         -- we need to print only the index of this machine,
         -- not the array indexed
         elem1 = onlyIndex elem
     in
         "AddTo" ++ (fstCap.machineName) machine ++ fstCap setName ++ "List(" ++
         Cl.tomurphi elem1 ++ ", " ++ generalIndex machine ++ ");"

 tomurphi (Del (Owner machine) setName elem)
   = let
         -- if elem is an explicit machine and not a variable for machine,
         -- we need to print only the index of this machine,
         -- not the array indexed
         elem1 = onlyIndex elem
     in
         "RemoveFrom" ++ (fstCap.machineName) machine ++ fstCap setName ++ "List(" ++
         Cl.tomurphi elem1 ++ ", " ++ generalIndex machine ++ ");"

 tomurphi (Stall) = "return false;" -- message is not processed

 tomurphi (EmptyResp str) = "-- " ++ str -- e.g. "hit". Strings that don't
                                         -- mean anything in murphi


 --------------------------
 -- helper for broadcast and Send
 -- prints assignment to msg arg

msgParamAssign :: (FormalParam, Maybe Field) -> String
msgParamAssign (formal, actual)
  = "newMsg." ++ formal ++ " := " ++
     (if actual == Nothing then "undefined"
      else (Cl.tomurphi.fromJust) actual)
     ++ ";\n"

-----------------------------------------------------------------


instance Cl.MurphiClass Field where
 tomurphi (Field variable owner)          = Cl.tomurphi owner ++
                                            Cl.tomurphi variable
 tomurphi (JustIndex machine Nothing)     = formalIndexStr machine
 tomurphi (JustIndex machine (Just num))  = machine ++ show num
 tomurphi (SetSize setName owner)         = "MultisetCount(i:" ++
                                            Cl.tomurphi owner
                                            ++ setName ++ ", true" ++ ")"


-----------------------------------------------------------------


instance Cl.MurphiClass Owner where
  tomurphi Msg = "msg."
  tomurphi Global = ""
  tomurphi Local  = ""
  tomurphi ThisNode = "node."
  tomurphi (Owner machine) = indexedMachineGen machine ++ "."


----------------------------------------------------------------


instance Cl.MurphiClass Variable where
 tomurphi (Simple varName)             = varName
 tomurphi (ArrayElem arrayName index)  = arrayName ++ "[" ++ show index ++ "]"
 tomurphi (MachineArray machine)       = indexedFormalStr machine
 tomurphi (NonsymIndex machine index)  = machine ++ show index -- for nonsym


----------------------------------------------------------------


instance Cl.MurphiClass (Either Field Val) where
 tomurphi (Left field) = Cl.tomurphi field
 tomurphi (Right val)  = val


----------------------------------------------------------------


instance Cl.MurphiClass (Maybe Field) where
 tomurphi Nothing = "UNDEFINED"
 tomurphi (Just (field)) = Cl.tomurphi field


----------------------------------------------------------------


instance Cl.MurphiClass Guard where
  tomurphi (Receive mtype [] src vc)      =  guardMType mtype ++ guardMsgVC vc
                                             ++ guardMsgSrc src

  tomurphi (Receive mtype argVals src vc) = let  argConds = concat $
                                                             map printArgCond argVals
                                            in  guardMType mtype ++
                                                guardMsgSrc src ++
                                                guardMsgVC vc ++ argConds



  tomurphi (AtState machine state)
    = indexedMachineGen machine ++ ".state = " ++ state

  tomurphi (guard1 :&: guard2)
    = "(" ++ Cl.tomurphi guard1 ++ ")" ++ "\n&\n"
       ++ "(" ++ Cl.tomurphi guard2 ++ ")"

  tomurphi (guard1 :|: guard2)
    = "(" ++ Cl.tomurphi guard1 ++ ")" ++ "\n|\n"
       ++ "(" ++ Cl.tomurphi guard2 ++ ")"

  tomurphi (Not guard)
    = "(" ++ "!" ++ Cl.tomurphi guard ++ ")"

  -------------------------------------------

  tomurphi (Equals field1 (Left field2))
    =  "(" ++ Cl.tomurphi field1 ++ "=" ++ Cl.tomurphi field2 ++ ")"

  tomurphi (Equals field1 (Right intExp))
    =  "(" ++ Cl.tomurphi field1 ++ "=" ++ Cl.tomurphi intExp ++ ")"

  -------------------------------------------

  tomurphi (NotEq field1 (Left field2))
    = "(" ++ Cl.tomurphi field1 ++ "!=" ++ Cl.tomurphi field2 ++ ")"

  tomurphi (NotEq field1 (Right intExp))
    =  "(" ++ Cl.tomurphi field1 ++ "!=" ++ Cl.tomurphi intExp ++ ")"

  -------------------------------------------

  tomurphi (Greater field1 (Left field2))
    = Cl.tomurphi field1 ++ " > " ++ Cl.tomurphi field2

  tomurphi (Greater field (Right intExp))
    = Cl.tomurphi field ++ " > " ++ Cl.tomurphi intExp

  -------------------------------------------

  tomurphi (Less field1 (Left field2))
    = Cl.tomurphi field1 ++ " < " ++ Cl.tomurphi field2

  tomurphi (Less field (Right intExp))
    = Cl.tomurphi field ++ " <" ++ Cl.tomurphi intExp

  -------------------------------------------

  tomurphi (IsIn setField field)
    = let set     = Cl.tomurphi setField
          element = Cl.tomurphi field
      in  "(MultisetCount(i:" ++ set ++ ", " ++
           set ++ "[i]=" ++ element ++ ") > 0)"

  -------------------------------------------

-------------------------------------------------
-- helper functions for this section
-- (didnt work in where clause)

guardMType :: MType -> String
guardMType mtype = "msg.mtype = " ++ mtype

guardMsgVC :: Maybe VCName -> String
guardMsgVC (Nothing) = ""
guardMsgVC (Just vc) = " & msg.vc = " ++ vc

guardMsgSrc :: Maybe Field -> String
guardMsgSrc (Nothing) = ""
guardMsgSrc (Just src) = " & msg.src = " ++ Cl.tomurphi src

printArgCond :: (ArgName, Field) -> String
printArgCond (arg, field)  = " & msg." ++ arg ++ " = " ++ Cl.tomurphi field

-------------------------------------------------

----------------------------------------------------------------


-- print declarations of local variables
-- used in self-issued rules and machine receive functions
instance Cl.MurphiClass LocalVariables where
  tomurphi [] = ""
  tomurphi localVariables = pushBy 2 (mapconcatln Cl.tomurphi localVariables)


----------------------------------------------------------------


instance Cl.MurphiClass IntExp where
  tomurphi (Sum exp1 exp2)    = Cl.tomurphi exp1 ++ " + " ++ Cl.tomurphi exp2
  tomurphi (Minus exp1 exp2)  = Cl.tomurphi exp1 ++ " - " ++ Cl.tomurphi exp2
  tomurphi (Times exp1 exp2)  = Cl.tomurphi exp1 ++ " * " ++ Cl.tomurphi exp2
  tomurphi (Div   exp1 exp2)  = Cl.tomurphi exp1 ++ " / " ++ Cl.tomurphi exp2
  tomurphi (Group exp)        = "(" ++ Cl.tomurphi exp ++ ")"
  tomurphi (Const num)        = show num
  tomurphi (IntVar field)     = Cl.tomurphi field

-----------------------------------------------------------------
-----------------------------------------------------------------
