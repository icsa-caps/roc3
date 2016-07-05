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


-- responses (in receive functions)

instance Cl.MurphiClass Response where

 -- state :: String
 tomurphi (ToState machine state) = indexedMachineGen machine ++
                                    ".state = " ++ state ++ ";"


 -- Message :: Message MsgType [Maybe Field], src, dst :: Field
 tomurphi (Send (Message mtype params) src dst vc)
    = "Send(" ++ Cl.tomurphi src ++ ",\n" ++
      "     " ++ Cl.tomurphi dst ++ ",\n" ++
      "     " ++ vc         ++ ",\n" ++
      pushBy 5 (mapconcatlnComma Cl.tomurphi params) ++ ");"


 tomurphi (Assign var value)    = Cl.tomurphi var ++ " := "
                                  ++ Cl.tomurphi value ++ ";"

 tomurphi (AssignInt field intExp) = Cl.tomurphi field ++ " := "
                                     ++ Cl.tomurphi intExp ++ ";"


 -- elem :: Field
 tomurphi (Add (Owner machine) setName elem)
   = "AddTo" ++ indexedMachineGen machine ++ fstCap setName ++ "List(" ++
     Cl.tomurphi elem ++ ", " ++ generalIndex machine ++ ");"

 tomurphi (Del (Owner machine) setName elem)
   = "RemoveFrom" ++ indexedMachineGen machine ++ fstCap setName ++ "List(" ++
     Cl.tomurphi elem ++ ", " ++ generalIndex machine ++ ");"

 tomurphi (Stall) = "return false;" -- message is not processed


-----------------------------------------------------------------


instance Cl.MurphiClass Field where
 tomurphi (Field variable owner) = Cl.tomurphi owner ++ Cl.tomurphi variable


-----------------------------------------------------------------


instance Cl.MurphiClass Owner where
  tomurphi Msg = "msg."
  tomurphi Global = ""
  tomurphi Local  = ""
  tomurphi ThisNode = "node."
  tomurphi (Owner machine) = indexedMachineGen machine ++ "."


----------------------------------------------------------------


instance Cl.MurphiClass Variable where
 tomurphi (Simple varName) = varName
 tomurphi (ArrayElem arrayName index) = arrayName ++ "[" ++ show index ++ "]"
 tomurphi (MachineArray machine) = indexedMachineGen machine
 tomurphi (MachineIndex machine) = generalIndex machine


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
  tomurphi (Receive mtype [] vc)      =  printMType mtype ++ printMsgVC vc
  tomurphi (Receive mtype argVals vc) = let temp = map printArgCond argVals
                                            argConds = concatWith " & " temp
                                        in  printMType mtype ++ " & " ++
                                            argConds ++ printMsgVC vc

  tomurphi (AtState machine state)
    = indexedMachineGen machine ++ ".state = " ++ state

-------------------------------------------------
-- helper functions for this section
-- (didnt work in where clause)

printMType :: MType -> String
printMType mtype = "msg.mtype = " ++ mtype

printMsgVC :: Maybe VCName -> String
printMsgVc (Nothing) = ""
printMsgVC (Just vc) = " & msg.vc = " ++ vc

printArgCond :: (ArgName, (Either Field Val)) -> String
printArgCond (arg, (Right const)) = "msg." ++ arg ++ "=" ++ const
printArgCond (arg, (Left field))  = "msg." ++ arg ++ "=" ++ Cl.tomurphi field

-------------------------------------------------

----------------------------------------------------------------


-- print declarations of local variables
instance Cl.MurphiClass LocalVariables where
  tomurphi [] = ""
  tomurphi localVariables = "var\n" ++
                            pushBy 2 (mapconcatln Cl.tomurphi localVariables)


----------------------------------------------------------------


instance Cl.MurphiClass SelfIssueRule where
  tomurphi (SelfIssueRule rulename guard responses)
    = "rule \"" ++ rulename ++ "\"\n" ++
      pushBy 2 (Cl.tomurphi guard) ++ "\n" ++
      "=>\n" ++
      pushBy 2 (mapconcatln Cl.tomurphi responses) ++ "\n" ++
      "endrule;"


-----------------------------------------------------------------

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
