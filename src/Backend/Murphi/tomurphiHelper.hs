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


-----------------------------------------------------------------


-- responses (in receive functions)

instance Cl.MurphiClass Response where

 -- state :: String
 tomurphi (ToState machine state) = indexedMachineGen machine ++
                                    ".state = " ++ state ++ ";"


 -- Message :: Message MsgType [Maybe Field], src, dst :: Field
 tomurphi (Send (Message mtype params) src dst)
    = "Send(" ++ Cl.tomurphi src ++ ",\n" ++
      "     " ++ Cl.tomurphi dst ++ ",\n" ++
      pushBy 5 (mapconcatlnComma Cl.tomurphi params) ++ ");"


 tomurphi (Assign var value)    = Cl.tomurphi var ++ " := " ++ Cl.tomurphi value ++ ";"


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
 tomurphi (Receive mtype) = "msg.mtype = " ++ mtype
 tomurphi (AtState machine state)  = indexedMachineGen machine ++ ".state = " ++ state


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
