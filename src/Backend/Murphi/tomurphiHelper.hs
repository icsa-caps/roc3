-----------------------------------------------------------------
--------------- helper tomurphi implementations -----------------
-----------------------------------------------------------------

module tomurphiHelper where

--------------------------------

import MurphiAST
import MurphiClass

--------------------------------

-----------------------------------------------------------------
-----------------------------------------------------------------


instance Cl.MurphiClass Machine where
  tomurphi :: Machine -> String
  tomurphi (Machine machineType machineIndex ) = machineType ++
                                                "[" ++ machineIndex ++ "]"

  tomurphi (Alias alias)                       = alias

  tomurphi (Nonsym machineType num)       = machineType ++ show num


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
 tomurphi (ToState machine state) = Cl.tomurphi machine ++
                                    ".state = " ++ state ++ ";"


 -- Message :: Message MsgType [Maybe Field], src, dst :: Field
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


-----------------------------------------------------------------


instance Cl.MurphiClass Field where
 tomurphi (Field variable owner) = Cl.tomurphi owner ++ Cl.tomurphi variable


-----------------------------------------------------------------


instance Cl.MurphiClass Owner where
  tomurphi Msg = "msg."
  tomurphi Global = ""
  tomurphi Local  = ""
  tomurphi ThisNode = "node."
  tomurphi (Owner machine) = Cl.tomurphi machine ++ "."


----------------------------------------------------------------


instance Cl.MurphiClass Variable where
 tomurphi (Simple varName) = varName
 tomurphi (ArrayElem arrayName index) = arrayName ++ "[" ++ show index ++ "]"
 tomurphi (MachineArray machine) = Cl.tomurphi machine


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
 tomurphi (AtState machine state)  = Cl.tomurphi machine ++ ".state = " ++ state


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
