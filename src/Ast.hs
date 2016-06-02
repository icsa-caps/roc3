
module Ast where

data Ast        = Model [ Machine ]
                  deriving(Show)


data Machine    = Machine MachineType Int Fields MachineFunction
                  deriving(Show)

type MachineFunction = [ ( State, Guard, Maybe State, [Response] ) ]

type Fields     = [Field]

data Field      = Boolean VarName
                | Integer VarName Low Hi
                | Enum VarName [String]
                | Node MachineType VarName -- TODO: check MachineType is one of the machines
                | Array Field
                deriving(Show)

type Low = Int
type Hi  = Int

type MachineType = String

data State       = State String
                  deriving(Show)

data Guard       = Guard Mail
                  deriving(Show)

data Mail        = Issue Msg
                 | Send Msg Rec
                 | Receive Msg
                 | ReceiveFrom Msg Src
                  deriving(Show)

type Rec = String
type Src = String


data Msg        = Msg String
                | MsgWithArg String MachineType
                  deriving(Show)


data Response   = Response Mail
                | Update Assignment
                | Other String
                  deriving(Show)


data Assignment = Var VarName String
                | VarNum VarName Int
                  deriving(Show)

type VarName = String
