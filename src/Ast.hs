
module Ast where

data Ast        = Model [ Machine ]
                  deriving(Show)


data Machine    = Machine MachineType Int Fields [ ( State, Guard, Maybe State, [Response] ) ]
                  deriving(Show)

data Fields     = Fields [String]
                  deriving(Show)

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
