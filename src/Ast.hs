
module Ast where

data Ast        = Model [ Node ]
                  deriving(Show)


data Node       = Node NodeType Int [ ( State, Guard, [Response] ) ]
                  deriving(Show)

type NodeType   = String

data State      = State String
                  deriving(Show)

data Guard      = Guard Mail
                  deriving(Show)

data Mail       = Issue Msg
                |Send Msg NodeType
                | Receive Msg
                | ReceiveFrom Msg NodeType
                  deriving(Show)


data Msg        = Msg String
                | MsgWithArg String NodeType
                  deriving(Show)


data Response   = Response Mail
                | Trans State
                | Update Assignment
                | Other String
                  deriving(Show)


data Assignment = Var VarName String
                | VarNum VarName Int
                deriving(Show)

type VarName = String
