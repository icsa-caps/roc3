
module Ast where

data Ast        = Model {
                          globals  :: [ TypeDecl],
                          channels :: [ Channel ],
                          networks :: [ Network ],
                          machines :: [ Machine ]
                        }
                  deriving(Show)


data Channel = Channel String
               deriving(Show)

data Network = Network Order String [Channel]
               deriving(Show)

data Order = Ord | Unord
            deriving(Show,Enum)



data Machine    = Machine {
                            machineType :: String,
                            size :: Int,
                            fields :: [Field],
                            machineFunction ::  [ ( State, Guard, Maybe State, [Response] ) ]
                           }
                  deriving(Show)


type Fields     = [Field]

type Field      = TypeDecl

-- Field and MsgArg are both TypeDecl

data TypeDecl   = Boolean VarName
                | Integer VarName Lo Hi
                | Enum VarName [String]
                | Node MachineType VarName -- TODO: check MachineType is one of the machines
                | Array Size TypeDecl
                | Map MachineType TypeDecl
                | SetNum Size TypeDecl
                | SetName MachineType TypeDecl
                deriving(Show)

type MachineType = String
type Lo = Int
type Hi  = Int
type Size = Int

data State  = State String
              deriving(Show)


-- problem with guard: the constructor allows an arbitrary
-- mail to be a guard, while it is only incoming messages
-- (i.e. receives) that can act as guards.
-- possible fix: keep the front-end AST as it is,
-- but check when transforming it to the target AST
-- that any mails used as guards are receives
-- and throw an informing error if otherwise

data Guard       = Guard Mail     -- extend with arbitrary guards
                  deriving(Show)

data Mail        = Issue Msg
                 | Send Msg Dst
                 | ReceiveFrom Msg Src
                  deriving(Show)


type Dst = String
type Src = Maybe String


data Msg     = Msg String MsgArgs
               deriving(Show)


type MsgArgs = [MsgArg]

type MsgArg  = TypeDecl


-- Response has a similar problem with Guard:
-- it allows arbitrary msgs to be responses,
-- while a machine can't repond with a receive
-- nor with an issue -- that's a limitation of murphi:
-- you are not allowed to fire a rule on demand
-- e.g. in a function body

data Response   = Response Mail
                | Update Assignment
                | SelfIssue String -- will be ignored in the Target AST see mi.c3
                | Add SetName String
                | Del SetName String
                | Stall
                  deriving(Show)

type SetName = String

data Assignment = Var VarName String
                | VarNum VarName Int
                  deriving(Show)

type VarName = String
