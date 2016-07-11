
module Ast where

-- type synonyms
type Fields          = [Field]
type MachineType     = String
type Name            = String
type Size            = Int
type Dst             = Param
type Src             = Param
type DstSet          = Param
type MType           = String
type MsgArgs         = [MsgArg]
type SetName         = String
type VarName         = String
type Index           = Int
type StartVal        = String
type MachineFCase    = ( State, Guard,  Maybe State, [Response] )
type MachineFunction = [MachineFCase]
type ArrayName       = String


data Ast = Model {
                   -- globals  :: [ TypeDecl], not suppported in backend
                   -- we can add it back easily in the front-end
                   networks :: [ Network ],
                   machines :: [ Machine ]
                 }
                 deriving(Show,Eq)


data VC = VC String
          deriving(Show,Eq)

data Network = Network Order Name [VC]
               deriving(Show,Eq)

data Order = Ord | Unord
             deriving(Show,Enum,Eq)

data Symmetry = Symmetric | Nonsymmetric
                deriving(Show,Enum,Eq)


data Machine  = Machine {
                            symmetry        :: Symmetry,
                            machineType     :: String,
                            size            :: Int,
                            startstate      :: State,
                            fields          :: [Field],
                            machineFunction :: MachineFunction
                        }
                deriving(Show,Eq)

data Field = Field TypeDecl (Maybe StartVal)
             deriving(Show,Eq)


-- Field and MsgArg are both TypeDecl

data TypeDecl   = Boolean VarName
                | Integer VarName Lo Hi
                | Enum VarName [String]
                | Vertex MachineType VarName -- TODO: check MachineType is one of the machines
                | Array (Either Size MachineType) TypeDecl
                | Set   (Either Size MachineType) TypeDecl
                  deriving(Show,Eq)

type Lo = Int
type Hi  = Int

data State  = State String
              deriving(Show,Eq)


-- problem with guard: the constructor allows an arbitrary
-- mail to be a guard, while it is only incoming messages
-- (i.e. receives) that can act as guards.
-- possible fix: keep the front-end AST as it is,
-- but check when transforming it to the target AST
-- that any mails used as guards are receives
-- and throw an informing error if otherwise

data Guard       = ReceiveFrom Msg (Maybe Src) (Maybe VC) -- Nothing if we do not care about the vc     -- extend with arbitrary guards
                 | Issue String
                   deriving(Show,Eq)


data Msg         = Msg MType MsgArgs
                   deriving(Show,Eq)


data MsgArg      = GuardAssign TypeDecl VarName
                 | MsgArg TypeDecl
                   deriving(Show,Eq)

-- Note: for the first constructor, we either assign a value
-- to the message argument (if in Send) or we check the argument for a value
-- (in Receive). When transforming the AST we figure out which of the two.
-- the second constructor is used when we want neither of the above.


-- Response has a similar problem with Guard:
-- it allows arbitrary msgs to be responses,
-- while a machine can't repond with a receive
-- nor with an issue -- that's a limitation of murphi:
-- you are not allowed to fire a rule on demand
-- e.g. in a function body

data Response   = Send Msg Dst VC
                | Broadcast Src DstSet Msg VC
                | EmptyResp String  -- e.g. "hit". Ignore in target AST
                | Add SetName (Either Param Int)
                | Del SetName (Either Param Int)
                | Stall
                | Assign Param Param
                | AssignNum Param IntExp
                | AssignLocal TypeDecl Param
                | AssignLocalNum TypeDecl IntExp
                  deriving(Show,Eq)


data Param      = ArrayElem ArrayName Index
                | NonSymInst MachineType Index -- for nonsymmetric machines
                | VarOrVal String -- local, global, field, value
                  deriving(Show,Eq)

-- for transforming the AST:
-- when we have a Variable VarName, we search the list of fields of the machine
-- we are in
-- (all this takes place only in responses, inside a machine receive function)
-- if it is not there, it is either a local variable, that needs to be declared
-- or a constant. All the possible constants that are strings are true/false
-- and enum values. So we keep a list of these and if the string is not here
-- either, it is a local variable.

-- if we have global variables, we can add a declaration "globlal" before the
-- variable when we make the assignment or better search first in the list
-- of globals


data IntExp = Sum   IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div   IntExp IntExp
            | Group IntExp
            | Const Int
            | IntVar VarName  -- must find if it s local, machine field etc.
              deriving(Show,Eq)




-----------------------------------------------------------------
-----------------------------------------------------------------

-- vim: set ft=haskell
