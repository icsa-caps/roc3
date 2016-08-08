
module Ast where

-- type synonyms
type Fields          = [Field]
type MachineType     = String
type Name            = String
type Size            = Int
type Dst             = Param
type Src             = Param
type DstSet          = Param    -- it's a simple identifier
type MType           = String
type MsgArgs         = [MsgArg]
type SetName         = String
type VarName         = String
type Index           = Int
type StartVal        = String
type MachineFCase    = ( State, Guard,  Maybe State, [Response] )
type MachineFunction = [MachineFCase]
type ArrayName       = String
type Responses       = [Response]


data Ast = Model {
                   -- globals  :: [TypeDecl], not suppported in backend
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



data Guard       = ReceiveFrom Msg (Maybe Src) (Maybe VC) -- Nothing if we do not care about the vc     -- extend with arbitrary guards
                 | Issue String
                 | Equals Param (Either Param IntExp)
                 | Greater Param (Either Param IntExp)
                 | Less Param (Either Param IntExp)
                 | NotEq Param (Either Param IntExp)
                 | Not Guard
                 | Guard :&: Guard
                 | Guard :|: Guard
                   deriving(Show,Eq)


data Msg         = Msg MType [MsgArg]
                   deriving(Show,Eq)


data MsgArg      = GuardAssign TypeDecl Param  -- see note below
                 | MsgArg TypeDecl
                   deriving(Show,Eq)


-- Note: for the first constructor, we either assign a value
-- to the message argument (if in Send/Broadcast)
-- or we check the argument for a value (in Receive).
-- When transforming the AST we figure out which of the two.
-- the second constructor is used when sending/broadcasting a message and
-- the argument of the message has the same name with the formal parameter,
-- or when receiving a message and we don't check the message arguments



-- Response has a similar problem with Guard:
-- it allows arbitrary msgs to be responses,
-- while a machine can't repond with a receive
-- nor with an issue -- that's a limitation of murphi:
-- you are not allowed to fire a rule on demand
-- e.g. in a function body

data Response   = Send Msg Dst VC -- functions also as broadcast, depending on dst
                | EmptyResp String  -- e.g. "hit". Ignore in target AST
                | Add SetName (Either Param Int)
                | Del SetName (Either Param Int)
                | Clear Name
                | Stall
                | Assign Param Param
                | AssignNum Param IntExp
                | AssignLocal TypeDecl Param
                | AssignLocalNum TypeDecl IntExp
                  deriving(Show,Eq)

-- Note on clear: because we may want to clear a composite type,
-- we can't use a parameter as argument to Clear. Param models
-- simple types, that can be passed as arguments to functions.


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
            | IntVar Param  -- must find if it s local, machine field etc.
              deriving(Show,Eq)




-----------------------------------------------------------------
-----------------------------------------------------------------

-- vim: set ft=haskell
