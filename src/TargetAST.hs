
module TargetAST where

import qualified Ast as Front-- frontend AST

-- this AST captures the subset of the murphi language
-- as it is used in the MSI protocol implementations
-- (at a first stage the MI implementation).
-- It is strongly based on the outline used in the above implementations


-- capital T at the end of datatypes is used to differentiate from
-- those with the same name in Ast.hs and stands for Target
-- Alas, we use some of the types from Ast.hs


-- simple type synonyms
type Size         = Int
type TypeName     = String
type Val          = String
type Var          = String
type VCName       = String
type NetName      = String
type Network      = Either OrderedNet UnorderedNet
type Name         = String
type State        = String
type MachineType  = String
type Rec          = String
type MsgType      = String
type StateName    = String
type MsgArg       = TypeDecl

-- helper data structures
type VCNets = [ (VCName, Network) ]

-- for Send
data Message = Message MsgType [ (TypeName, Maybe Val) ]
             deriving(Show)

-- in place of TypeDecl in Ast, to make printing easier
data TypeDecl = Decl Name Type
              deriving(Show)

data Type = Boolean
          | Integer Lo Hi
          | Enum [Val]
          | Array Index Type
          deriving(Show)

type Lo    = Int
type Hi    = Int
type Index = String

-- at Murphi, printing function
-- at Transform, function from Front.TypeDecl to Back.TypeDecl



-----------------------------------------------------------------
data Program = Program Constants
                       Types
                       Variables
                       CommonFunctions
                       MachineFunctions
                       Rules
                       Startstate
                       Invariants
                deriving(Show)

-----------------------------------------------------------------
-- Constants


data Constants = Constants {
                             machineSizesC :: [(MachineType,Size)],
                             vcs          :: [VCName]
                           }
                deriving(Show)

-----------------------------------------------------------------
-- Types
data Types  = Types
             {
                machineSizesT    :: [(MachineType,Size)],   -- the scalarsets for indexing

                nodes           :: [MachineType],         -- union of the machines

                vcType          :: Size,

                msgType         :: [String],

                msgArgs         :: [MsgArg], -- only info we need to print msg

                machineStates   :: [ (MachineType,
                                    [State],
                                    [TypeDecl]) ]
             }
             deriving(Show)



-- reason for introducing Message (in place of Msg from frontend) :
-- it encaptures the same information, only that we know what kind of arguments
-- messages don't have
-- not needed for the time being


-- the list is a list of arguments of the message (type of argument and value).
-- if a message doesn't have an argument of some time the value is nothing

-----------------------------------------------------------------
-- Variables

data Variables      = Variables {
                                  machines :: [MachineType],
                                  orderedNets :: [OrderedNet],
                                  unorderedNets :: [UnorderedNet]
                                 }
                                 deriving(Show)

data OrderedNet     = OrderedNet Name Size
                      deriving(Show)
data UnorderedNet   = UnorderedNet Name Size
                      deriving(Show)

netName :: Network -> String
netName (Left (OrderedNet name _))    = name
netName (Right (UnorderedNet name _)) = name



-----------------------------------------------------------------

-- Common Functions

data CommonFunctions = FunctionParameters {
                        advanceQ    :: [OrderedNetsName],
                        send        :: ( [(Param,TypeName)], [VCName] )
                       } deriving(Show)

type OrderedNetsName = String
type Param           = String

-----------------------------------------------------------------

-- Receive Functions

data MachineFunctions = MachineFunctions [ ( MachineType, ReceiveFunction ) ]
                      deriving(Show)

type ReceiveFunction = [ ( State, Guard, [Respond] ) ]

data Guard = Receive MType   -- the src may be important
            | AtState State
            deriving(Show)

type MType = String --mtype in murphi

data Respond = ToState State
             | Send Message Dst
             | Assign Var Val
             deriving(Show)

type Dst = String
-----------------------------------------------------------------

-- Rules

-- problem: at murphi, one of the responses in rules can be
-- "clear <field>" (in MSI it's acks).
-- just an optimization, no need to do it


data Rules = Rules { selfIssueRules   :: [SelfIssueRule],
                     receiveOrdNets   :: [ReceiveOrdNet],
                     receiveUnordNets :: [ReceiveUnordNet] }
                     deriving(Show)


data SelfIssueRule = SelfIssueRule Guard [Respond] -- Guard is a function of
                    deriving(Show)                  -- the guard in the relevant
                                                    -- part of the front-end

data ReceiveOrdNet    = ReceiveOrdNet NetName [VCName] [MachineType]
                        deriving(Show)

data ReceiveUnordNet  = ReceiveUnordNet NetName [VCName] [MachineType]
                        deriving(Show)


-- we need the machines for isMember
-- maybe we could have them in a global variable

-----------------------------------------------------------------

-- Startstate
-- TODO
data Startstate = Startstate String
                deriving(Show)





-----------------------------------------------------------------

-- Invariants
-- TODO
data Invariants = Invariants String
                  deriving(Show)



-----------------------------------------------------------------
