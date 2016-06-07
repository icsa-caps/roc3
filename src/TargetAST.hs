
module TargetAst where

import Ast -- frontend AST

-- this AST captures the subset of the murphi language
-- as it is used in the MSI protocol implementations
-- (at a first stage the MI implementation).
-- It is strongly based on the outline used in the above implementations


-- capital T at the end of datatypes is used to differentiate from
-- those with the same name in Ast.hs and stands for Target
-- Alas, we use some of the types from Ast.hs


-- simple type synonyms
type MachineT     = String
type Size         = Int
type Type         = String
type Val          = String
type Var          = String
type ChannelName  = String
type MsgNet       = Either OrderedNet UnorderedNet
type Name         = String

-- helper data structures
type ChanelNets = [ (ChannelName, MsgNet) ]


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
                             machineSizes :: [Int],
                             netSizes     :: [Int]
                           }
                deriving(Show)

-----------------------------------------------------------------
-- Types
data Types  = Types
             {
                machinesInd    :: [(MachineType,Size)],   -- the scalarsets for indexing

                nodes          :: [MachineType],         -- union of the machines

                msgType        :: [String],

                message        :: MsgArgs, -- only info we need to print msg

                machineStates  :: [ (MachineType,
                                    [State],
                                    [Field]) ]
             }
             deriving(Show)



-- reason for introducing Message (in place of Msg from frontend) :
-- it encaptures the same information, only that we know what kind of arguments
-- messages don't have
-- not needed for the time being
--data Message = Message MsgType [ (Type, Maybe Val) ]

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

-----------------------------------------------------------------
-- Common Functions

data CommonFunctions = FunctionParameters {
                        advanceQ    :: [OrderedNetsName],
                        send        :: ( [(Param,Type)], [ChannelName] )
                       } deriving(Show)

type OrderedNetsName = String
type Param           = String

-----------------------------------------------------------------
-- Receive Functions

type MachineFunctions = [ ( MachineType, ReceiveFunction ) ]

type ReceiveFunction = [ ( State, GuardT, [Respond] ) ]

data GuardT = ReceiveT MType   -- the src may be important
            | AtState State
            deriving(Show)

type MType = String --mtype in murphi

data Respond = ToState State
             | Send MsgStr [(ArgType,Arg)] Rec
             | Assign Var Val
             deriving(Show)

type MsgStr  = String
type ArgType = String
type Arg     = String
-----------------------------------------------------------------
-- Rules
-- problem: at murphi, one of the responses in rules can be
-- "clear <field>" (in MSI it's acks).
-- I don't know how we can find which fields we need to clear


data Rules = Rules { selfIssueRules   :: [SelfIssueRule],
                     receiveOrdNets   :: [ReceiveOrdNet],
                     receiveUnordNets :: [ReceiveUnordNet] }
                     deriving(Show)


data SelfIssueRule = SelfIssueRule GuardT [Respond] -- GuardT is a function of
                    deriving(Show)                  -- the guard in the relevant
                                                    -- part of the front-end

data ReceiveOrdNet    = ReceiveOrdNet NetName [ChannelName] [MachineT]
                        deriving(Show)

data ReceiveUnordNet  = ReceiveUnordNet NetName [ChannelName] [MachineT]
                        deriving(Show)


type NetName = String
-- we need the machines for isMember
-- maybe we could have them in a global variable

-----------------------------------------------------------------
-- Startstate
-- TODO
type Startstate = String





-----------------------------------------------------------------
-- Invariants
-- TODO
type Invariants = String



-----------------------------------------------------------------
