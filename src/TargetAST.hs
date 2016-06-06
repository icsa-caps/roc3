
module TargetAst where

import Ast.hs -- frontend AST

-- this AST captures the subset of the murphi language
-- as it is used in the MSI protocol implementations
-- (at a first stage the MI implementation).
-- It is strongly based on the outline used in the above implementations


-- capital T at the end of datatypes is used to differentiate from
-- those with the same name in Ast.hs and stands for Target


-- simple type synonyms
type Machine    = String
type Size       = Int
type Type       = String
type Val        = String
type Channel    = String
type MsgNet     = Either OrderedNet UnorderedNet
type Name       = String

-- helper data structures
type ChanelNets = [ (Channel, MsgNet) ]


data Program = Program Constants
                       Types
                       Variables
                       CommonFunctions
                       MachinesFunctions
                       Rules
                       Startstate
                       Invariants

-----------------------------------------------------------------
-- Constants


type Constants = {
                   machineSizes :: [Int],
                   netSizes     :: [Int]
                  }


-----------------------------------------------------------------
-- Types
data Types  = Types
             {
                machines       :: [(Machine,Size)],   -- the scalarsets for indexing

                nodes          :: [Machines],         -- union of the machines

                msgType        :: MsgType,            -- all the types Msgs can have
                                                      -- in murphi enum
                message        :: Message,

                machineStates  :: [ (machine,         -- records in murphi
                                    [state],          --
                                    [field]) ]        --
             }

type MsgType = [ String ] -- diff kinds of msgs


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
                                  machines :: [Machine],
                                  orderedNets :: [OrderedNet]
                                  unorderedNets :: [UnorderedNet]
                                 }


data OrderedNet     = OrderedNet Name Size
data UnorderedNet   = UnorderedNet Name Size


-----------------------------------------------------------------
-- Common Functions

CommonFunctions = FunctionParameters
                {
                  advanceFWDQ :: [OrderedNetsName]
                  send        :: ( [(Param,Type)], -- extract from Message
                                    [Channel]     -- VCs with ordered nets
                                 )
                }

type OrderedNetsName = String

-----------------------------------------------------------------
-- Receive Functions

type MachineFunctions = [ ( Machine, ReceiveFunction ) ]

type ReceiveFunction = [ ( State, GuardT, [Respond] ) ]

data GuardT = ReceiveT Msg From   -- the src may be important
            | AtState State

data Respond = ToState State
             | Send MsgStr [(MsgArgType,Arg)] Rec
             | Assign Var Val

-----------------------------------------------------------------
-- Rules
-- problem: at murphi, one of the responses in rules is
-- "clear <field>" (in MSI it's acks).
-- I don't know how we can find which fields we need to clear


data Rules = Rules { SelfIssueRules   :: [SelfIssueRule],
                     ReceiveOrdNets   :: [ReceiveOrdNet],
                     ReceiveUnordNets :: [ReceiveUnordNet] }

data SelfIssueRule = SelfIssueRule GuardT [Respond] -- GuardT is a function of
                                                    -- the guard in the relevant
                                                    -- part of the front-end

data ReceiveOrdNet    = ReceiveOrdNet NetName [Channel] [Machine]

data ReceiveUnordNet  = ReceiveUnordNet NetName [Channel] [Machine]


type NetName = String
-- we need the machines for isMember
-- maybe we could have them in a global variable

-----------------------------------------------------------------
-- Startstate






-----------------------------------------------------------------
-- Invariants




-----------------------------------------------------------------
