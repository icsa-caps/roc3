-----------------------------------------------------------------

-- this AST captures the subset of the murphi language
-- as it is used in the MSI protocol implementations
-- (at a first stage the MI implementation).
-- It is strongly based on the outline used in the above implementations

-----------------------------------------------------------------

module MurphiAST where

-----------------------------------------------------------------

-- simple type synonyms
type Size             = Int
type TypeName         = String
type Val              = String
type ArgName          = String
type VCName           = String
type NetName          = String
type Network          = Either OrderedNet UnorderedNet
type OrderedNetName   = String
type UnorderedNetName = String
type Name             = String
type State            = String
type MachineType      = String
type Rec              = String
type StateName        = String
type SetName          = String
type VarName          = String
type ArrayName        = String
type Index            = Int
type AliasName        = String
type RuleName         = String
type StartVal         = String
type MType            = String --mtype in murphi
type Responses        = [Response]
type FormalParam      = String




-- helper data structures, data types
type VCNets = [ (VCName, Network) ]


-----------------------------------------------------------------
-- for accessing a machine
-- WE DO NOT NEED THE SIZE OF THE MACHINES
-- we declare a constant equal to the size
-- of a machine. So we can refer to that.

data Machine = AnyType MachineType  -- don't know/don't care about symmetry
             | Sym MachineType
             | Nonsym MachineType NonsymMIndex
             | Synonym String
               deriving(Show,Eq)

-- we may refer to non-symmetric machines using a formal parameter
-- (e.g. in the receive function, a loop or a ruleset)
-- and we may refer to them also directly, which is also their purpose and
-- difference from symmetric machines
-- (e.g. sending a message to the second instance of the machine)
-- Arbitrary is for the former and Specific for the latter
data NonsymMIndex = Arbitrary
                  | Specific Index
                    deriving(Show,Eq)


data Symmetry = Symmetric | Nonsymmetric
                deriving(Show,Eq)

-- this datatype SHOULD NOT BE USED AT THE DECLARATION PARTS
-- i.e. not in Constants, Types and Variables
-- where we can use pairs of machine name and the other piece of
-- information we need

-----------------------------------------------------------------

-- only for declaring msg args; not used when sending or brodacasting messages.
-- see below for these
type MsgArg           = TypeDecl
type MsgArgs          = [MsgArg]




-- only for Send and Broadcast
data Message = Message MType [ (FormalParam, Maybe Field) ] -- for Owner see bellow
               deriving(Show,Eq)
-- The list of Message arguments has elements of type Field, instead of Variable,
-- with the owner set to Msg, in order to print them like "msg." ++ fieldName
-- if we use machine fields, local vars or constants the owner
-- is set accordingly

data Owner = Msg
           | Global           -- don't add "<smth>." in front (nums, consts)
           | Owner Machine    -- use indexedMachine when printing
           | Local            -- need to add declaration
           | ThisNode
             deriving(Show,Eq)


data Field = Field Variable Owner
           | JustIndex MachineType (Maybe Int)
           | SetSize SetName Owner
             deriving(Show,Eq)

-- Note: SimpleIndex is used when we want to print the index of a machine
-- instead of the array <machine name>State indexed.
-- This is the case for Send, Broadcast, addToSet, removeFromSet and AdvanceQ.
-- All these procedures in murphi take as argument the index of a machine.
-- In the translator, all these will take as arguments a Field
-- that corresponds to an indexed machine with constructor Field,
-- because this makes the translation easier. Then locally, at the
-- tomurphi implementation of each, we will transform that Field to SimpleIndex
-- if it is a machine.
-- The integer argument to the constructor is for nonsymetric machines.



-- not used anymore for broadcasting, may be used in responses (who knows)
--data SetField = SetField Field Type   -- Type is the type of the elements
--                deriving(Show,Eq)



type Src = Field
type Dst = Field


data Variable = Simple VarName
              | ArrayElem ArrayName Index
              | MachineArray MachineType -- machine array indexed by std index
              | NonsymIndex MachineType Index -- instance of nonsym machine
                deriving(Show,Eq)



-- for variables that are only in a particular function or procedure.
-- we need the list because we have to print their declarations
type LocalVariables = [TypeDecl]


-- in place of TypeDecl in Ast
-- to make printing easier the name is at the begining
data TypeDecl = Decl Name Type
                deriving(Show,Eq)


data Type = Boolean
          | Integer Lo Hi
          | Enum [Val]
          | Node MachineType -- printed in Types as machine index type
          | Array (Either Size MachineType) Type  -- if Machine, indexed by the
                                                  -- machine index, scalarset or enum
          | Set (Either Size MachineType) Type    -- Left machine means the size
            deriving(Show,Eq)                     -- of the set = #machines


type Lo    = Int
type Hi    = Int


-- data type for integer expressions

data IntExp = Sum   IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div   IntExp IntExp
            | Group IntExp
            | Const Int
            | IntVar Field  -- must find if it s local, machine field etc.
              deriving(Show,Eq)



-----------------------------------------------------------------
-----------------------------------------------------------------

data Program = Program Constants
                       Types
                       Variables
                       CommonFunctions
                       MachineFunctions
                       Rules
                       Startstate
                       Invariants
                deriving(Show,Eq)


-----------------------------------------------------------------
-----------------------------------------------------------------

-- Constants, Types and Variables should not use the Machine datatype,
-- but tuples that have the machine name and any other information necessary
-- for printing


-- Constants

data Constants = Constants {
                             machineSizesC :: [(MachineType,Size)],
                             vcs           :: [VCName]
                           }
                 deriving(Show,Eq)

-----------------------------------------------------------------
-----------------------------------------------------------------

-- Types

data Types  = Types
             {
                machinesSym :: [(MachineType, Size, Symmetry)], -- we need the size
                                                                -- to print enums

                msgType         :: [String], -- kinds of msgs (Ack, Fwd etc.)

                msgArgs         :: MsgArgs, -- only info we need to print msg

                machineStates   :: [ (MachineType,
                                    [State],       -- states of each machine
                                    [TypeDecl]) ]  -- fields of the machine
             }
             deriving(Show,Eq)


-----------------------------------------------------------------
-----------------------------------------------------------------

-- Variables

data Variables      = Variables {
                                  machines :: [MachineType],
                                  orderedNets :: [OrderedNetName],
                                  unorderedNets :: [UnorderedNetName]
                                 }
                                 deriving(Show,Eq)

data OrderedNet     = OrderedNet Name Size
                      deriving(Show,Eq)

data UnorderedNet   = UnorderedNet Name Size
                      deriving(Show,Eq)

netName :: Network -> String
netName (Left (OrderedNet name _))    = name
netName (Right (UnorderedNet name _)) = name


-----------------------------------------------------------------
-----------------------------------------------------------------

-- Common Functions

data CommonFunctions = CommonFunctions {
                        -- need one advanceQ for each ordered net!
                        advanceQ        :: [OrderedNetName],
                        send            :: ( MsgArgs, [(NetName,[VCName])] )
                       } deriving(Show,Eq)

type Param           = String

-----------------------------------------------------------------
-----------------------------------------------------------------

-- Machine Functions
-- for GetMachineFunctions.hs : need to extract local variables

data MachineFunctions = MachineFunctions [ ( MachineType,
                                             Sets,
                                             [BCastInfo],
                                             ReceiveFunction,
                                             LocalVariables ) ]
                        deriving(Show,Eq)

--------------------------------

-- we need add and remove functions for each set field a machine has
type Sets     = [TypeDecl] -- when printing we check TypeDecl is a set

type ElemType = Type

-- we have one bcast function for each
-- pair of msg and the set it is addressed to
-- [MsgArg] is list of std msg arguments
data BCastInfo = BCast MachineType SetName ElemType [MsgArg]
                 deriving(Show, Eq)

type ReceiveFunction = [ Reaction ]

type Reaction =( State, [ (Maybe Guard, [Response]) ] )

--------------------------------

data Guard           = Receive MType [(ArgName, Field)]
                               (Maybe Src) (Maybe VCName)
                     | AtState Machine State
                     | Equals Field (Either Field IntExp) -- F.Param maps to B.Field
                     | Greater Field (Either Field IntExp)
                     | Less Field (Either Field IntExp)
                     | NotEq Field (Either Field IntExp)
                     | Not Guard
                     | Guard :&: Guard
                     | Guard :|: Guard
                     | IsIn SetField Field
                       deriving(Show,Eq)

type SetField = Field

-- Note on Guard, Receive: the list is the value each msg arg must have (if any)
-- we use field for the values because they may be machine fields or local vars
-- in addition to constants. We don't mention any arguments for which we don't test
-- their values at the frontEnd (see GuardAssign constructor at front end)


data Response        = ToState Machine State
                     | Send Message Src Dst VCName    -- see note below for dst
                     | Broadcast Message Src DstSet VCName
                     | Clear Field
                     | Assign Field Field
                     | AssignInt Field IntExp
                     | Add Owner SetName Field  -- the owner of the set is the machine
                     | Del Owner SetName Field  -- in question. Owner here is  for the elem
                     | Stall
                     | EmptyResp String --e.g. hit. corresponds to F.EmptyResp
                       deriving(Show,Eq)

type Elem = Field
type DstSet = Field -- we assume the variable refers to a set


-----------------------------------------------------------------
-----------------------------------------------------------------

-- Rules
-- for GetRules.hs : need to find self issue rules,
-- extract guards

-- problem: at murphi, one of the responses in rules can be
-- "clear <field>" (in MSI it is acks).
-- just an optimization, no need to do it


data Rules = Rules SelfIssueRules
                   ReceiveOrdNets
                   ReceiveUnordNets
             deriving(Show,Eq)

type SelfIssueRules   = [( MachineType, [SelfIssueRule] )]
type ReceiveOrdNets   = [ReceiveOrdNet]
type ReceiveUnordNets = [ReceiveUnordNet]



-- the guard should (mostly) be AtStateAlias "node" <state>
-- self-issue rules are part of the machine description
-- the rulename is the self-issued msg i.e. what follows *
data SelfIssueRule    = SelfIssueRule RuleName LocalVariables Guard [Response]
                        deriving(Show,Eq)


-- we need a list of all the machines in the receive rules, because we
-- have IsMember calls for each of them
-- to check that the message is accepted by the machine it is sent to
data ReceiveOrdNet    = ReceiveOrdNet NetName [VCName] [MachineType]
                        deriving(Show,Eq)

data ReceiveUnordNet  = ReceiveUnordNet NetName [VCName] [MachineType]
                        deriving(Show,Eq)


-- we need the machines for isMember
-- maybe we could have them in a global variable

-----------------------------------------------------------------
-----------------------------------------------------------------

-- Startstate
data Startstate = Startstate {
                                machinesStart      ::  [(MachineType,
                                                         State,
                                                        [FieldStart])],
                                orderedNetsStart   ::  [OrderedNetName],
                                unorderedNetsStart :: [UnorderedNetName]
                             }
                  deriving(Show,Eq)

-- must support also arrays which are initialised with loops
-- the fields of a machien are of type TypeDecl (see Types section),
-- so the FieldStart should be the same
-- we must have ALL the fields of the machine
type FieldStart = (TypeDecl, Maybe StartVal)

-----------------------------------------------------------------
-----------------------------------------------------------------

-- Invariants
-- TODO
data Invariants = Invariants String
                  deriving(Show,Eq)



-----------------------------------------------------------------
-----------------------------------------------------------------
