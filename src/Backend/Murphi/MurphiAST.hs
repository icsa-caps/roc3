
module MurphiAST where

-- this AST captures the subset of the murphi language
-- as it is used in the MSI protocol implementations
-- (at a first stage the MI implementation).
-- It is strongly based on the outline used in the above implementations


-- simple type synonyms
type Size             = Int
type TypeName         = String
type Val              = String
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
type MsgArg           = TypeDecl
type MsgArgs          = [MsgArg]
type VarName          = String
type ArrayName        = String
type Index            = Int
type AliasName        = String
type RuleName         = String
type MachineIndex     = String
type StartVal         = String


-- helper data structures, data types
type VCNets = [ (VCName, Network) ]



-- for accessing a machine
-- WE DO NOT NEED THE SIZE OF THE MACHINES
-- we declare a constant equal to the size
-- of a machine. So we can refer to that.

data Machine = Sym MachineType
             | Nonsym MachineType NonsymMIndex
             | Synonym String
               deriving(Show)

-- we may refer to non-symmetric machines using a formal parameter
-- (e.g. in the receive function, a loop or a ruleset)
-- and we may refer to them also directly, which is also their purpose and
-- difference from symmetric machines
-- (e.g. sending a message to the second instance of the machine)
-- Arbitrary is for the former and Specific for the latter
data NonsymMIndex = Arbitrary
                  | Specific Index
                    deriving(Show)


data Symmetry = Symmetric | Nonsymmetric
                deriving(Show)

-- this datatype SHOULD NOT BE USED AT THE DECLARATION PARTS
-- i.e. not in Constants, Types and Variables
-- where we can use pairs of machine name and the other piece of
-- information we need

-----------------------------------------------------------------

-- only for Send and Broadcast
data Message = Message MType [ Maybe Field ] -- for Owner see bellow
               deriving(Show)


data Owner = Msg
           | Global
           | Owner Machine    -- use indexedMachine when printing
           | Local            -- need to add declaration
           | ThisNode
             deriving(Show)


-- change fields to account for elements of arrays i.e. spesific machines
data Field = Field Variable Owner
             deriving(Show)

-- for printing broadcasting functions
data SetField = SetField Field Type   -- Type is the type of the elements
                deriving(Show)

type Src = Field
type Dst = Field


data Variable = Simple VarName
              | ArrayElem ArrayName Index
              | MachineArray Machine
              | MachineIndex Machine
                deriving(Show)



-- for variables that are only in a particular function or procedure.
-- we need the list because we have to print their declarations
type LocalVariables = [TypeDecl]


-- in place of TypeDecl in Ast, to make printing easier
data TypeDecl = Decl Name Type
              deriving(Show)


data Type = Boolean
          | Integer Lo Hi
          | Enum [Val]
          | Node MachineType
          | Array Index Type
          | Set (Either MachineType Size) Type -- Left machine means the size
          deriving(Show)                       -- of the set = #machines


type Lo    = Int
type Hi    = Int


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
                deriving(Show)


-----------------------------------------------------------------
-----------------------------------------------------------------

-- Constants, Types and Variables should not use the Machine datatype,
-- but tuples that have the machine name and any other information necessary
-- for printing


-- Constants


data Constants = Constants {
                             machineSizesC :: [(MachineType,Size)],
                             vcs          :: [VCName]
                           }
                deriving(Show)

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
             deriving(Show)


-----------------------------------------------------------------
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
-----------------------------------------------------------------

-- Common Functions
data CommonFunctions = FuncParams {
                        -- need one advanceQ for each ordered net!
                        advanceQ        :: [OrderedNetName],
                        send            :: ( MsgArgs, [(NetName,[VCName])] ),
                        -- we assume the field is a set
                        -- one broadcasting function for each pair
                        -- of set of fields and a message
                        broadcast       :: [ (SetField, Message) ]
                        -- MSI has also a BCastInv procedure

                       } deriving(Show)

type Param           = String

-----------------------------------------------------------------
-----------------------------------------------------------------

-- Machine Functions

data MachineFunctions = MachineFunctions [ ( MachineType,
                                             Sets,
                                             ReceiveFunction,
                                             LocalVariables ) ]
                        deriving(Show)

-- we need a pair of add and remove functions for each set field a machine has
type Sets            = [TypeDecl] -- in MurphiPrint we check TypeDecl is a set

type ReceiveFunction = [ ( State, [ (Maybe Guard, [Response]) ] ) ]


data Guard           = Receive MType
                     | AtState Machine State
                       deriving(Show)

type MType           = String --mtype in murphi


data Response        = ToState Machine State
                     | Send Message Src Dst     -- see note below for dst
                     | Broadcast Message DstSet
                     | Assign Field Field     -- what if the value also has an owner?
                     | Add Owner SetName Field  -- the owner of the set is the machine
                     | Del Owner SetName Field  -- in question. Owner here is  for the elem
                     | Stall
                       deriving(Show)

type Elem = Field
type DstSet = Field -- we assume the variable refers to a set


-----------------------------------------------------------------
-----------------------------------------------------------------

-- Rules

-- problem: at murphi, one of the responses in rules can be
-- "clear <field>" (in MSI it is acks).
-- just an optimization, no need to do it


data Rules = Rules SelfIssueRules
                   ReceiveOrdNets
                   ReceiveUnordNets
             deriving(Show)

type SelfIssueRules = [( MachineType, [SelfIssueRule] )]
type ReceiveOrdNets = [ReceiveOrdNet]
type ReceiveUnordNets = [ReceiveUnordNet]



-- the guard should (mostly) be AtStateAlias "node" <state>
data SelfIssueRule    = SelfIssueRule RuleName Guard [Response]  -- Guard is a function of
                        deriving(Show)                  -- the guard in the relevant
                                                        -- part of the front-end

-- we need a list of all the machines in the receive rules, because we
-- have IsMember calls for each of them
-- to check that the message is accepted by the machine it is sent to
data ReceiveOrdNet    = ReceiveOrdNet RuleName NetName [VCName] [MachineType]
                        deriving(Show)

data ReceiveUnordNet  = ReceiveUnordNet RuleName NetName [VCName] [MachineType]
                        deriving(Show)


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
                  deriving(Show)


type FieldStart = (VarName, Maybe StartVal)

-----------------------------------------------------------------
-----------------------------------------------------------------

-- Invariants
-- TODO
data Invariants = Invariants String
                  deriving(Show)



-----------------------------------------------------------------
-----------------------------------------------------------------
