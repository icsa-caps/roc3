
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
type VarName          = String
type ArrayName        = String
type Index            = Int
type AliasName        = String


-- helper data structures, data types
type VCNets = [ (VCName, Network) ]

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

                --vcType          :: Size,

                msgType         :: [String], -- kinds of msgs (Ack, Fwd etc.)

                msgArgs         :: [MsgArg], -- only info we need to print msg

                machineStates   :: [ (MachineType,
                                    [State],       -- states of each machine
                                    [TypeDecl]) ]  -- fields of the machine
             }
             deriving(Show)


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
data CommonFunctions = FuncParams {
                        -- need one advanceQ for each ordered net!
                        advanceQ        :: [OrderedNetName],
                        send            :: ( [MsgArg], [(NetName,[VCName])] )
                        -- MSI has also a BCastInv procedure

                       } deriving(Show)

type Param           = String

-----------------------------------------------------------------

-- Machine Functions

data MachineFunctions = MachineFunctions [ ( MachineType,
                                             Sets,
                                             ReceiveFunction ) ]
                                         LocalVariables
                        deriving(Show)

-- we need a pair of add and remove functions for each set field a machine has
type Sets            = [TypeDecl] -- in MurphiPrint we check TypeDecl is a set

type ReceiveFunction = [ ( State, [ (Maybe Guard, [Response]) ] ) ]


data Guard           = Receive MType
                     | AtStateAlias AliasName State
                     | AtState MachineType Index State
                       deriving(Show)

type MType           = String --mtype in murphi


data Response        = ToState MachineType Index State
                     | Send Message Src Dst     -- see note below for dst
                     | Assign Field Field     -- what if the value also has an owner?
                     | Add Owner SetName Field  -- the owner of the set is the machine
                     | Del Owner SetName Field  -- in question. Owner here is  for the elem
                     | Stall
                       deriving(Show)

type Elem = Field

-- only for Send
data Message = Message MType [ Maybe Field ] -- for Owner see bellow
               deriving(Show)

-- who owns a field? needed in Response
-- i.e. when we print variable assignments or send messages
-- e.g. to print "dir.owner = cache[1]" we need to know that (in this context)
-- owner is a field "owned" by dir and is not global
-- (in which case we would print "owner = cache[1]")
-- or if we want to print "Send(msg.src)" or "Send(cache[1].src)"
-- we want to differentiate between the two srcs
data Owner = Msg
           | Global
           | Machine MachineType Index
           | Local                 -- need to add declaration
           | ThisNode
             deriving(Show)

-- change fields to account for elements of arrays i.e. spesific machines
data Field = Field Variable Owner
             deriving(Show)

type Src = Field
type Dst = Field

data Variable = Simple VarName
              | ArrayElem ArrayName Index
              | MachineArray MachineType Index
                  deriving(Show)


-- Send : dst in the frontend language does not have a type
-- (syntax : "dst!Msg") but we must infer the owner,
-- like for the Message arguments

-----------------------------------------------------------------

-- Rules

-- problem: at murphi, one of the responses in rules can be
-- "clear <field>" (in MSI it is acks).
-- just an optimization, no need to do it


data Rules = Rules { selfIssueRules   :: [SelfIssueRule],
                     receiveOrdNets   :: [ReceiveOrdNet],
                     receiveUnordNets :: [ReceiveUnordNet] }
                     deriving(Show)


data SelfIssueRule = SelfIssueRule Guard [Response] -- Guard is a function of
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
