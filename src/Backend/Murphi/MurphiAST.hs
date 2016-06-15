
module MurphiAST where

-- this AST captures the subset of the murphi language
-- as it is used in the MSI protocol implementations
-- (at a first stage the MI implementation).
-- It is strongly based on the outline used in the above implementations


-- simple type synonyms
type Size             = Int
type TypeName         = String
type Val              = String
type Var              = String
type VCName           = String
type NetName          = String
type Network          = Either OrderedNet UnorderedNet
type OrderedNetName   = String
type UnorderedNetName = String
type Name             = String
type State            = String
type MachineType      = String
type Rec              = String
type MsgType          = String
type StateName        = String
type SetName          = String
type MsgArg           = TypeDecl

-- helper data structures
type VCNets = [ (VCName, Network) ]

-- only for Send
data Message = Message MsgType [ (TypeName, Maybe Val, Owner) ] -- for Owner see bellow
             deriving(Show)

-- in place of TypeDecl in Ast, to make printing easier
data TypeDecl = Decl Name Type
              deriving(Show)

data Type = Boolean
          | Integer Lo Hi
          | Enum [Val]
          | Node MachineType -- TODO: add tomurphi implementation
          | Array Index Type
          | Set (Either MachineType Size) Type -- Left machine means the size
          deriving(Show)                       -- of the set = #machines

type Lo    = Int
type Hi    = Int
type Index = String

-- who owns a field? needed in Respond
-- i.e. when we print variable assignments or send messages
-- e.g. to print "dir.owner = cache[1]" we need to know that (in this context)
-- owner is a field "owned" by dir and is not global
-- (in which case we would print "owner = cache[1]")
-- or if we want to print "Send(msg.src)" or "Send(cache[1].src)"
-- we want to differentiate between the two srcs
data Owner = Msg
           | Global
           | Machine MachineType
           deriving(Show)


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

data MachineFunctions = MachineFunctions [ (Sets, MachineType, ReceiveFunction ) ]
                        deriving(Show)

-- we need a pair of add and remove functions for each set field a machine has
type Sets            = [TypeDecl] -- in MurphiPrint we check TypeDecl is a set

type ReceiveFunction = [ ( State, Guard, [Respond] ) ]


data Guard           = Receive MType   -- the src may be important
                     | AtState State
                      deriving(Show)

type MType           = String --mtype in murphi


data Respond         = ToState State
                     | Send Message Dst
                     | Assign Var Val Owner
                     | Add SetName Elem Owner
                     | Del SetName Elem Owner
                       deriving(Show)

type Elem = String
type Dst = String
-----------------------------------------------------------------

-- Rules

-- problem: at murphi, one of the responses in rules can be
-- "clear <field>" (in MSI it is acks).
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
