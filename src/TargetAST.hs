
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


data Program = Program Constants
                       Types
                       Variables
                       CommonFunctions
                       MachinesFunctions
                       Rules
                       Startstate
                       Invariants

-----------------------------------------------------------------



type Constants = [Size] -- constants are the sizes of machines


-----------------------------------------------------------------

data Types  = Types
             {
                machines       :: [(Machine,Size)],   -- the scalarsets for indexing

                nodes          :: [Machines],         -- union of the machines

                msgType        :: [Msg],              -- msgs from the input

                machineStates  :: [ (machine,         -- records in murphi
                                    [state],          --
                                    [field]) ]        --
             }




-----------------------------------------------------------------


data Variables      = data [machines]       -- may be augmented (e.g networks)


-----------------------------------------------------------------


CommonFunctions = FunctionParameters {
                                       SendArgs :: [MsgArgType]
        -- extensions: fwd ordered queue
                                      }


type MsgArgType = String


-----------------------------------------------------------------

type MachineFunctions = [ ( Machine, ReceiveFunction ) ]

type ReceiveFunction = [ ( State, GuardT, [Respond] ) ]

data GuardT = ReceiveT Msg From   -- the src may be important
            | AtState State

data Respond = ToState State
             | Send MsgStr [(MsgArgType,Arg)] Rec
             | Assign Var Val

-----------------------------------------------------------------

data Rules = Rules { SelfIssueRules :: [SelfIssueRule],
                     ReceiveRules :: [ReceiveRule] }

data SelfIssueRule = SelfIssueRule GuardT [Respond] -- GuardT is a function of
                                                    -- the guard in the relevant
                                                    -- part of the front-end




data ReceiveRule = ReceiveRule BufferName    -- keep the associated channels
                                             -- in a map











-----------------------------------------------------------------
