module GetRules(getRules) where

------------------------------------------------------------------

import qualified Ast as F
import qualified MurphiAST as B
import TransGen
import TransResponse
import TransMsg
import TransGuard
import Data.Maybe -- for fromJust

------------------------------------------------------------------
------------------------------------------------------------------


-- data Rules = Rules SelfIssueRules
--                    ReceiveOrdNets
--                    ReceiveUnordNets
--              deriving(Show,Eq)
--
-- SelfIssueRule   = SelfIssueRule RuleName Guard [Response]
-- ReceiveOrdNet   = ReceiveOrdNet NetName [VCName] [MachineType]
-- ReceiveUnordNet = ReceiveUnordNet NetName [VCName] [MachineType]


getRules :: F.Ast -> B.Rules
getRules fAst =  B.Rules selfIssueRules receiveOrdNets receiveUnordNets
  where
      machines = getMachineNames fAst

      nonsyms  = getNonsyms fAst

      ordNets = getOrdVCs fAst
      receiveOrdNets = map (\(netName, vcs) ->
                             B.ReceiveOrdNet netName vcs machines)
                           ordNets

      unordNets = getUnordVCs fAst
      receiveUnordNets = map (\(netName, vcs) ->
                               B.ReceiveUnordNet netName vcs machines)
                             unordNets


      selfIssueRules = let machineInfo = findSelfIssued fAst
                           stdArgs     = stdMsgArgs fAst
                       in  allSelfIssued machineInfo stdArgs nonsyms




-----------------------------------------------------------------
-----------------------------------------------------------------


-- get the ordered networks with their channels
getOrdVCs :: F.Ast -> [(B.NetName, [B.VCName])]
getOrdVCs fAst = let allNets  = F.networks fAst
                     ordered  = filter (isOrdered) allNets
                 in map (\(F.Network _ name vcs) -> (name, map getVCName vcs))
                        ordered

--------------------------------

-- get the unordered networks with their channels
getUnordVCs :: F.Ast -> [(B.NetName, [B.VCName])]
getUnordVCs fAst = let allNets    = F.networks fAst
                       unordered  = filter (not . isOrdered) allNets
                   in  map (\(F.Network _ name vcs) -> (name, map getVCName vcs))
                           unordered


----------------------------------------------------------------

-- returns the machines with their self issued rules and their fields
findSelfIssued :: F.Ast -> [(F.MachineType, [F.Field], [F.MachineFCase])]
findSelfIssued fAst
  = let machines      = F.machines fAst
        allMFunctions = map F.machineFunction machines
        machineNames  = getMachineNames fAst
        machineFields = map F.fields machines
        triplets      = zip3 machineNames machineFields allMFunctions
        selfIssued    = map ( \(machine, fields, function) ->
                              (machine, fields, filter isSelfIssued function) )
                         triplets
        -- remove machines with no self-issue rules
    in  filter ( \(machine, fields, function) -> function /= [] ) selfIssued

-----------------------------------------------------------------

-- build one self-issued rule

singleSelfIssued :: F.MachineType -> [F.Field]   -- the machine and its fields
                    -> [B.MsgArg]                -- standard form of msg in Murphi
                    -> [F.MachineType]           -- list of nonsymmetric machines
                    -> F.MachineFCase -> B.SelfIssueRule
singleSelfIssued machine machineFields stdArgs nonsyms
                (state1, selfIssueMsg, state2, fResps)

      = B.SelfIssueRule rulename localVars bGuard bResps

  where
    (F.Issue rulename) = selfIssueMsg

    localVars = findLocal fResps -- use also in transResponse

    changeState = if state2 == Nothing then []
                  else let stateName = transState machine $ fromJust state2
                       in  [B.ToState (B.AnyType machine) stateName]

    bResps = map (transResponse machine machineFields stdArgs nonsyms localVars) fResps
             ++ changeState

    bGuard = guardState machine state1

-----------------------------------------------------------------

singleMachineSelfIssued :: F.MachineType -> [F.Field] ->
                           [B.MsgArg] ->      -- standard form of msg in Murphi
                           [F.MachineType] -> -- list of nonsymmetric machines
                           [F.MachineFCase] -> (B.MachineType,[B.SelfIssueRule])

singleMachineSelfIssued machine fields stdArgs nonsyms functionCases
 = let rules = map (singleSelfIssued machine fields stdArgs nonsyms) functionCases
   in  (machine,rules)


allSelfIssued :: [ ( F.MachineType, [F.Field], [F.MachineFCase] ) ] ->
                 [B.MsgArg] ->      -- standard form of msg in Murphi
                 [F.MachineType] -> -- list of nonsymmetric machines
                 [(B.MachineType,[B.SelfIssueRule])]

allSelfIssued allMachinesInfo stdArgs nonsyms = map forTriplet allMachinesInfo
    where
        forTriplet :: ( F.MachineType, [F.Field], [F.MachineFCase] ) ->
                      (B.MachineType,[B.SelfIssueRule])
        forTriplet (machine, fields, rulesToBe)
          = singleMachineSelfIssued machine fields stdArgs nonsyms rulesToBe


------------------------------------------------------------------
------------------------------------------------------------------
