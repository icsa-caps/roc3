-----------------------------------------------------------------
-------------- tomurphi implementation for Rules ------------
-----------------------------------------------------------------

module Rules where

-----------------------------------------------------------------

import  MurphiAST
import qualified MurphiClass as Cl
import Data.Char

-- general helper functions
import GenHelper

-- helper tomurphi implementations
import TomurphiHelper

-----------------------------------------------------------------
-----------------------------------------------------------------


instance Cl.MurphiClass Rules where
 tomurphi (Rules selfIssued receiveOrdNets receiveUnordNets)
   = finalSelfIssued ++
     "-- ordered networks receive rules" ++
     finalReceiveOrdNets ++
     "-- unordered networks receive rules" ++
     finalReceiveUnordNets

  where
   -----------------------------
   -- self-issued rules

   -- self-issued rules of one machine
   singleMachineSelfIssueRules :: (MachineType, [SelfIssueRule]) -> String
   singleMachineSelfIssueRules (machine, rules)
     = "-- " ++ machine ++ " self-issued rules\n"++
       "ruleset"++ formalIndexStr machine ++ ":" ++ indexTypeStr machine
       ++  " do\n" ++
       "  alias node:" ++ indexedFormalStr machine ++ " do\n\n" ++
       pushBy 4 (mapconcatln Cl.tomurphi rules) ++ "\n" ++
       "  endalias;\n" ++
       "endruleset;\n"

   -- self-issued rules for all machines
   allSelfIssued :: [(MachineType, [SelfIssueRule])] -> String
   allSelfIssued = mapconcatln singleMachineSelfIssueRules

   finalSelfIssued = allSelfIssued selfIssued

   -----------------------------
   -- unordered network rules

   -- a single unordered net receive rule
   singleUnordNet :: ReceiveUnordNet -> String
   singleUnordNet (ReceiveUnordNet netName vcs machines)
     = let disjunctVCs   = disjunction $ map ("msg.vc = " ++ ) vcs
           allIsMember   = map (isMember "n") machines
           allProcess    = map processMessage machines
           casesMachines = printIfElse allIsMember allProcess
           ruleName      = netName ++ "Receive"

       in  "ruleset n:Node do\n" ++
           "  choose midx:" ++ netName ++ "[n] do\n" ++
           "    alias chan:net[n]\n" ++
           "      alias msg:chan[midx]\n\n" ++

            "       rule " ++ ruleName ++ "\n" ++
            pushBy 10 (disjunctVCs)    ++ "\n" ++
                    "=>\n" ++
            pushBy 10 casesMachines    ++ "\n" ++
            "       endrule;\n\n" ++

            "     endalias;\n"    ++
            "   endalias;\n"      ++
            "  endchoose;\n"      ++
            "endruleset;\n"

   -- receive rules for a list of unordered nets
   allUnordNet :: [ReceiveUnordNet] -> String
   allUnordNet = mapconcatln singleUnordNet

   -- final
   finalReceiveUnordNets = allUnordNet receiveUnordNets

    -----------
   -- helper functions
   isMember :: String -> MachineName -> String
   isMember alias machine = "IsMember(" ++ alias ++ ", " ++ machine ++ ")"

   processMessage :: MachineName -> String
   processMessage machine = "if " ++ machine ++ "Receive(msg,n) then \n" ++
                             "  MultisetRemove(midx, chan);\n" ++
                             "endif;\n"
   -----------


   -----------------------------
   -- ordered network rules

   -- a single ordered net receive rule
   singleOrdNet :: ReceiveOrdNet -> String
   singleOrdNet (ReceiveOrdNet netName vcs machines)
     = let disjunctVCs   = disjunction $ map ("msg.vc = " ++ ) vcs
           allIsMember   = map (isMember "n") machines
           allDeQ        = map (deQ netName) machines
           casesMachines = printIfElse allIsMember allDeQ
           ruleName      = netName ++ "Receive"

       in  "ruleset n:Node do\n" ++
           "  choose midx:" ++ netName ++ "[n] do\n" ++
           "    alias chan:net[n]\n" ++
           "      alias msg:chan[midx]\n\n" ++

           "       rule " ++ ruleName ++ "\n" ++
           "         " ++ countName netName ++ "[n] > 0\n" ++
           "       =>\n" ++
           pushBy 9 casesMachines ++ "\n" ++
           "       endrule;\n\n" ++

           "     endalias;\n"    ++
           "   endalias;\n"      ++
           "  endchoose;\n"      ++
           "endruleset;\n"

   -- helper function
   deQ :: NetName -> MachineName -> String
   deQ net machine = "if " ++ machine ++ "Receive then\n" ++
                     "   Advance" ++ net ++ "(n);\n" ++
                     "endif;"


   -- receive rules for a list of ordered nets
   allOrdNet :: [ReceiveOrdNet] -> String
   allOrdNet = mapconcatln singleOrdNet

   -- applying it
   finalReceiveOrdNets = allOrdNet receiveOrdNets
   -----------------------------

----------------------------------------------------------------
----------------------------------------------------------------

instance Cl.MurphiClass SelfIssueRule where
  tomurphi (SelfIssueRule rulename localVars guard responses)
    = "rule \"" ++ rulename ++ "\"\n" ++
      pushBy 2 (Cl.tomurphi guard) ++ "\n" ++
      "=>\n" ++
      pushBy 2 (Cl.tomurphi localVars) ++
      "Begin\n" ++
      pushBy 2 (mapconcatln Cl.tomurphi responses) ++ "\n" ++
      "endrule;"


-----------------------------------------------------------------
-----------------------------------------------------------------
