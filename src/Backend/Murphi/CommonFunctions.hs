-----------------------------------------------------------------
--------- tomurphi implementation for common functions ----------
---------- i.e. send and advancing ordered buffers --------------
---------------------  and error functions  ---------------------
-----------------------------------------------------------------

module CommonFunctions where

-----------------------------------------------------------------

import MurphiAST
import qualified MurphiClass as Cl
import Data.Char
import Data.List.Split -- for tokenizing strings
                       -- splitOn is used in pushBy

-- general helper functions
import GenHelper

-- helper tomurphi implementations
import TomurphiHelper

-----------------------------------------------------------------
-----------------------------------------------------------------




instance Cl.MurphiClass CommonFunctions where
 tomurphi (CommonFunctions orderedNetNames sendInfo) =
    finalSend      ++ "\n" ++
    finalAdvanceQ  ++ "\n" ++
    errorFunctions ++ "\n"

  where

   -- advance ordered network queue
   advanceQ netName = let count = countName netName
                      in "procedure Advance" ++ netName ++ "(n:Node);\n" ++
                          "begin\n" ++
                          " Assert (" ++  count ++ "[n] > 0) \"Trying to advance empty Q\";\n" ++
                          " for i := 0 to " ++ count ++ "[n]-1 do\n" ++
                            "  if i < " ++ count ++ "[n]-1 then\n   " ++
                              netName ++ "[n][i] := " ++ netName ++ "[n][i+1];\n" ++
                            "  else\n" ++
                              "   undefine " ++ netName ++ "[n][i];\n" ++
                            "  endif;\n" ++
                          " endfor;\n" ++
                          " " ++ count ++ "[n] := " ++ count ++ "[n] - 1;\n" ++
                          "end;\n"

   finalAdvanceQ = mapconcatln advanceQ orderedNetNames

   -----------------------------

   -- Send
   msgArgs = fst sendInfo
   netVCs  = snd sendInfo



   sendTop = "Procedure Send(mtype: MessageType;\n" ++
             "               " ++ "src: Node;\n"    ++
             "               " ++ "dst: Node;\n"    ++
             "               " ++ "vc: VC_Type;\n"  ++
             pushBy 15 (mapconcatln Cl.tomurphi msgArgs) ++ ");"

   sendNext = "var\n  msg: Message;\n\nbegin\n"

   sendStandardAssignments = "msg.src = src;\n" ++
                             "msg.dst = dst;\n" ++
                             "msg.vc  = vc;\n"

   sendEnd = "\nend;\n"

   msgFieldAssign :: MsgArg -> String
   msgFieldAssign (Decl argname argtype) = "msg." ++ argname ++ ":= " ++ argname

   assignments = mapconcatln msgFieldAssign msgArgs

   finalSend = sendTop     ++ "\n" ++
               sendNext    ++ "\n" ++
               pushBy 5 sendStandardAssignments ++ -- contains new line
               pushBy 5 assignments ++ "\n" ++
               pushBy 5 printedNets ++ "\n" ++
               sendEnd

   -----------------------------
   nets = map fst netVCs
   vcs  = map snd netVCs

   -- all the if-then clauses for checking for each network if the msg is in
   -- one of the VCs of that network
   printedNets = printAddNet nets vcConds


   -- adding msg to Network
   printAddNet (net:nets) (cond:conds) =
    " if " ++ cond ++ " then\n  " ++ addToNet net ++ "\n" ++ printAddNetRest nets conds


   printAddNetRest [net] [cond] = " else\n  " ++ addToNet net ++ "\n endif;"
   printAddNetRest [] _         = ""  -- when there is a single net
                                      -- we don't need else stmt in murphi
   printAddNetRest (net : nets) (cond : conds)
     = " elsif " ++ cond ++ " then\n  " ++ addToNet net ++"\n" ++ printAddNetRest nets conds


   -- list of conditions for adding a msg to a net
   -- each element of list is the disjunction that is the condition for this net
   vcConds = let temp = map (map ("vc = " ++)) vcs  -- vcs :: [[VCName]]
             in  map disjunction temp


   -- adding a msg to an arbitrary net
   addToNet :: NetName -> String
   addToNet net | isOrdered net  = addOrd net
                | otherwise      = addUnord net


   -- adding a msg to an ordered net
   addOrd net = let count = countName net
                in  "Assert("++ count ++ "[dst] < NET_MAX)" ++
                    " \"Too many msgs on " ++ net ++ "Q\";\n" ++
                    "  " ++ net ++ "[dst][" ++ count ++ "[dst]] := msg;\n" ++
                    "  " ++ count ++ "[dst] := " ++ count ++ "[dst] + 1;\n"


   -- adding a msg to an unordered net
   addUnord net =
    "Assert (MultiSetCount(i:" ++ net ++ "[dst], true) < NET_MAX)" ++
    "\"Too many messages\";\n" ++
    "  MultiSetAdd(msg, " ++ net ++ "[dst]);\n"

   -----------------------------

   -- helper function
   isOrdered :: NetName -> Bool
   isOrdered net = net `elem` orderedNetNames


   -----------------------------

   -- error functions
   errorFunctions = "procedure ErrorUnhandledMsg();\n" ++
                    "begin\n" ++
                    "  error \"Unhandled message type!\";\n" ++
                    "end;\n\n"
                    ++
                    "procedure ErrorUnhandledState();\n" ++
                    "begin\n" ++
                    "  error \"Unhandled state!\";\n"++
                    "end;"


----------------------------------------------------------------
