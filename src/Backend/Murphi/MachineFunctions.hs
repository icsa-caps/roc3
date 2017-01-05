-----------------------------------------------------------------
-------- tomurphi implementation for machine functions ----------
-- i.e. adding/removing from set , broadcasting and receive fucntions
-----------------------------------------------------------------

module MachineFunctions where

-----------------------------------------------------------------

import  MurphiAST
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




instance Cl.MurphiClass MachineFunctions where

 tomurphi (MachineFunctions allInfo ) =
   mapconcatln setReceiveSingle allInfo      -- each function is implemented
                                             -- for a single machine


  where

   -----------------------------

   -- add/remove from set functions

   setFunctions :: MachineType -> TypeDecl -> String
   setFunctions machine set = addToSet machine set ++ "\n" ++
                              removeFromSet machine set


   addToSet :: MachineType -> TypeDecl -> String
   addToSet machine (Decl setName (Set _ elemType))
     = let thisSet =indexedFormalStr machine ++ "." ++ setName
       in  "procedure AddTo" ++ fstCap machine ++ fstCap setName ++ "List" ++
           "(x: " ++ Cl.tomurphi elemType ++ "; " ++
           formalIndexStr machine ++ " : " ++ indexTypeStr machine ++
           ");\nbegin\n\n" ++
           "  if MultiSetCount(i:" ++ thisSet ++ ", " ++
           thisSet ++ "[i] = x) = 0\n" ++
           "  then\n" ++ "   MultiSetAdd(x," ++ thisSet ++ ");\n" ++
           "  endif;\n\nend;\n"

   addToSet _  _ = error "Used MurphiPrint.addToSet on a non-set"

   -----------------------------

   removeFromSet :: MachineType -> TypeDecl -> String
   removeFromSet machine (Decl setName (Set _ elemType))
    = let thisSet = indexedFormalStr machine ++ "." ++ setName
      in  "procedure RemoveFrom" ++ fstCap machine ++ fstCap setName ++ "List" ++
          "( x: " ++ Cl.tomurphi elemType ++ "; " ++
          formalIndexStr machine ++ " : " ++ indexTypeStr machine ++
          " );\nbegin\n\n" ++
          " MultiSetRemovePred(i:" ++ thisSet ++ "," ++  thisSet ++ "[i] = x);\n"
          ++ "\nend;\n"

   removeFromSet _ _  = error "Used MurphiPrint.removeFromSet on a non-set"

   finalSetFunctions :: MachineType -> [TypeDecl] -> String
   finalSetFunctions machine sets = mapconcatln (setFunctions machine) sets

   ----------------------------------------------------------
   ----------------------------------------------------------
   -- broadcasting functions
   -- one broadcasting funtion for each pair of set and msg
   -- murphi cannot handle more generality:
   -- composite types can't be arguments of functions or procedures



   -- broadcast for a single set and msg
   singleBroadcast :: BCastInfo -> String
   -- TODO BCast has extra arg [MsgArg]
   -- must change implementation
   singleBroadcast (BCast machine set elemType stdArgs)
     = let
           srcField      = Field (Simple "src") Local
           dstField      = Field (Simple "n") Local
           thisSet       = indexedFormalStr machine ++ "." ++ set
           functionName  = "procedure BroadcastTo" ++ fstCap set
           spaceNum      = length functionName + 1 -- 1 for (
           allMsgArgs -- must pring assignment for src, mtype, vc as well. Boolean plays no role.
              = [(Decl "src" Boolean), (Decl "mtype" Boolean), (Decl "vc" Boolean)]
                        ++ stdArgs
           assignNewMsgFields = mapconcatln checkUndefAssign allMsgArgs
       in
           functionName ++
           "(msg:Message; " ++
            (formalIndexStr machine ++ ":"
                   ++ indexTypeStr machine) ++  ");\n" ++
           "\nvar\n" ++
           "  newMsg:Message;\n" ++
           "\nbegin\n" ++
           "\n  -- if a field is undefined, we must assign the undefined value explicitly\n" ++
           (pushBy 2 assignNewMsgFields) ++ "\n" ++ -- set the values of newMsg
           "  for n:Node do\n" ++
           "    if  ( IsMember(n, " ++ Cl.tomurphi elemType ++  ") &\n" ++
           "       MultiSetCount(i:" ++ thisSet ++ ", "
           ++ thisSet ++ "[i] = n) != 0 )\n" ++
           "    then\n" ++
           "      newMsg.dst := n;\n" ++
           "      Send(newMsg);\n" ++
           "    endif;\n" ++
           "  endfor;\n" ++
           "end;\n"
       where
           -- checks if a msg arg is undefined and set the val of the newMsg arg.
           --  If it is invalid, newMsg:=undefined
           -- o/w murphi throws error.
           checkUndefAssign :: MsgArg -> String
           checkUndefAssign (Decl name _)
            = let msgArg = "msg." ++ name
                  newMsgArg = "newMsg." ++ name
              in  "if isUndefined(" ++ msgArg ++ ") then\n" ++
                  "   " ++ newMsgArg ++ " := undefined;\n" ++
                  "else\n" ++
                  "   " ++ newMsgArg ++ " := " ++ msgArg ++ ";\n" ++
                  "endif;\n"

   ----------------------------------------------------------
   ----------------------------------------------------------

   -- Printing responses and guards e.g taking different cases (if-then) for
   -- mtype and responding
   allResponses :: [Response] -> String
   allResponses responses = mapconcatln Cl.tomurphi responses

   -----------------------------

   guardedResponses :: (Maybe Guard, [Response]) -> String
   guardedResponses (Just guard, responses) = Cl.tomurphi guard ++ " then\n" ++
                                              pushBy 2 (allResponses responses)
   -- if no guard, just print the responses
   guardedResponses (Nothing, responses) = allResponses responses

   elsifResponses :: [(Maybe Guard, [Response])] -> String
   elsifResponses []           = ""
   elsifResponses guardsResponses =
     let indiv = map guardedResponses guardsResponses
     in  mapconcatln ( "\nelsif " ++ ) indiv ++ "\n"

   -----------------------------

   -- If no guard, then there is only one case to consider
   -- and we print the responses
   finalGuardsResps [(Nothing, responses)] = allResponses responses

   finalGuardsResps ( (Just guard,responses) : rest )
    = "if " ++ guardedResponses (Just guard, responses) ++ "\n" ++
      elsifResponses rest  ++ "\n" ++
      "else\n" ++
      "   ErrorUnhandledMsg();\n" ++  -- one ln in ouput however many (even 0) I put here
      "endif;\n"

   -----------------------------

   -- taking different cases for each state
   caseState :: ( State, [ ( Maybe Guard, [Response]) ] ) -> String
   caseState (state, guardsResps) = "\nCase " ++ state ++ ":\n" ++
                                    (pushBy 3 (finalGuardsResps guardsResps))

   --Take different cases for all states
   caseAllStates :: [( State, [ (Maybe Guard, [Response]) ] )] -> String
   caseAllStates = mapconcatln caseState

   -----------------------------

   -- printing the receive function
   finalMachineReceive :: MachineType -> ReceiveFunction -> LocalVariables -> String
   finalMachineReceive machine statesGuardsReps localVariables
    = "function " ++ machine ++ "Receive(msg:Message; " ++
      formalIndexStr machine ++ " : "++ indexTypeStr machine
      ++ ") : boolean;\n" ++
      "\nvar\n" ++
      "  newMsg : Message;\n" ++
      (pushBy 2 (Cl.tomurphi localVariables)) ++
      "\nbegin\n" ++
      " switch " ++
      indexedFormalStr machine ++ ".state" ++
      pushBy 3 (caseAllStates statesGuardsReps) ++
      "\n   else\n" ++
      "      ErrorUnhandledState();\n" ++
      " endswitch;\n\n" ++
      " -- Message processed\n" ++
      " return true;\n" ++
      "end;\n"


   -----------------------------
   ----------------------------

   -- set + receive functions
   setReceiveSingle :: ( MachineType, Sets, [BCastInfo],
                         ReceiveFunction, LocalVariables ) -> String
   setReceiveSingle (machine, sets, bcasts, stateGuardsReps, localVariables) =
     "-- " ++ machine ++ " functions {{{\n\n" ++
     "-- Add/remove from sets\n" ++
     finalSetFunctions machine sets ++ "\n" ++
     "---------------------------------------------------------\n\n" ++
     "-- broadcasting \n" ++
     mapconcatln singleBroadcast bcasts ++ "\n" ++
     "---------------------------------------------------------\n\n" ++
     "-- Receive function \n" ++
     finalMachineReceive machine stateGuardsReps localVariables ++
     "\n-- }}}\n"

----------------------------------------------------------------
----------------------------------------------------------------
