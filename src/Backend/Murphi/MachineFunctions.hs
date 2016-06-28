-----------------------------------------------------------------
-------- tomurphi implementation for machine functions ----------
------ i.e. adding/removing from set and receive fucntions ------
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

 tomurphi (MachineFunctions machine_Sets_ReceiveFuncion_LocalVars ) =
   mapconcatln setReceiveSingle machine_Sets_ReceiveFuncion_LocalVars

  where

   -----------------------------

   -- add/remove from set functions

   setFunctions :: MachineType -> TypeDecl -> String
   setFunctions machine set = addToSet machine set ++ "\n" ++
                              removeFromSet machine set


   addToSet :: MachineType -> TypeDecl -> String
   addToSet machine (Decl setName (Set _ elemType))
     = let thisSet = toMachineArrayStr machine ++ "[index]." ++ setName
       in  "procedure addTo" ++ fstCap machine ++ setName ++ "List" ++
           "(x: " ++ Cl.tomurphi elemType ++ ", index: " ++ indexNameStr machine ++
           ");\nbegin\n" ++
           "  if MultiSetCount(i:" ++ thisSet ++ ", " ++
           thisSet ++ "[i] = x) != 0\n" ++
           "  then\n" ++ "   MultiSetAdd(x," ++ thisSet ++ ");\n" ++
           "  endif;\nend;\n"

   addToSet _  _ = error "Used MurphiPrint.addToSet on a non-set"

   -----------------------------

   removeFromSet :: MachineType -> TypeDecl -> String
   removeFromSet machine (Decl setName (Set _ elemType))
    = let thisSet = toMachineArrayStr machine ++ "[index]." ++ setName
      in  "procedure RemoveFrom" ++ fstCap machine ++ setName ++ "List" ++
          "( x: " ++ Cl.tomurphi elemType ++ ", index:" ++ indexNameStr machine ++
          " );\nbegin" ++
          " MultiSetRemovePred(i:" ++ thisSet ++ "," ++  thisSet ++ "[i] = x);\n"
          ++ "end;\n"

   removeFromSet _ _  = error "Used MurphiPrint.removeFromSet on a non-set"

   finalSetFunctions :: MachineType -> [TypeDecl] -> String
   finalSetFunctions machine sets = mapconcatln (setFunctions machine) sets

   -----------------------------
   -----------------------------

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
      "   ErrorUnhandledMsg();\n" -- one ln in ouput however many (even 0) I put here


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
    = "function " ++ machine ++ "Receive(msg:Message; index: "
      ++ indexNameStr machine ++") : boolean;\n" ++
      Cl.tomurphi localVariables ++
      "begin\n" ++
      pushBy 3 (caseAllStates statesGuardsReps) ++
      "\n   else\n" ++
      "      ErrorUnhandledState();\n" ++
      "  endswitch;\n\n" ++
      " -- Message processed\n" ++
      " return true;\n" ++
      "end;\n"


   -----------------------------
   ----------------------------

   -- set + receive functions
   setReceiveSingle :: ( MachineType, Sets, ReceiveFunction, LocalVariables ) -> String
   setReceiveSingle (machine, sets, stateGuardsReps, localVariables) =
     "-- " ++ machine ++ " functions {{{\n" ++
     "-- Add/remove from sets\n" ++
     finalSetFunctions machine sets ++ "\n" ++
     "---------------------------------------------------------\n" ++
     " -- Receive function \n" ++
     finalMachineReceive machine stateGuardsReps localVariables ++
     "\n-- }}}\n"

----------------------------------------------------------------
