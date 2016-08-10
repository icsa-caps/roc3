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
   singleBroadcast (BCast machine set elemType mtype msgArgs)
     = let
           srcField      = Field (Simple "src") Local
           dstField      = Field (Simple "n") Local
           thisSet       = indexedFormalStr machine ++ "." ++ set
           functionName  = "procedure Broadcast" ++ fstCap mtype ++ fstCap set
           spaceNum      = length functionName + 1 -- 1 for (
           argsForSend   = map sendArg msgArgs -- MsgArg -> Field for Send in loop
           msg           = Message mtype argsForSend
       in
           functionName ++
           "(src:Node;\n" ++
            ( pushBy spaceNum (formalIndexStr machine ++ ":"
                   ++ indexTypeStr machine) ) ++ ";\n" ++
            ( pushBy spaceNum "vc:VC_Type" ) ++ ";\n" ++
            ( pushBy spaceNum (declareArgs msgArgs) ) ++ ");\n" ++
           "begin\n" ++
           "  for n:Node do\n" ++
           "    if  ( IsMember(n, " ++ Cl.tomurphi elemType ++  ") &\n" ++
           "       MultiSetCount(i:" ++ thisSet ++ ", "
           ++ thisSet ++ "[i] = n) != 0 )\n" ++
           "    then\n" ++
           ( pushBy 6 (Cl.tomurphi (Send msg srcField dstField "vc")) ) ++ "\n" ++
           "    endif;\n" ++
           "  endfor;\n" ++
           "end;\n"
       where
           declareArgs = mapconcatln Cl.tomurphi

           -- we must transform the MsgArg to Field for
           -- the Send in the loop
           -- MsgArg :: TypeDecl
           -- Note: the argument to a function in  Murphi can't be
           -- set or array, because only simple (not composite) types can
           -- be passed as arguments to functions.
           sendArg :: MsgArg -> Maybe Field
           sendArg (Decl _ (Set _ _))
             = error "murphi can't take a composite type (here set) as argument"
           sendArg (Decl _ (Array _ _ ))
             = error "murphi can't take a composite type (here array) as argument"
           sendArg (Decl name _) -- we don't care about the type
              = Just $ Field (Simple name) Global -- passed/argument


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
      Cl.tomurphi localVariables ++
      "begin\n" ++
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
