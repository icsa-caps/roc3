---------------------------- Guards -----------------------------

module TransGuard where

-----------------------------------------------------------------
-----------------------------------------------------------------

import qualified Ast as F
import qualified MurphiAST as B
import Data.Maybe -- for fromJust
import TransGen
import TransMsg
import TransResponse -- for transExp

-----------------------------------------------------------------
-----------------------------------------------------------------

transGuard :: F.MachineType -> [F.Field]  -- the machine and its fields
              -> [B.MsgArg]               -- standard form of msg in Murphi
              -> [F.MachineType]          -- non symmetric machines
              -> B.LocalVariables
              -> F.Guard -> B.Guard

transGuard machine machineFields stdArgs nonsyms locals guard
 = case guard of
   (F.Equals param1 (Left param2)) -> let field1 = (transVar machine machineFields
                                                             stdArgs nonsyms locals
                                                             param1)
                                          -----------------------------------
                                          field2 = (transVar machine machineFields
                                                             stdArgs nonsyms locals
                                                             param2)
                                          -----------------------------------
                                      in  B.Equals field1 (Left field2)


   (F.NotEq param1 (Left param2)) -> let field1 = (transVar machine machineFields
                                                            stdArgs nonsyms locals
                                                            param1)
                                          -----------------------------------
                                         field2 = (transVar machine machineFields
                                                            stdArgs nonsyms locals
                                                            param2)
                                         -----------------------------------
                                    in  B.NotEq field1 (Left field2)


   (F.Equals param (Right intExp)) -> let field  =  (transVar machine machineFields
                                                              stdArgs nonsyms locals
                                                              param)
                                          -----------------------------------
                                          bIntExp = (transExp machine machineFields
                                                              stdArgs nonsyms locals
                                                              intExp)
                                       -----------------------------------
                                   in  B.Equals field (Right bIntExp)


   (F.NotEq param (Right intExp)) -> let field    =  (transVar machine machineFields
                                                               stdArgs nonsyms locals
                                                               param)
                                         -----------------------------------
                                         bIntExp = (transExp machine machineFields
                                                             stdArgs nonsyms locals
                                                             intExp)
                                         -----------------------------------
                                     in  B.NotEq field (Right bIntExp)




   (F.Not innerGuard)      -> B.Not (transGuard machine machineFields stdArgs
                                                nonsyms locals
                                                innerGuard)

   (guard1 F.:&: guard2)   -> let bg1 = transGuard machine machineFields stdArgs
                                                nonsyms locals
                                                guard1
                                  --------------------------------------------
                                  bg2 = transGuard machine machineFields stdArgs
                                                   nonsyms locals
                                                   guard2
                                  --------------------------------------------
                              in  bg1 B.:&: bg2


   (guard1 F.:|: guard2)   -> let bg1 = transGuard machine machineFields stdArgs
                                                nonsyms locals
                                                guard1
                                  --------------------------------------------
                                  bg2 = transGuard machine machineFields stdArgs
                                                   nonsyms locals
                                                   guard2
                                  --------------------------------------------
                              in  bg1 B.:|: bg2

   msgGuard                -> transReceiveMsg machine machineFields stdArgs
                                              nonsyms locals
                                              msgGuard

-----------------------------------------------------------------
-----------------------------------------------------------------


transReceiveMsg :: F.MachineType -> [F.Field]  -- the machine and its fields
                   -> [B.MsgArg]               -- standard form of msg in Murphi
                   -> [F.MachineType]          -- non symmetric machines
                   -> B.LocalVariables
                   -> F.Guard -> B.Guard

transReceiveMsg machine machineFields stdArgs nonsyms locals (F.Issue _)
    = error ("Shouldn't use transReceiveMsg on F.Issue guard.\n" ++
            "F.Issue are rules, they are not part of the receive function")

transReceiveMsg machine machineFields stdArgs nonsyms locals (F.ReceiveFrom msg src vc)
    = B.Receive mtype (guardMsgArgs msg) (transMaybeSrc src) (transMaybeVC vc)

    where

        mtype = mtypeFromMsg msg

        transMaybeSrc Nothing    = Nothing
        transMaybeSrc (Just src) = Just $ transVar machine
                                                   machineFields
                                                   stdArgs
                                                   nonsyms
                                                   locals
                                                   src

        transMaybeVC Nothing   = Nothing
        transMaybeVC (Just vc) = Just $ getVCName vc


        guardMsgArgs :: F.Msg -> [(B.ArgName, B.Field)]
        guardMsgArgs (F.Msg _ args) = let maybes = map transArg args
                                          noNothing = filter (/=Nothing) maybes
                                      in  map fromJust noNothing
           where
               transArg :: F.MsgArg -> Maybe (B.ArgName, B.Field)
               transArg (F.MsgArg _) = Nothing
               transArg (F.GuardAssign typeDecl param)
                  = let testVal  = transVar machine
                                           machineFields
                                           stdArgs
                                           nonsyms
                                           locals
                                           param
                        argName  = getTypeDeclName typeDecl
                    in  Just (argName, testVal)

-----------------------------------------------------------------
-----------------------------------------------------------------

-- translate a state at the front end to
-- a guard with a state at the Backend

guardState :: F.MachineType -> F.State -> B.Guard
guardState machine state = let stateName  = transState machine state
                               bMachine   = B.AnyType machine
                           in  B.AtState bMachine stateName












-----------------------------------------------------------------
-----------------------------------------------------------------
