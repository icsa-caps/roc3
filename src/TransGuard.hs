---------------------------- Guards -----------------------------

module TransGuard where

-----------------------------------------------------------------
-----------------------------------------------------------------

import qualified Ast as F
import qualified MurphiAST as B
import Data.Maybe -- for fromJust
import TransGen
import TransMsg

-----------------------------------------------------------------
-----------------------------------------------------------------

transReceiveMsg :: F.MachineType -> [F.Field]  -- the machine and its fields
                   -> [B.MsgArg]               -- standard form of msg in Murphi
                   -> B.LocalVariables
                   -> F.Guard -> B.Guard

transReceiveMsg machine machineFields stdArgs locals (F.Issue _)
    = error ("Shouldn't use transReceiveMsg on F.Issue guard.\n" ++
            "F.Issue are rules, they are not part of the receive function")

transReceiveMsg machine machineFields stdArgs locals (F.ReceiveFrom msg src vc)
    = B.Receive mtype (guardMsgArgs msg) (transMaybeSrc src) (transMaybeVC vc)

    where

        mtype = mtypeFromMsg msg

        transMaybeSrc Nothing    = Nothing
        transMaybeSrc (Just src) = Just $ transVar machine
                                                   machineFields
                                                   stdArgs
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
