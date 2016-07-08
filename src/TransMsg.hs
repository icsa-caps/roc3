---------------------------  Messages ---------------------------

module TransMsg where

-----------------------------------------------------------------

import Data.Maybe -- for fromJust
import Data.List  -- for nub
import qualified Ast as F
import qualified Backend.Murphi.MurphiAST as B
import TransGen

-----------------------------------------------------------------
-----------------------------------------------------------------

-- get a list of all msgs



-- get msg from Guard
msgFromGuard :: F.Guard -> Maybe F.Msg
msgFromGuard (F.ReceiveFrom msg _ _ ) = Just msg
msgFromGuard _                        = Nothing


-- get msg from Response
msgFromResp :: F.Response -> Maybe F.Msg
msgFromResp (F.Send msg _ _)         = Just msg
msgFromResp (F.Broadcast _ _ msg _)  = Just msg
msgFromResp _                        = Nothing

-- get msg from list of Responses
msgsFromResps :: [F.Response] -> [F.Msg]
msgsFromResps resps = let maybes    = map msgFromResp resps
                          noNothing = filter (/=Nothing) maybes
                      in  map fromJust noNothing

-- get msg from machine function
msgFromMachineF :: F.MachineFunction -> [F.Msg]
msgFromMachineF machineF = concat $ map singleCase machineF
  where
    -- get messages from a single case in the machine function
    -- i.e. single combination of state and incoming message
    singleCase :: F.MachineFCase -> [F.Msg]
    singleCase (state, guard, maybeState, resps)
      = let guardMsgs = map fromJust $ filter (/=Nothing) $ [msgFromGuard guard]
            respsMsgs = msgsFromResps resps
        in  respsMsgs ++ guardMsgs


-- get all messages

getFMsgs :: F.Ast -> [F.Msg]
getFMsgs fAst = let machines   = F.machines fAst
                    machinesFs = map F.machineFunction machines
                in  nub $ concat $ map msgFromMachineF machinesFs


-----------------------------------------------------------------

--------------------------------
-------------------------------

-- get all Mtypes

-- extract Mtype from a message
mtypeFromMsg :: F.Msg -> B.MType
mtypeFromMsg (F.Msg mtype _) = mtype   -- F.Mtype = B.Mtype = String

-- get all Mtypes
getMTypes :: F.Ast -> [B.MType]
getMTypes fAst = let msgs = getFMsgs fAst
                 in  nub $ map mtypeFromMsg msgs

--------------------------------
--------------------------------

-- get all msg arguments

-- get arguments of a message
argOfMsg :: F.Msg -> [F.MsgArg]
argOfMsg (F.Msg _ args) = args


-- transform a msg arg
transMsgArg :: F.MsgArg -> B.MsgArg
transMsgArg (F.GuardAssign typeDecl _)  = transTypeDecl typeDecl
transMsgArg (F.MsgArg typeDecl)         = transTypeDecl typeDecl


-- get the general form of a message in the backend

getMsgArgs :: F.Ast -> [B.MsgArg]
getMsgArgs fAst = let msgs     = getFMsgs fAst
                      fMsgArgs = concat $ map argOfMsg msgs
                  in  nub $ map transMsgArg fMsgArgs


--------------------------------
--------------------------------

-----------------------------------------------------------------
-----------------------------------------------------------------
