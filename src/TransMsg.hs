---------------------------  Messages ---------------------------

module TransMsg where

-----------------------------------------------------------------

import Data.Maybe -- for fromJust
import Data.List  -- for nub
import qualified Ast as F
import qualified MurphiAST as B
import TransGen


-----------------------------------------------------------------
-----------------------------------------------------------------

-- functions for messages

-- get a list of all msgs



-- get msg from Guard
msgFromGuard :: F.Guard -> Maybe F.Msg
msgFromGuard (F.ReceiveFrom msg _ _ ) = Just msg
msgFromGuard _                        = Nothing


-- get msg from Response
msgFromResp :: F.Response -> Maybe F.Msg
msgFromResp (F.Send msg _ _)         = Just msg
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


-- get the name of the formal parameter of a message
formalMsgArg :: F.MsgArg -> B.MsgArg
formalMsgArg (F.GuardAssign typeDecl _)  = transTypeDecl typeDecl
formalMsgArg (F.MsgArg typeDecl)         = transTypeDecl typeDecl


-- get the general form of a message in the backend
-- src should also be a msg arg
stdMsgArgs :: F.Ast -> [B.MsgArg]
stdMsgArgs fAst = let msgs     = getFMsgs fAst
                      fMsgArgs = (concat $ map argOfMsg msgs)
                  in  nub $ map formalMsgArg fMsgArgs


-- get just the names of msg args
getMsgArgsNames :: F.Ast -> [String]
getMsgArgsNames fAst = map (\(B.Decl name _) -> name) $ stdMsgArgs fAst


--------------------------------
--------------------------------

transMsg :: F.MachineType -> [F.Field]  -- the machine and its fields
            -> [B.MsgArg]               -- standard form of msg args in Murphi
            -> [F.MachineType]          -- non symmetric machines
            -> B.LocalVariables
            -> F.Msg -> B.Message
transMsg machine machineFields stdArgs nonsyms locals (F.Msg mtype args)
    = let
            -- get names of the std args
            stdArgsNames = map (\(B.Decl name _) -> name) stdArgs

            -- transform front args to B.Fields, paired
            -- with the respective formal param
            withformalsArgs  = map getArg args -- :: (String, Maybe B.Field)

            -- look up the name of each std argument in the previous list.
            backArgs     = map oneMaybe $ -- Maybe (Maybe B.Field) -> Maybe B.Field
                           map ((flip lookup) withformalsArgs) stdArgsNames

            -- the list of msg args that are simply mentioned in the
            -- list, they are not assigned to or checked against a value;
            -- these shouldn't appear at all in the args of B.Message;
            -- if we give them the value Nothing then they are set to undefined
            -- in the responses
            simplyReferenced = filter (\(formal,value)-> value == Nothing)
                             $ map getArg args

            -- we subtract those arguments simply referenced because these
            -- will get a value though a direct assignment
            argsWithAssignments = (zip stdArgsNames backArgs) \\ simplyReferenced

      in    B.Message mtype  argsWithAssignments
  where
      -- get the argument in the backend, with the name of the formal parameter
      -- it corresponds to
      getArg :: F.MsgArg -> (String, Maybe B.Field)
      getArg (F.MsgArg typeDecl)
        = let formal = getTypeDeclName typeDecl
          in  (formal, Nothing)
              -- in this case (above), the msg arg name is simply mentioned, but
              -- no value is given in the list of args.
              -- this means we should expect that a value is given
              -- earlier in the roc3 file. Hence, we shouldn't return
              -- a pair at all:
              -- if we return (name, Nothing), it will be set to undefined in murphi;
              -- if we return (name, B.Field (B.Simple name) B.Msg)
              -- we will print newMsg.name = msg.name, which we don't want.
      getArg (F.GuardAssign decl param)
        = let formal = getTypeDeclName decl
              field = transVar machine machineFields stdArgs nonsyms locals param
          in  (formal, Just field)

      oneMaybe :: Maybe (Maybe B.Field) -> Maybe B.Field
      oneMaybe (Just Nothing) = Nothing
      oneMaybe (Just (Just field)) = Just field
      oneMaybe Nothing = Nothing




-----------------------------------------------------------------
-----------------------------------------------------------------
