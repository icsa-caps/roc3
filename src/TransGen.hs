-----------------------------------------------------------------
---------- Functions that extract essential information ---------
-------------------- from the front-end AST ---------------------

module TransGen where

-----------------------------------------------------------------

import Data.Maybe -- for fromJust
import Data.List  -- for nub
import qualified Ast as F
import qualified Backend.Murphi.MurphiAST as B

-----------------------------------------------------------------
-----------------------------------------------------------------

-- Helper functions


--------------------------------

-- is a net ordered?
isOrdered :: F.Network -> Bool
isOrdered (F.Network F.Ord _ _) = True
isOrdered _               = False

--------------------------------

backSymmetry :: F.Symmetry -> B.Symmetry
backSymmetry (F.Symmetric)    = B.Symmetric
backSymmetry (F.Nonsymmetric) = B.Nonsymmetric

--------------------------------

getStateName :: F.State -> String
getStateName (F.State name) = name

--------------------------------

-- get the vcs of a network
getChanNet :: F.Network -> [F.VC]
getChanNet (F.Network order name vcs) = vcs

--------------------------------

-- get the names of all the vcs of the model
getAllVCs :: F.Ast -> [String]
getAllVCs frontAST = let nets = F.networks frontAST
                         vcs = concat $ map getChanNet nets
                     in  map (\(F.VC name) -> name) vcs

----------------------------------------------------------------

-- functions for low-level constructs (e.g. responses, type declarations etc.)

-------------------------------
-- Transforming type declarations
--------------------------------

-- returns the name of a type declaration
getTypeDeclName :: F.TypeDecl -> String
getTypeDeclName (F.Boolean varName)       = varName
getTypeDeclName (F.Integer varName _ _)   = varName
getTypeDeclName (F.Enum varName _)        = varName
getTypeDeclName (F.Vertex _ varName)      = varName
getTypeDeclName (F.Array _ otherTypeDecl) = getTypeDeclName otherTypeDecl
getTypeDeclName (F.Set _ otherTypeDecl)   = getTypeDeclName otherTypeDecl

--------------------------------

-- returns the type of this type declaration
-- (e.g.boolean, array of array of enums etc.)
getType :: F.TypeDecl -> B.Type
getType (F.Boolean _)             = B.Boolean

getType (F.Integer _ lo hi)       = B.Integer lo hi

getType (F.Enum _ values)         = B.Enum values

getType (F.Vertex machineType _)  = B.Node machineType

getType (F.Array (Left size) otherDecl)
  = B.Array (Left size) (getType otherDecl)

getType (F.Array (Right machine) otherDecl)
  = B.Array (Right machine) (getType otherDecl)

getType (F.Set (Left size) otherDecl)
  = B.Set (Left size) (getType otherDecl)

getType (F.Set (Right machine) otherDecl)
  = B.Set (Right machine) (getType otherDecl)

--------------------------------

-- getting from the fronend type declaration to the backend type declaration
transTypeDecl :: F.TypeDecl -> B.TypeDecl
transTypeDecl frontTypeDecl = B.Decl (getTypeDeclName frontTypeDecl)
                                     (getType frontTypeDecl)

--------------------------------
--------------------------------

----------------------------------------------------------------
----------------------------------------------------------------

-- Getting names and sizes (non machine specific)

--------------------------------

getMachineNames :: F.Ast -> [B.MachineType]
getMachineNames frontAST = let machinesAllInfo = F.machines frontAST
                           in  map F.machineType machinesAllInfo

--------------------------------


getVCNames :: F.Ast -> [B.VCName]
getVCNames frontAST = let nets = F.networks frontAST
                          vcs  = map ( \(F.Network _ _ vcs) -> vcs ) nets
                      in  map (\(F.VC name) -> name) $ concat vcs

--------------------------------

getAllNetNames :: F.Ast -> [B.NetName]
getAllNetNames frontAST = let nets = F.networks frontAST
                          in  map ( \(F.Network _ name _) -> name ) nets

--------------------------------

getOrdNetNames :: F.Ast -> [B.OrderedNetName]
getOrdNetNames frontAST = let allNets = F.networks frontAST
                              onlyOrdered = filter isOrdered allNets
                          in  map( \(F.Network _ name _) -> name ) onlyOrdered

--------------------------------

getUnordNetNames :: F.Ast -> [B.UnorderedNetName]
getUnordNetNames frontAST = let allNets = F.networks frontAST
                                onlyUnordered = filter (not . isOrdered) allNets
                            in  map( \(F.Network _ name _) -> name ) onlyUnordered

--------------------------------

----------------------------------------------------------------
----------------------------------------------------------------
-- getting machine info

getMachineSizes :: F.Ast -> [B.Size]
getMachineSizes frontAST = let machinesAllInfo = F.machines frontAST
                           in  map F.size machinesAllInfo

--------------------------------

getSymmetries :: F.Ast -> [B.Symmetry]
getSymmetries frontAST = let machinesAllInfo = F.machines frontAST
                             frontSymmetries = map F.symmetry machinesAllInfo
                         in  map backSymmetry frontSymmetries

--------------------------------

getStartstates :: F.Ast -> [B.State]
getStartstates frontAST = let machinesAllInfo = F.machines frontAST
                          in  map (getStateName . F.startstate) machinesAllInfo



--------------------------------



--------------------------------


-----------------------------------------------------------------
-----------------------------------------------------------------


---------------------------  Messages ---------------------------


-----------------------------------------------------------------

-- get a list of all msgs

-- get msg from mail
msgFromMail :: F.Mail -> Maybe F.Msg
msgFromMail (F.Send msg _ _ )        = Just msg
msgFromMail (F.ReceiveFrom msg _ _ ) = Just msg
msgFromMail _                        = Nothing


-- get msg from Guard
msgFromGuard :: F.Guard -> Maybe F.Msg
msgFromGuard (F.Guard mail) = msgFromMail mail


-- get msg from Response
msgFromResp :: F.Response -> Maybe F.Msg
msgFromResp (F.Response mail) = msgFromMail mail
msgFromResp _                 = Nothing

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
