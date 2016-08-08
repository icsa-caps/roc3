-----------------------------------------------------------------
----------------------- Machine functions -----------------------
---------------- Receive functions, broadcasting ----------------

module GetMachineFunctions where

-----------------------------------------------------------------
-----------------------------------------------------------------

import qualified Ast as F
import qualified MurphiAST as B
import Data.Maybe -- for fromJust
import Data.List  -- for nub
import Data.Char
import TransGen
import TransMsg
import TransGuard
import TransResponse

-----------------------------------------------------------------
-----------------------------------------------------------------

-- BCastInfo = BCast MachineType SetName ElemType Message
--ReceiveFunction = [ ( State, [ (Maybe Guard, [Response]) ] ) ]





getMachineFunctions :: F.Ast -> B.MachineFunctions
getMachineFunctions fAst
  = let
      stdArgs = stdMsgArgs fAst
      machines = F.machines fAst
      nonsyms  = getNonsyms fAst

      in B.MachineFunctions $ map (singleMFunction stdArgs nonsyms) machines


-----------------------------------------------------------------

singleMFunction :: [B.MsgArg] ->   -- standard msg arguments format
                   [F.MachineType] -> -- nonsymmetric machines
                    F.Machine -> (B.MachineType, B.Sets, [B.BCastInfo],
                                  B.ReceiveFunction, B.LocalVariables)
                                  -- mfunction :: [( State, Guard,  Maybe State, [Response] )]
                                  -- backend equivalent in B.ReceiveFunction is List of
                                  -- ( State, [ (Maybe Guard, [Response])
singleMFunction stdArgs nonsyms (F.Machine _ machine _ _ fields mFunction)
   = let
         sets = findSets fields

         allFrontResps    = concat $ map ( \ (_,_,_,resps) -> resps )
                                         mFunction
         locals           = findLocal allFrontResps

         -- remove instances of the machine functions with self issued
         -- messages. These belong to B.Rules not to the machine function
         noSelfIssued     = filter (not.isSelfIssued) mFunction

         groupedReactions = groupSameStart noSelfIssued
         receiveFunction  = map (receiveInst machine fields stdArgs nonsyms locals)
                                groupedReactions



         bcasts         = filter (isBCast fields stdArgs locals) allFrontResps
         bcastMtypes    = map bCastMtype bcasts
         castWithMType  = zip bcastMtypes bcasts
         noDupls        = map snd $
                           nubBy ( \(mtype1,_) -> \(mtype2,_)
                                     -> mtype1 == mtype2 )
                                 castWithMType

         bcastinfo      = map (finalBCast machine fields stdArgs)
                              noDupls

     in
         (machine, sets, bcastinfo, receiveFunction, locals)


-----------------------------------------------------------------

-- write a method for finding pairs of messages and sets for which we need
-- a broadcast procedure

-- we must also find the arguments of the broadcasted message

-- BCastInfo =
-- BCast MachineType SetName ElemType MType [Maybe MsgArg]



-- This function puts together all the functions defined below
-- to construct a B.BCastInfo
-- we assume the response is a F.Broadcast
finalBCast :: F.MachineType -> F.Fields -> [B.MsgArg] -> -- std msg args
              F.Response -> B.BCastInfo

finalBCast machine fields stdArgs resp
  = let
        mtype    = bCastMtype resp
        setName  = bCastSetName resp
        elemType = bCastElemType fields setName
        msgArgs  = bCastMsgArgs stdArgs resp
    in
        B.BCast machine setName elemType mtype msgArgs



-- we assume the response is a BCast
bCastMtype :: F.Response -> B.MType
bCastMtype (F.Send msg _ _) = mtypeFromMsg msg
bCastMType _   = error "used bCast on response that is not a broadcast"

bCastSetName :: F.Response -> B.SetName
bCastSetName (F.Send _ dstSet _)  = varName dstSet
bCastSetName _   = error "used bCast on response that is not a broadcast"

bCastElemType :: F.Fields -> F.SetName -> B.ElemType
bCastElemType fields name
  = let
        typeDecls    = map ( \(F.Field typeDecl _) -> typeDecl)
                        fields
        names        = map getTypeDeclName typeDecls

        namesTypes   = zip names typeDecls

        thisTypeDecl = let maybeTypeDecl = lookup name namesTypes
                       in if maybeTypeDecl == Nothing then
                             (error ("didn't declare the set" ++ name ++
                                   "in the fields of the machine." ++ name ++
                                   "is used in a broadcast"))
                          else let typeDecl = fromJust maybeTypeDecl
                               in case typeDecl of
                                   (F.Set _ elemType) -> elemType
                                   _                  -> (error ("broadcasting to machine field " ++ name
                                                                  ++ "which is not a set!"))


        (B.Decl _ elemType) = transTypeDecl thisTypeDecl

    in  elemType

bCastMsgArgs :: [B.MsgArg] ->    -- std msg args
                F.Response -> [Maybe B.MsgArg]

bCastMsgArgs stdArgs (F.Send msg _ _)
  = let
        -- get the arguments of the broadcast
        args       = argOfMsg msg

        -- take only the formal parameters
        formalArgs = map formalMsgArg args
    in
        -- look up each argument in the formal parameters
        -- this message has. We want Nothing in the place of
        -- non-existent arguments and Just the formal parameter
        -- if the message has it
        map (flip lookup (zip formalArgs formalArgs)) stdArgs

----------------------------------------------------------------

-- find the machine fields that are sets

findSets :: [F.Field] -> [B.TypeDecl]
findSets [] = []
findSets ( (F.Field typeDecl _ ):rest )
   = case typeDecl of
       F.Set _ _ -> transTypeDecl typeDecl : findSets rest
       _       -> findSets rest

--------------------------------

-- Group MachineFCases i.e. tuples (State, Guard, Maybe State, Responses)
-- by the same starting state
-- F.MachineFCase :: (F.State, F.Guard, Maybe F.State, F.Responses)
groupSameStart :: [F.MachineFCase] ->
                  [(F.State, [(F.Guard, Maybe F.State, F.Responses)])]
groupSameStart cases
  = let
        startSts = nub $ map ( \(startSt, guard, nextSt, resps)
                                -> (startSt,[]) )
                             cases

        -- to apply the helper function addMany we must transform cases to
        -- key value pairs
        makeKeyVals = map ( \(startSt, guard, nextSt, resps)
                              -> (startSt, (guard, nextSt, resps)) )
                          cases

    in
        -- group MachineFCases according to state
        addMany makeKeyVals startSts

   where

       -- we have a map where the values are lists.
       -- Given a key-value pair we look up the key and add the
       -- value to the list of this key
       addOne :: (Eq a, Eq b) => (a,b) -> [(a,[b])] -> [(a,[b])]
       addOne (k,v) list = let prevVal = fromJust $ lookup k list
                               newVal  = prevVal ++ [v]
                           in  (delete (k,prevVal) list) ++ [(k,newVal)]

       -- we apply the above to a list of key,value pairs.
       -- That is we group the values according to the keys,
       addMany :: (Eq a,Eq b) => [(a,b)] -> [(a,[b])] -> [(a,[b])]
       addMany [] final = final
       addMany (first : rest) list
         = let withFirst = addOne first list
           in  addMany rest withFirst

----------------------------------------------------------------


-- we transform all the reactions with the same starting state
receiveInst :: F.MachineType -> [F.Field]  -- the machine and its fields
            -> [B.MsgArg]                  -- standard form of msg in Murphi
            -> [F.MachineType]             -- non symmetric machines
            -> B.LocalVariables
            ------------------------
            -> (F.State, [(F.Guard, Maybe F.State, F.Responses)])
            -> B.Reaction

receiveInst machine machineFields stdArgs nonsyms locals (fState, withoutStart)
   = let
         -- get the responses and the guards of each machineFCase
         guardsResps = map singleGuard withoutStart
         startState = transState machine fState

     in
        (startState, guardsResps)

    where
        singleGuard :: (F.Guard, Maybe F.State, [F.Response])
                       -> (Maybe B.Guard, [B.Response])
        singleGuard (fGuard, fNextSt, fResps)
         = let
               bGuard = Just $ transGuard machine
                                          machineFields
                                          stdArgs
                                          nonsyms
                                          locals
                                          fGuard
               -- go to next state
               bNextSt = if fNextSt == Nothing then []
                         else [B.ToState (B.AnyType machine)
                                         (transState machine $ fromJust fNextSt)]

               bResps = map (transResponse machine
                                           machineFields
                                           stdArgs
                                           nonsyms
                                           locals)
                            fResps

                         ++ bNextSt

           in  (bGuard, bResps)

----------------------------------------------------------------


-----------------------------------------------------------------
-----------------------------------------------------------------
