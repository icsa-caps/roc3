--------------------------- Responses ---------------------------
-----------------------------------------------------------------

module TransResponse where

-----------------------------------------------------------------
-----------------------------------------------------------------

import qualified Ast as F
import qualified MurphiAST as B
import TransGen
import TransMsg
import Data.Maybe -- fromJust

-----------------------------------------------------------------
-----------------------------------------------------------------


transExp :: F.MachineType -> [F.Field]  -- the machine and its fields
            -> [B.MsgArg]               -- standard form of msg in Murphi
            -> [F.MachineType]          -- non symmetric machines
            -> B.LocalVariables
            -> F.IntExp -> B.IntExp
transExp machine fields stdArgs nonsyms locals (F.Sum   exp1 exp2)
  = B.Sum   (transExp machine fields stdArgs nonsyms locals exp1)
            (transExp machine fields stdArgs nonsyms locals exp2)

transExp machine fields stdArgs nonsyms locals (F.Minus exp1 exp2)
  = B.Minus (transExp machine fields stdArgs nonsyms locals exp1)
            (transExp machine fields stdArgs nonsyms locals exp2)

transExp machine fields stdArgs nonsyms locals (F.Times exp1 exp2)
  = B.Times (transExp machine fields stdArgs nonsyms locals exp1)
            (transExp machine fields stdArgs nonsyms locals exp2)

transExp machine fields stdArgs nonsyms locals (F.Div   exp1 exp2)
  = B.Div   (transExp machine fields stdArgs nonsyms locals exp1)
            (transExp machine fields stdArgs nonsyms locals exp2)

transExp machine fields stdArgs nonsyms locals (F.Group exp)
  = B.Group (transExp machine fields stdArgs nonsyms locals exp)

transExp machine fields stdArgs nonsyms locals (F.Const num)
  = B.Const num

transExp machine fields stdArgs nonsyms locals (F.IntVar param)
  = B.IntVar (transVar machine fields stdArgs nonsyms locals param)


-----------------------------------------------------------------
-----------------------------------------------------------------


transResponse :: F.MachineType -> [F.Field]  -- the machine and its fields
                 -> [B.MsgArg]               -- standard form of msg in Murphi
                 -> [F.MachineType]          -- non symmetric machines
                 -> B.LocalVariables
                 -> F.Response -> B.Response
transResponse machine machineFields stdArgs nonsyms locals resp
  = case resp of
      F.Stall                           ->  B.Stall
      F.Send msg dst vc                 ->  let bMsg = transMsg machine
                                                                machineFields
                                                                stdArgs
                                                                nonsyms
                                                                locals
                                                                msg
                                                vcName = getVCName vc
                                                -- we are in receive function/
                                                -- rules. We index the machine
                                                -- with general index
                                                bSrc = B.Field
                                                        (B.MachineArray machine)
                                                        B.Global
                                                bDst = transVar machine
                                                                machineFields
                                                                stdArgs
                                                                nonsyms
                                                                locals
                                                                dst

                                                -- do we send to one machine or
                                                -- to a set?
                                                isBroadcast = isBCast machineFields
                                                                      stdArgs
                                                                      locals
                                                                      resp

                                            in  if isBroadcast then
                                                    B.Broadcast bMsg bSrc bDst vcName
                                                else
                                                    B.Send bMsg bSrc bDst vcName


      F.Add setName (Left param )       ->  let elem = transVar machine
                                                                machineFields
                                                                stdArgs
                                                                nonsyms
                                                                locals
                                                                param
                                                -- we can add/remove elements of
                                                -- sets only from the machine
                                                -- that owns them
                                                owner = B.Owner $ B.AnyType machine

                                            in  B.Add owner setName elem

      F.Add setName (Right num)         ->  let elem  = B.Field (B.Simple (show num))
                                                                 B.Global
                                                owner = B.Owner $ B.AnyType machine
                                            in  B.Add owner setName elem

      F.Del setName (Left param )       ->  let elem  = transVar machine
                                                                 machineFields
                                                                 stdArgs
                                                                 nonsyms
                                                                 locals
                                                                 param
                                                owner = B.Owner $ B.AnyType machine
                                            in  B.Del owner setName elem

      F.Del setName (Right num)         ->  let elem  = B.Field (B.Simple (show num))
                                                                B.Global
                                                owner = B.Owner $ B.AnyType machine
                                            in  B.Del owner setName elem

      F.Assign param1 param2            ->  let field1 = transVar machine
                                                                  machineFields
                                                                  stdArgs
                                                                  nonsyms
                                                                  locals
                                                                  param1

                                                field2 = transVar machine
                                                                  machineFields
                                                                  stdArgs
                                                                  nonsyms
                                                                  locals
                                                                  param2

                                            in  B.Assign field1 field2

      F.AssignNum param intExp          ->  let field   = transVar machine
                                                                   machineFields
                                                                   stdArgs
                                                                   nonsyms
                                                                   locals
                                                                   param

                                                bIntExp = transExp machine
                                                                   machineFields
                                                                   stdArgs
                                                                   nonsyms
                                                                   locals
                                                                   intExp

                                            in  B.AssignInt field bIntExp

      F.AssignLocal typeDecl param      ->  let name = getTypeDeclName typeDecl
                                                -- make the typeDecl param
                                                param1 = F.VarOrVal name
                                                -- and let transVar make it a
                                                -- field (and figure out it's
                                                -- local)
                                                field = transVar machine
                                                                 machineFields
                                                                 stdArgs
                                                                 nonsyms
                                                                 locals
                                                                 param1

                                                value = transVar machine
                                                                 machineFields
                                                                 stdArgs
                                                                 nonsyms
                                                                 locals
                                                                 param

                                            in  B.Assign field value

      F.AssignLocalNum typeDecl intExp  ->  let name = getTypeDeclName typeDecl
                                                -- make the typeDecl param
                                                param1 = F.VarOrVal name
                                                -- and let transVar make it a
                                                -- field (and figure out it's
                                                -- local)
                                                field = transVar machine
                                                                 machineFields
                                                                 stdArgs
                                                                 nonsyms
                                                                 locals
                                                                 param1

                                                bExp  = transExp machine
                                                                 machineFields
                                                                 stdArgs
                                                                 nonsyms
                                                                 locals
                                                                 intExp

                                            in  B.AssignInt field bExp

      F.EmptyResp str                   ->  B.EmptyResp str
--------------------------------

----------------------------------------------------------------
----------------------------------------------------------------

-- helper function for distinguishing send from broadcast
isSet :: [F.Field] -> [B.MsgArg] -> B.LocalVariables
         -> F.Param -> Bool

isSet _ _ _ (F.NonSymInst _ _) = False
isSet fields msgArgs locals (F.VarOrVal name)
   = let allPairs = allNamesTypes fields msgArgs locals
         value = lookup name allPairs

    in   if value == Nothing then False
         else simpleIsSet $ fromJust value


isSet fields msgArgs locals (F.ArrayElem name _)
   = let allPairs = allNamesTypes fields msgArgs locals
         value = lookup name allPairs

    in   if value == Nothing then False
         else isArrayOfSets $ fromJust value




simpleIsSet :: B.Type -> Bool
simpleIsSet (B.Set _ _) = True
simpleIsSet _           = False

isArrayOfSets :: B.Type -> Bool
isArrayOfSets (B.Array _ (B.Set _ _)) = True
isArrayOfSets _                       = False


allNamesTypes :: [F.Field] -> [B.MsgArg] -> B.LocalVariables
                 -> [(B.Name, B.Type)]
allNamesTypes fields msgArgs locals
  = let machinePairs = namesTypesF $ map (\ (F.Field typeDecl _) -> typeDecl )
                                         fields
        msgArgPairs = namesTypesB msgArgs
        localPairs  = namesTypesB locals
        -- list of all (name,type)
    in  machinePairs ++ msgArgPairs ++ localPairs
    where

        namesTypesB :: [B.TypeDecl] -> [(B.Name, B.Type)]
        namesTypesB typeDecls = map ( \(B.Decl name btype) -> (name,btype)  )
                                   typeDecls

        namesTypesF :: [F.TypeDecl] -> [(B.Name, B.Type)]
        namesTypesF typeDecls = namesTypesB $ map transTypeDecl typeDecls


isBCast :: [F.Field] -> [B.MsgArg]
            -> B.LocalVariables ->
            F.Response -> Bool

isBCast fields stdArgs locals (F.Send _ dst _)
  = isSet fields stdArgs locals dst

isBCast _ _ _ _ = False




-----------------------------------------------------------------
-----------------------------------------------------------------
