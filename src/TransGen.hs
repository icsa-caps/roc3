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
isOrdered _                     = False

--------------------------------

backSymmetry :: F.Symmetry -> B.Symmetry
backSymmetry (F.Symmetric)    = B.Symmetric
backSymmetry (F.Nonsymmetric) = B.Nonsymmetric

--------------------------------

getStateName :: F.State -> String
getStateName (F.State name) = name

-------------------------------

transState :: F.State -> B.State
transState = getStateName         -- currently B.State = String

-------------------------------

-- get the vcs of a network
getChanNet :: F.Network -> [F.VC]
getChanNet (F.Network order name vcs) = vcs

--------------------------------


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

arrayOrMachine :: [F.MachineType] -> [F.MachineType] -> F.Param -> F.Param
arrayOrMachine syms nonsyms (ArrayElem name index)
    | name `elem` syms      = error "trying to index symmetric machine"
    | name `elem` nonsyms   = NonSymInst name index -- instance of nonsym
    | otherwise             = ArrayElem name index  -- indeed array elem

--------------------------------

-- getting from the fronend type declaration to the backend type declaration
transTypeDecl :: F.TypeDecl -> B.TypeDecl
transTypeDecl frontTypeDecl = B.Decl (getTypeDeclName frontTypeDecl)
                                     (getType frontTypeDecl)

--------------------------------

findLocal :: [F.Response] -> B.LocalVariables    -- LocalVariables = [TypeDecl]
findLocal resps =
    let maybes = map findLocalSingle resps
        noNothing = filter (/=Nothing) maybes
    in  nub $ map (fromJust) noNothing

    where
        findLocalSingle :: F.Response -> Maybe B.TypeDecl
        findLocalSingle (AssignLocal typeDecl _)
           = Just $ transTypeDecl typeDecl
        findLocalSingle (AssignLocalNum typeDecl _)
           = Just $ transTypeDecl typeDecl
        findLocalSingle _
           = Nothing


--------------------------------


transVar :: F.MachineType -> [F.Fields]  -- the machine and its fields
            -> [B.MsgArg]                -- standard form of msg in Murphi
            -> B.LocalVariables
            -> F.Param -> B.Field
transVar machine machineFields stdMsgArgs locals (VarOrVal var)

  = let fieldsNames  = map (\(Field typeDecl _) -> getTypeDeclName typeDecl)
                           machineFields

        msgArgsNames = map (\(Decl name _) -> name)
                           stdMsgArgs

        localNames   = map (\(Decl name _) -> name)
                           locals

    in if var `elem` fieldsNames
            then (Field (Simple var) (Owner (AnyType machine)))
       else
         if var `elem` msgArgsNames
             then (Field (Simple var) (Owner Msg))
         else
            if var `elem` localNames
                then (Field (Simple var) (Local))
            else
                (Field (Simple var) (Global)) -- it's a constant





transResponse :: F.Response -> B.Response
transResponse resp =

  where

--------------------------------

----------------------------------------------------------------
----------------------------------------------------------------

-- Getting names and sizes (non machine specific)

--------------------------------


getVCNames :: F.Ast -> [B.VCName]
getVCNames fAst = let nets = F.networks fAst
                      vcs  = map ( \(F.Network _ _ vcs) -> vcs ) nets
                  in  map (\(F.VC name) -> name) $ concat vcs

--------------------------------

getAllNetNames :: F.Ast -> [B.NetName]
getAllNetNames fAst = let nets = F.networks fAst
                      in  map ( \(F.Network _ name _) -> name ) nets

--------------------------------

getOrdNetNames :: F.Ast -> [B.OrderedNetName]
getOrdNetNames fAst = let allNets = F.networks fAst
                          onlyOrdered = filter isOrdered allNets
                      in  map( \(F.Network _ name _) -> name ) onlyOrdered

--------------------------------

getUnordNetNames :: F.Ast -> [B.UnorderedNetName]
getUnordNetNames fAst = let allNets = F.networks fAst
                            onlyUnordered = filter (not . isOrdered) allNets
                        in  map( \(F.Network _ name _) -> name ) onlyUnordered

--------------------------------

getNetsVCs :: F.Ast -> [(B.NetName, [B.VCName])]
getNetsVCs fAst = let nets     = F.networks fAst
                      vcNames  = map vcsFromNet nets
                      netNames = map (\(F.Network _ name _) -> name)
                                     nets
                  in  zip netNames vcNames
  where
    vcsFromNet :: F.Network -> [B.VCName]
    vcsFromNet (F.Network _ _ vcs)
      = map (\(F.VC name) -> name) vcs

--------------------------------


-----------------------------------------------------------------
-----------------------------------------------------------------

---------------------------- Machines ---------------------------


-----------------------------------------------------------------


--------------------------------

getMachineNames :: F.Ast -> [B.MachineType]
getMachineNames fAst = let machinesAllInfo = F.machines fAst
                       in  map F.machineType machinesAllInfo

--------------------------------

getMachineSizes :: F.Ast -> [B.Size]
getMachineSizes fAst = let machinesAllInfo = F.machines fAst
                       in  map F.size machinesAllInfo

--------------------------------

getSymmetries :: F.Ast -> [B.Symmetry]
getSymmetries fAst = let machinesAllInfo = F.machines fAst
                         frontSymmetries = map F.symmetry machinesAllInfo
                     in  map backSymmetry frontSymmetries

--------------------------------

getStartstates :: F.Ast -> [B.State]
getStartstates fAst = let machinesAllInfo = F.machines fAst
                      in  map (getStateName . F.startstate) machinesAllInfo



--------------------------------

getMachineFields :: F.Ast -> [[B.TypeDecl]]
-- fields are declared as TypeDecl in MurphiAST
getMachineFields fAst = let machines  = F.machines fAst
                            fFields   = map F.fields machines
                            typeDecls = map (map (\(F.Field typeDecl _)
                                                    -> typeDecl))
                                            fFields
                        in  map (map transTypeDecl) typeDecls

--------------------------------

-- we don't include here a function for machine states because we may want
-- to have either a list with no duplicates in GetTypes or with duplicates,
-- if any, in MachineFunctions. The latter case is due to the fact that
-- the transition to another state doesn't depend only on the current state


-----------------------------------------------------------------
-----------------------------------------------------------------
