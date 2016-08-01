-----------------------------------------------------------------
---------- Functions that extract essential information ---------
-------------------- from the front-end AST ---------------------

module TransGen where

-----------------------------------------------------------------

import Data.Maybe -- for fromJust
import Data.List  -- for nub
import Data.Char  -- for toUpper
import qualified Ast as F
import qualified MurphiAST as B


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

getVCName :: F.VC -> String
getVCName (F.VC name) = name

-------------------------------

-- e.g. if I is a state for both Cache and Directory,
-- in murphi the directory state will be Directory_I and
-- for the Cache Cache_I
transState :: F.MachineType -> F.State -> B.State
transState machine state = machine ++ '_' : getStateName state

-------------------------------

-- get the vcs of a network
getChanNet :: F.Network -> [F.VC]
getChanNet (F.Network order name vcs) = vcs

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

findLocal :: [F.Response] -> B.LocalVariables    -- LocalVariables = [TypeDecl]
findLocal resps =
    let maybes    = map findLocalSingle resps
        noNothing = filter (/=Nothing) maybes
    in  nub $ map (fromJust) noNothing

    where
        findLocalSingle :: F.Response -> Maybe B.TypeDecl
        findLocalSingle (F.AssignLocal typeDecl _)
           = Just $ transTypeDecl typeDecl
        findLocalSingle (F.AssignLocalNum typeDecl _)
           = Just $ transTypeDecl typeDecl
        findLocalSingle _
           = Nothing


--------------------------------
-- given an F.Param (variable, array element, constant) and the
-- necessary context, get the equivalent in Murphi
-- essentially we use the context to determine the ownership of variables
-- if the F.Param is the instance of a non-symmetric machine,
-- we must use transNonSymInst. So we take cases

transVar :: F.MachineType -> [F.Field]   -- the machine and its fields
            -> [B.MsgArg]                -- standard form of msg in Murphi
            -> [F.MachineType]           -- non symmetric machines
            -> B.LocalVariables
            -> F.Param -> B.Field

transVar _ _ _ _ _ (F.NonSymInst machine index) -- redundant
   = B.Field (B.NonsymIndex machine index) B.Global

transVar machine machineFields stdMsgArgs nonsyms locals param

  = let fieldsNames  = map (\(F.Field typeDecl _) -> getTypeDeclName typeDecl)
                           machineFields

        msgArgsNames = map (\(B.Decl name _) -> name)
                           stdMsgArgs
                       ++ ["src"] -- src in responses in frontend refers to
                                -- the msg src, so in murphi it's msg.src

        localNames   = map (\(B.Decl name _) -> name)
                           locals

        name = varName param

        finalParam = if name `elem` nonsyms then machineFromArray param
                     else param

        var = varFromParam finalParam

    in if name `elem` fieldsNames             -- machine field
            then (B.Field var (B.Owner (B.AnyType machine)))
       else
         if name `elem` msgArgsNames          -- message argument
             then (B.Field var B.Msg)
         else
            if name `elem` localNames         -- local variable
                then (B.Field var B.Local)
            else
                (B.Field var B.Global) -- constant, instance of machine


--------------------------------

-- the parser by default parses expressions <identifier> [<num>]
-- as arrays. But they may refer to an instance of a non-symmetric machine
machineFromArray :: F.Param -> F.Param
machineFromArray (F.ArrayElem name index) = F.NonSymInst name index

isArray :: F.Param -> Bool
isArray (F.ArrayElem _ _) = True
isArray _               = False

--------------------------------

varFromParam :: F.Param -> B.Variable
varFromParam (F.ArrayElem arrayName index) = B.ArrayElem arrayName index
varFromParam (F.VarOrVal iden)             = B.Simple iden
varFromParam (F.NonSymInst machine index)  = B.NonsymIndex machine index

-- the identifier for an array, a variable, a constant or an instance of
-- a machine
varName :: F.Param -> String
varName (F.ArrayElem arrayName _)     = arrayName
varName (F.VarOrVal iden)             = iden
varName (F.NonSymInst machine index)  = machine


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

-- returns the list of nonsymmetric machines.
-- we need it to transform array elements to
-- instances of machines when needed
getNonsyms :: F.Ast -> [F.MachineType]
getNonsyms fAst = let machinesAllInfo = F.machines fAst
                      allMachines   = map F.machineType machinesAllInfo
                      allSymmetries = map F.symmetry machinesAllInfo
                      pairs         = zip allMachines allSymmetries

                  in map fst $ filter ( \(m,sym) -> sym == F.Nonsymmetric )
                                      pairs

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
                          fStartstates    = map F.startstate machinesAllInfo
                          machineNames    = map F.machineType machinesAllInfo
                          machinesStartSt = zip machineNames fStartstates
                      in  map (uncurry transState) machinesStartSt

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
----------------------------------------------------------------

-- we don't include here a function for machine states because we may want
-- to have either a list with no duplicates in GetTypes or with duplicates,
-- if any, in MachineFunctions. The latter case is due to the fact that
-- the transition to another state doesn't depend only on the current state


----------------------------------------------------------------

-- MachineFCase is an instance of the receive function
-- i.e. a combination of state and guard with a bunch
-- of responses. It can be a single self-issued rule if
-- the guard is a self-issued message

isSelfIssued :: F.MachineFCase -> Bool
isSelfIssued (stateInit, guard, stateAfter, resps)
 = case guard of
      F.Issue _ -> True
      _         -> False



-----------------------------------------------------------------
-----------------------------------------------------------------
