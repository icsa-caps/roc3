-----------------------------------------------------------------
---------- Functions that extract essential information ---------
-------------------- from the front-end AST ---------------------

module TransGen where

-----------------------------------------------------------------

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

----------------------------------------------------------------

-- functions for low-level constructs (e.g. responses, type declarations etc.)

-------------------------------
Transforming type declarations
--------------------------------

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
getVCNames frontAST = let channelsAllInfo = F.channels frontAST
                      in  map (\ (F.Channel name) -> name) channelsAllInfo

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

getStartstate :: F.Ast -> [B.State]
getStartstate frontAST = let machinesAllInfo = F.machines frontAST
                         in  map (getStateName . F.startstate) machinesAllInfo



--------------------------------



--------------------------------


----------------------------------------------------------------
----------------------------------------------------------------
