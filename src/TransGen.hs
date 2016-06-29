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
                         in  map startstate machinesAllInfo

--------------------------------



--------------------------------


----------------------------------------------------------------
----------------------------------------------------------------
