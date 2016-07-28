module GetStartstate(getStartstate) where

-----------------------------------------------------------------

import qualified Ast as F
import qualified MurphiAST as B
import TransGen
-----------------------------------------------------------------
-----------------------------------------------------------------

-- data Startstate = Startstate {
--      machinesStart      ::  [(MachineType,
--                                State,
--                                [FieldStart])],
--      orderedNetsStart   ::  [OrderedNetName],
--      unorderedNetsStart :: [UnorderedNetName]
--                             }

-- final functions that transforms one AST to the other


getStartstate :: F.Ast -> B.Startstate
getStartstate frontAST = B.Startstate machinesStart
                                      orderedNets
                                      unorderedNets
   where
     orderedNets   = getOrdNetNames   frontAST
     unorderedNets = getUnordNetNames frontAST
     machinesStart = let machineNames = getMachineNames frontAST
                         startStates  = getStartstates  frontAST
                         fields       = getFieldsStart  frontAST
                     in  zip3 machineNames startStates fields




----------------------------------------------------------------
-- get the the fields of all machines, with their starting value
getFieldsStart :: F.Ast -> [[B.FieldStart]]
getFieldsStart frontAST = let machinesAllInfo = F.machines frontAST
                              frontFields     = map F.fields machinesAllInfo
                          in  map (map fieldInit) frontFields


--------------------------------

-- get the initialisation of a machine field
fieldInit :: F.Field -> B.FieldStart
fieldInit (F.Field typeDecl startVal)
   = (transTypeDecl typeDecl, startVal)

--------------------------------

----------------------------------------------------------------
----------------------------------------------------------------
