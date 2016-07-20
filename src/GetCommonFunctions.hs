module GetCommonFunctions(getCommonFunctions) where

-----------------------------------------------------------------

import TransGen
import qualified Ast as F
import qualified Backend.Murphi.MurphiAST as B
import Data.List  -- for nub

-----------------------------------------------------------------
-----------------------------------------------------------------
-- data CommonFunctions = FuncParams {
--                        advanceQ        :: [OrderedNetName],
--                        send            :: ( MsgArgs, [(NetName,[VCName])] ),
--                       } deriving(Show,Eq)

getCommonFunctions :: F.Ast -> B.CommonFunctions
getCommonFunctions fAst = B.CommonFunctions advanceQ send
  where
    advanceQ = getOrdNetNames fAst

    msgArgs = stdMsgArgs fAst
    netVCs = getNetsVCs fAst
    send = (msgArgs, netVCs)
