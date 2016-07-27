-----------------------------------------------------------------
---------- tomurphi implementation for the invariants -----------
-----------------------------------------------------------------

module Invariants where

-----------------------------------------------------------------

import  MurphiAST
import qualified MurphiClass as Cl
import Data.Char
import Data.List.Split -- for tokenizing strings
                       -- splitOn is used in pushBy

-- general helper functions
import GenHelper

-- helper tomurphi implementations
import tomurphiHelper

-----------------------------------------------------------------
-----------------------------------------------------------------


instance Cl.MurphiClass Invariants where
 tomurphi = "-- haven't completed the invariants part yet.\n" ++
            "-- for the time being we need to do it by hand\n\n"


 -----------------------------------------------------------------
