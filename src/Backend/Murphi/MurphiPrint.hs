
-- functions that print the target AST to murphi source code


-- allow type synonyms and composite types to implement typeclasses
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}



module Backend.Murphi.MurphiPrint where

import  MurphiAST
import qualified MurphiClass as Cl


-- tomurphi implementations of each part of a muprhi program
import Types    -- this is the types section of Murphi, not Haskell types!
import Constants
import Variables
import CommonFunctions
import MachineFunctions
import Rules
import Startstate
import Invariants

-----------------------------------------------------------------


-----------------------------------------------------------------
----------------- Main tomurphi implementation -----------------
-----------------------------------------------------------------



-- top printing method for murphi
tomurphiTop :: Program -> String
tomurphiTop (Program constants
                   types
                   variables
                   commonFunctions
                   machineFunctions
                   rules
                   startstate
                   invariants )

    = "\n-- Constants {{{\n\n"
          ++ Cl.tomurphi constants
          ++ "\n\n-- }}}\n" ++
      "\n-- Types {{{\n\n"
          ++ Cl.tomurphi types
          ++ "\n\n-- }}}\n" ++
      "\n-- Variables {{{\n\n"
          ++ Cl.tomurphi variables
          ++ "\n\n-- }}}\n" ++
      "\n-- Common Functions {{{\n\n"
          ++ Cl.tomurphi commonFunctions
          ++ "\n\n-- }}}\n" ++
      "\n-- Machine Functions {{{\n\n"
          ++ Cl.tomurphi machineFunctions
          ++ "\n\n-- }}}\n" ++
      "\n-- Rules {{{\n\n"
          ++ Cl.tomurphi rules
          ++ "\n\n-- }}}\n" ++
      "\n-- Startstate {{{\n\n"
          ++ Cl.tomurphi startstate
          ++ "\n-- }}}\n" ++       -- we don't need two \n at startstate
      "\n-- Invariants {{{\n\n"
          ++ Cl.tomurphi invariants
          ++ "\n\n-- }}}\n"


-----------------------------------------------------------------
