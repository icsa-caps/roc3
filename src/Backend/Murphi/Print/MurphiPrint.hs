
-- functions that print the target AST to murphi source code


-- allow type synonyms and composite types to implement typeclasses
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}



module Backend.Murphi.Print.MurphiPrint(printMurphiProgram) where

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
printMurphiProgram :: Program -> String
printMurphiProgram (Program constants
                   types
                   variables
                   commonFunctions
                   machineFunctions
                   rules
                   startstate
                   invariants )

    = separatingLine ++
     "-- Constants {{{\n"
         ++ separatingLine
         ++ Cl.tomurphi constants
         ++ "\n\n-- }}}\n\n"
         ++ separatingLine ++
      "-- Types {{{\n"
          ++ separatingLine
          ++ Cl.tomurphi types
          ++ "\n\n-- }}}\n\n"
          ++ separatingLine ++
      "-- Variables {{{\n"
          ++ separatingLine
          ++ Cl.tomurphi variables
          ++ "\n\n-- }}}\n\n"
          ++ separatingLine ++
      "-- Common Functions {{{\n"
          ++ separatingLine
          ++ Cl.tomurphi commonFunctions
          ++ "\n\n-- }}}\n\n"
          ++ separatingLine ++
      "-- Machine Functions {{{\n"
          ++ separatingLine ++ "\n"
          ++ Cl.tomurphi machineFunctions
          ++ "\n\n-- }}}\n\n"
          ++ separatingLine ++
      "-- Rules {{{\n"
          ++ separatingLine
          ++ Cl.tomurphi rules
          ++ "\n\n-- }}}\n\n"
          ++ separatingLine ++
      "-- Startstate {{{\n"
          ++ separatingLine
          ++ Cl.tomurphi startstate
          ++ "\n-- }}}\n\n" -- we don't need two \n at startstate
          ++ separatingLine ++
      "-- Invariants {{{\n"
          ++ separatingLine
          ++ Cl.tomurphi invariants
          ++ "\n\n-- }}}\n\n"

      where
         separatingLine
           = "----------------------------------------" ++ -- 40 -
             "--------------------------------------\n"    -- another 40 -
-----------------------------------------------------------------
