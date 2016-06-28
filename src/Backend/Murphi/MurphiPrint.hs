
-- functions that print the target AST to murphi source code


-- allow type synonyms and composite types to implement typeclasses
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}



module MurphiPrint where

import  MurphiAST
import qualified MurphiClass as Cl
import Data.Char
import Data.List.Split -- for tokenizing strings
                       -- splitOn is used in pushBy

-- general helper functions
import GenHelper

-- helper tomurphi implementations
import tomurphiHelper


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



instance Cl.MurphiClass Program where

 --tomurphi :: Program -> String
 tomurphi (Program constants
                   types
                   variables
                   commonFunctions
                   machineFunctions
                   rules
                   startstate
                   invariants )

    = "-- Constants {{{\n"          ++ Cl.tomurphi constants         ++ "-- }}}" ++
      "-- Types {{{\n"              ++ Cl.tomurphi types             ++ "-- }}}" ++
      "-- Variables {{{\n"          ++ Cl.tomurphi variables         ++ "-- }}}" ++
      "-- Common Functions {{{\n"   ++ Cl.tomurphi commonFunctions   ++ "-- }}}" ++
      "-- Machine Functions {{{\n"  ++ Cl.tomurphi machineFunctions  ++ "-- }}}" ++
      "-- Rules {{{\n"              ++ Cl.tomurphi rules             ++ "-- }}}" ++
      "-- Startstate {{{\n"         ++ Cl.tomurphi startstate        ++ "-- }}}" ++
      "-- Invariants {{{\n"         ++ Cl.tomurphi invariants        ++ "-- }}}"


-----------------------------------------------------------------
