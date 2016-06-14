
-- Test tomurphi for CommonFunctions
-- rerun
-- changes have been made to some common helper functinons
-- for debugging tomurphi for Types
module CommonFunctionsExample where

import MurphiAST
import MurphiPrint
import MurphiClass


orderedNetNames = ["ord1","ord2"]

netsVCs = [("ord1",["vcOrd11","vcOrd12"]),
            ("ord2", ["vcOrd21"]),
            ("unord1",["vcUnord11","vcUnord12","vcUnord13"])]

arg1 = Decl "src" (Node "Cache")
arg2 = Decl "num" (Integer 1 10)

sendInfo = ([arg1,arg2], netsVCs)

comFun = FuncParams orderedNetNames sendInfo

murphiSrc = tomurphi comFun

-- main
main = writeFile "common_functions.txt" murphiSrc
