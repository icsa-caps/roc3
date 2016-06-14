-- Test tomurphi for Variables

module VariablesExample where

import MurphiAST
import MurphiPrint
import MurphiClass

cacheDir = ["Cache","Directory"]

ordNet   = OrderedNet   "MsgQ" 5

unordNet = UnorderedNet "Acks" 5

variables = Variables cacheDir [ordNet] [unordNet]

murphiSrc = tomurphi variables

main = writeFile "variables.txt" murphiSrc
