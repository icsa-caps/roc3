-- test for Types
module TypesExample where

import MurphiAST
import MurphiClass
import MurphiPrint


cache = ("Cache", 5)
dir   = ("Directory", 1)

machineSize = [cache,dir]

node = ["Cache", "Directory"]

msgTypes = ["Ack", "Fwd"]

msgArg1 = Decl "src" (Node "Cache")
msgArg2 = Decl "dst" (Node "Cache")
msgArg3 = Decl "Acks_received" (Integer 0 5)

msgArguments = [msgArg1, msgArg2, msgArg3]

cacheState = ( "Cache",
             ["I","M","IM","S","MI"],
             [Decl "ack_rec" (Integer 0 5)] )

dirState = ( "Directory",
             ["I","M","IM"],
             [Decl "owner" (Node "Cache"), Decl "ack_rec" (Integer 0 5)] )

allMacStates = [cacheState, dirState]

types = Types machineSize node msgTypes msgArguments allMacStates

murphiSrc = tomurphi types


-- main
main = writeFile "types.txt" murphiSrc
