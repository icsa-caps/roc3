module MachineFuncEx where

import MurphiAST
import MurphiPrint
import MurphiClass


-- main
main = writeFile "machineFuncEx1.txt" murphisrc1

-----------------------------------------------------------------

sets1 :: Sets
sets1 = [Decl "sharers" (Set (Left "Cache") (Node "Cache"))]

machine1 = "Directory"

state1 = "M"
state2 = "I"
-----------------


-- messages have only one argument
msg1a = Message "GetM" [Nothing]
msg2b = Message "Ack" [ Just (Field (Simple "5") Global)]

----------------

-- receive function
resp1a :: Response
resp1a = ToState "Directory" 0 "I"

resp1b :: Response
resp1b = ToState "Directory" 0 "M"

resp1c :: Response
resp1c = Send msg2b (Field (MachineArray "Directory" 0) Global) (Field (MachineArray "Directory" 0) Global)

resp1d :: Response
resp1d = Assign (Field (Simple "acks") (Machine "Directory" 0) ) (Field (Simple "10") Global)

resp1e :: Response
resp1e = Add (Machine "Directory" 0) "sharers" (Field (Simple "true") Global)


-- group them
resps1a :: [Response]
resps1a = [resp1a,resp1c,resp1d]
resps1b = [resp1b,resp1e]

---------------
-- guards
grd1 = Receive "GetM"
grd2 = Receive "Ack"

receiveFunction :: ReceiveFunction
receiveFunction = [ ( state1, [ ( Just grd1,resps1a ) ] ),
                    ( state2, [ ( Just grd2,resps1b ) ] )
                   ]

machineFunctions = MachineFunctions [ ( machine1, sets1, receiveFunction, [Decl "temp" (Node "Cache")])]
                                    


murphisrc1 = tomurphi machineFunctions
