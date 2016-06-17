module MachineFuncEx where

import MurphiAST
import MurphiPrint
import MurphiClass

-- 1st simple example
sets1 = []
machine1 = "Directory"

state1 = "M"
state2 = "I"
-----------------

-- messages have only one argument
msg1a = Message "GetM" [Nothing]
msg2b = Message "Ack" [ Just (Field "5" Global)]

----------------

-- receive function
resp1a = ToState "I"
resp1b = ToState "M"
resp1c = Send msg1a (Field "Directory" Global) (Field "Directory" Global)
resp1d = Assign ( Field "acks" (Machine "Directory") ) (Right "10")
resp1e = Add "sharers" (Right "true")
-- group them
resps1a = [resp1a,resp1c,resp1d]
resps1b = [resp1b,resp1e]

---------------
-- guards
grd1 = Receive "GetM"
grd2 = Receive "Ack"

receiveFunction = [ ( state1, [ ( Just grd1,resps1a ) ] ),
                    ( state2, [ ( Just grd2,resps1b ) ] )
                   ]

machineFunctions = MachineFunctions [ ( machine1, sets1, receiveFunction ) ]


murphisrc1 = tomurphi machineFunctions


-- main
main = writeFile "machineFuncEx1.txt" murphisrc1
