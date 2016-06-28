-----------------------------------------------------------------
------------------ general helper functions  --------------------
-----------------------------------------------------------------

module GenHelper where

--------------------------------

import  MurphiAST
import qualified MurphiClass as Cl
import Data.Char
import Data.List.Split -- for tokenizing strings
                       -- splitOn is used in pushBy

--------------------------------


type MachineName = String


decl :: String -> String -> String
decl iden val  = iden ++ " : " ++ val ++ ";\n"


declGen :: (Show a) => (String, a) -> String
declGen (iden,val) = decl iden (show val)


concatWith :: String -> [String] -> String
concatWith _ []       = ""  -- foldr1 doesnt support empty lists
concatWith split strs = foldr1 (\x y -> x ++ split ++ y) strs


concatln ::  [String] -> String
concatln arg = (concatWith "\n" arg )


concatcomma :: [String] -> String
concatcomma = concatWith ", "


fstCap :: String -> String
fstCap (ch:str) = (toUpper ch) : str


printEnum :: Name -> [Val] -> String
printEnum name values = name ++ ": enum { " ++
                        (concatWith (",\n" ++ alignSpace) values) ++
                        " };\n"
 where
  numSpaces  = length (name ++ ": enum { ")
  alignSpace = replicate numSpaces ' '



mapconcatln :: (a -> String) -> [a] -> String
mapconcatln f list = let indiv = map f list
                     in  concatln indiv

mapconcatlnComma :: (a -> String) -> [a] -> String
mapconcatlnComma f list = let indiv = map f list
                          in  concatWith ",\n" indiv


disjunction :: [String] -> String
disjunction = concatWith " | "


-- printing if-elsif-else-statements
printIfElse :: [String] -> [String] -> String
printIfElse (cond:conditions) (bod:bodies) = "if " ++ cond ++ " then\n  " ++
                                             bod ++ "\n\n" ++
                                             printElsif conditions bodies


-- printing elsif to else statements
printElsif :: [String] -> [String] -> String
printElsif [cond] [body]                   = "else\n  " ++ body ++ "endif;"
printElsif (cond:conditions) (body:bodies)  = "elsif " ++ cond ++ " then\n  " ++
                                             body ++ "\n\n" ++
                                             printElsif conditions bodies


-----------------------------------------------------------------
---------------- Methods for the Machine datatype ---------------
-----------------------------------------------------------------

-- most functions have two versions:
-- one taking as argument the machine name (String)
-- and another taking a machine (Machine)


getName :: Machine -> String
getName (Sym name)       = name
getName (Nonsym name _)  = name

--------------------------------

-- array of states (records with states and fields)
toMachineArrayStr :: MachineType -> String
toMachineArrayStr machine = machine ++ "s"

toMachineArray :: Machine -> String
toMachineArray = toMachineArrayStr . getName

--------------------------------

-- gives the name of the record for this machine (the state of the machine)
toMachineStateStr :: MachineType -> String
toMachineStateStr machine = machine ++ "State"

toMachineState :: Machine -> String
toMachineState = toMachineStateStr . getName

--------------------------------

-- synonym of index type, either the name of an enum (non symmetric machines)
-- or of a sclaraset (symmetric machines)
indexNameStr :: MachineType -> String
indexNameStr machine = machine ++ "Index"

indexName :: Machine -> String
indexName = indexNameStr . getName

--------------------------------

-- constant holding the size of this machine
machineSizeStr :: MachineType -> String
machineSizeStr machineName = machineName ++ "Size"

machineSize :: Machine -> String
machineSize machine = machineSizeStr $ getName machine

--------------------------------

-- index variable used as a formal parameter in functions, loops etc.
-- the latter is global; it is the value of a enum
localIndex :: Machine -> String
localIndex (Sym name)           = [head name]           -- fst letter
localIndex (Nonsym name index)  = name ++ show index

-- entry in array of machine states
-- again used as a local variable
indexedMachine :: Machine -> String
indexedMachine machine = toMachineArray machine ++ "[" ++ localIndex machine ++ "]"

-- use node as index
indexedByNode :: Machine -> String
indexedByNode machine = toMachineArray machine ++ "[node]"



----------------------------------------------------------------
----------------------------------------------------------------

-- moves each line by the specified number of spaces
pushBy :: Int -> String -> String
pushBy num = let spaces = replicate num ' '
             in  concatln . map (spaces ++ ) . splitOn "\n"
             -- filter (/=0) is for dismissing the empty new lines


----------------------------------------------------------------

-- extracting elements of triplets
fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thrd3 :: (a,b,c) -> c
thrd3 (a,b,c) = c


-- extract info from a Message
getMtype :: Message -> String
getMtype (Message mtype _ ) = mtype

getMsgParams :: Message -> [Maybe Field]
getMsgParams (Message _ params) = params

----------------------------------------------------------------


variableName :: Variable -> String
variableName (Simple varName)         = varName
variableName (ArrayElem arrayName _)  = arrayName
variableName (MachineArray machine)   = getName machine

fieldName :: Field -> String
fieldName (Field variable owner) = variableName variable

-----------------------------------------------------------------

-- how many msgs are in this buffer?
countName :: NetName -> String
countName netName = netName ++ "count"
