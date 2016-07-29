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
concatln  = concatWith "\n"


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
printIfElse (cond:conditions) (body:bodies) = "if " ++ cond ++ " then\n" ++
                                             (pushBy 2 body) ++ "\n" ++
                                             printElsif conditions bodies


-- printing elsif to else statements
printElsif :: [String] -> [String] -> String
printElsif [cond] [body]                    = "else\n" ++ (pushBy 2 body)
                                              ++ "\n" ++ "endif;\n"
printElsif [] _                             = "endif;\n"
printElsif (cond:conditions) (body:bodies)  = "elsif " ++ cond ++ " then\n" ++
                                             (pushBy 2 body) ++ "\n" ++
                                             printElsif conditions bodies


-----------------------------------------------------------------
---------------------- Methods for Machines ---------------------
-----------------------------------------------------------------

-- most functions have two versions:
-- one taking as argument the machine name (String)
-- and another taking a machine (Machine)


machineName :: Machine -> String
machineName (AnyType name)   = name
machineName (Sym name)       = name
machineName (Nonsym name _)  = name

--------------------------------

-- array of states (records with states and fields)
toMachineArrayStr :: MachineType -> String
toMachineArrayStr machine = machine ++ "s"

toMachineArray :: Machine -> String
toMachineArray = toMachineArrayStr . machineName

--------------------------------

-- gives the name of the record for this machine (the state of the machine)
toMachineStateStr :: MachineType -> String
toMachineStateStr machine = machine ++ "State"

toMachineState :: Machine -> String
toMachineState = toMachineStateStr . machineName

--------------------------------


-- formal parameter for indexing
formalIndexStr :: MachineType -> String
formalIndexStr machine = machine ++ "IndexVar"

formalIndex :: Machine -> String
formalIndex = formalIndexStr . machineName

-- refering to machine indexed in a
indexedFormalStr :: MachineType -> String
indexedFormalStr machine = toMachineArrayStr machine ++
                                 "[" ++ formalIndexStr machine ++ "]"

-- the type synonym of the indexing variables
-- i.e. scalarset for symmetric and Enum for nonsymmetric
-- machines. To iterate over all of the indices of a machine write
-- formalIndexStr machine ++ ":" ++ indexTypeStr machine

indexTypeStr :: MachineType -> String
indexTypeStr machine = machine ++ "IndexType"

indexType :: Machine -> String
indexType = indexTypeStr . machineName

--------------------------------

-- constant holding the size of this machine
machineSizeStr :: MachineType -> String
machineSizeStr machine = machine ++ "Size"

machineSize :: Machine -> String
machineSize machine = machineSizeStr $ machineName machine

--------------------------------

-- index variable used as a formal parameter in functions, loops etc.
-- the latter is global; it is the value of a enum
generalIndex :: Machine -> String
generalIndex (AnyType name)    = name ++ "IndexVar"
generalIndex (Sym name)        = name ++ "IndexVar"           -- fst letter
generalIndex (Nonsym name index)
   = case index of (Specific num) -> name ++ show num
                   (Arbitrary)    -> name ++ "IndexVar"


-- entry in array of machine states
-- again used as a local variable
indexedMachineGen :: Machine -> String
-- when we use a synonym for the machine in some context
-- (e.g. src, dst)
indexedMachineGen (Synonym var) = var
indexedMachineGen machine = toMachineArray machine ++
                        "[" ++ generalIndex machine ++ "]"

-- use node as index
indexedByNode :: Machine -> String
indexedByNode machine = toMachineArray machine ++ "[node]"

--------------------------------

-- if this field is a machine index,
-- hold the same info in the constructor  JustInde,
-- so that just the relevant index is printed,
-- and not the array corresponding to the machine indexed
onlyIndex :: Field -> Field
onlyIndex (Field (MachineArray machine) Global)
    = JustIndex machine Nothing -- we don't know index of nonsym

onlyIndex (Field (MachineIndex machine index) Global )
    = JustIndex machine (Just index)

onlyIndex other = other     -- don't alter other Field

--------------------------------

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
variableName (MachineArray machine)   = machine

fieldName :: Field -> String
fieldName (Field variable owner) = variableName variable

-----------------------------------------------------------------

-- how many msgs are in this buffer?
countName :: NetName -> String
countName netName = netName ++ "count"
