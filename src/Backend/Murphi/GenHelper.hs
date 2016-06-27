-----------------------------------------------------------------
------------------ general helper functions  --------------------
-----------------------------------------------------------------

module GenHelper where

--------------------------------

import MurphiAST
import MurphiClass

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


----------------------------------------------------------------

-- gives the name of the variable used for indexing a symmetric machine
-- i.e. the name of the corresponseing scalarset
toMachineScalar :: Machine -> String
toMachineScalar (Symmetric machine _) = machine ++ "Scalar"

-- used for indexing a machine
toMachineIndex :: MachineType -> String
toMachineIndex machine = machine ++ "Index"

-- the name of the array of this type of machines
toMachineArray :: MachineType -> String
toMachineArray machine = machine ++ "s"


-- synonym of index type, either the name of an enum (non symmetric machines)
-- or of a sclaraset (symmetric machines)
indexName machine   = case machine of
                      (Symmetric _ _)     -> toMachineScalar machine
                      (Nonsym name _) -> name



-- gives the name of the record for this machine (the state of the machine)
toMachineState :: Machine -> String
toMachineState machine = machine ++ "State"

-- retrieve the name of a machine (which is not an alias)
getMachineType :: Machine -> String
getMachineType (Machine machineType) = machineType
getMachineType (Nonsym machineType num) = machineType

----------------------------------------------------------------


-- moves each line by the specified number of spaces
pushBy :: Int -> String -> String
pushBy num = let spaces = replicate num ' '
             in  concatln . map (spaces ++ ) . splitOn "\n"
             -- filter (/=0) is for dismissing the empty new lines


----------------------------------------------------------------

-- extracting elements of triplets
fst3 :: (a,b,c,d) -> a
fst3 (a,b,c,d) = a

snd3 :: (a,b,c,d) -> b
snd3 (a,b,c,d) = b

thrd3 :: (a,b,c,d) -> c
thrd3 (a,b,c,d) = c


-- extract info from a Message
getMtype :: Message -> String
getMtype (Message mtype _ ) = mtype

getMsgParams :: Message -> [Maybe Field]
getMsgParams (Message _ params) = params

----------------------------------------------------------------


variableName :: Variable -> String
variableName (Simple varName)              = varName
variableName (ArrayElem arrayName _)       = arrayName
variableName (MachineArray machineName _)  = machineName

fieldName :: Field -> String
fieldName (Field variable owner) = variableName variable

-----------------------------------------------------------------

-- how many msgs are in this buffer?
countName :: NetName -> String
countName netName = netName ++ "count"
