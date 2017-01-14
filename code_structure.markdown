# Introduction
Our application is a translator from the roc3 language to murphi (if this doesn't make sense read the README). Here we describe how the code is structured inside the src folder and subfolders. The directory structure is such that it's easy to extend the application to include other frontends and backends.

------------------

# src
The only file is *main.hs*, the entry point of the application.

------------------

# src/frontend
Here do and will reside files relevant to the frontend language (currently only roc3).


## src/frontend/roc3
This directory contains the files
1. *Ast.hs* haskell file containing the datatypes for the Ast of roc3.
2. *Lexer.x* alex file that has the lexer for roc3.
3. *Parser.y, Parser.hs* Parser.y is the parser of roc3, written in happy, a parser generator for haskell. the .hs file is obviously the haskell generated file. Although is should be generated automatically from the cabal file it's not, so if you want to make changes to the parser make sure to type afterwards at the terminal *happy Parser.y* to generate a new haskell file.

------------------

# src/backend
Here do and will reside files relevant to the backend language (currently only murphi). The approach we followed for murphi is one directory with files for translating the roc3 AST to the murphi AST and another directory for printing murphi code from the murphi AST.

In both folders, the files are organised around the structure of the targeted murphi skeleton, which is based on the protocol framework from `http://users.ece.cmu.edu/~bgold/teaching/coherence.html`; the skeleton consists of the following sections: constants, types, variables, common functions (functions used by all machines e.g. for sending messages), machine functions (e.g. the function describing how a machine reacts to a message), rules and invariants. (The invariants section is currently not generated from our application)

----------------

## src/backend/murphi
The murphi directory has the subdirectories AST_Translation (files for transforming ASTs) and Print(for printing code from murphi AST).

### src/backend/murphi/AST_Translation
The files that start with a *Get* have functions for constructing the relevant part of the murphi AST from a roc3 AST.
1. *Ast.hs* copy of *Ast.hs* in frontend/roc3; necessary for functions for translating the roc3 AST to murphi AST
2. *GetCommonFunctions.hs*
3. *GetConstants.hs*
4. *GetInvariants.hs*
5. *GetMachineFunctions.hs*
6. *GetRules.hs* currently incomplete
7. *GetStartstate.hs*
8. *GetTypes.hs*
9. *GetVariables.hs*
10. *MurphiAST.hs* contains the datatypes for the murphi AST
11. *TransGen.hs* translation functions for various constructs
12. *TransGuard.hs* translation functions for guards and their components
13. *TransMsg.hs* functions for translating messages and obtaining information from messages
14. *TransMurphi.hs* the file that puts all translation functions together and generates the equivalent murphi AST out of a roc3 AST.
15. *TransResponse.hs* translates responses (e.g. variable assignment, sending/broadcasting a message etc.)

----------------

### src/backend/murphi/Print
The files that have the name of one of the section of the skeleton murphi file print that part of the skeleton. (e.g. CommonFunctions.hs prints the common functions section)
1. *CommonFunctions.hs*
2. *Constants.hs*
3. *GenHelper.hs* general helper functions
4. *Invariants.hs* currently incomplete, prints only comments
5. *MachineFunctions.hs*
6. *MurphiAST.hs* as before
7. *MurphiClass.hs* class for the datatypes that are printed to murphi code. Has a single function tomurphi.
8. *MurphiPrint.hs* puts together all printing functions and prints a complete murphi file from a murphi AST
9. *Rules.hs*
10. *Startstate.hs*
11. *TomurphiHelper.hs* has functions that print murphi code for lower-level parts of the syntactic tree (e.g. responses)
12. *Types.hs*
13. *Variables.hs*
