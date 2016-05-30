{
module Parser where

import Lexer
import Ast
}

%name ast
%tokentype { Token }
%error { parseError }

%token
    BEGIN         { TokenBegin }
    END           { TokenEnd }

    '{'           { TokenOpCrlBracket }
    '}'           { TokenClCrlBracket }
    '('           { TokenOpBracket }
    ')'           { TokenClBracket }
    '['           { TokenOpSqBracket }
    ']'           { TokenClSqBracket }
    '<'           { TokenOpAngleBracket }
    '>'           { TokenClAngleBracket }
    ';'           { TokenSemiColon }
    ','           { TokenComa }
    '='           { TokenEq }


    machine     { TokenMachine }
    Issue       { TokenIssue }
    Receive     { TokenReceive }
    Send        { TokenSend }
    Stall       { TokenStall }
    Trans       { TokenTrans }


    iden        { TokenIdentifier $$ }
    num         { TokenNum $$ }



%%

Model           : BEGIN Nodes END                                  { Model $2 }

Nodes           : Node                                          { [$1] }
                | Nodes Node                                    { $2 : $1 }

Node            : machine iden '[' num ']' '{' States_Guards '}'   { Node $2 $4 $7 }


States_Guards   : State_Guard                                   { [$1] }
                | States_Guards State_Guard                    { $2 : $1 }

State_Guard     : '(' iden ',' Guard ')' '{' Responses '}'         { (State $2, $4, $7) }

Guard           : Mail                                             { Guard $1 }


Mail            : Issue '(' Msg ')'                                { Issue $3 }
                | Send '(' Msg ',' iden ')'                        { Send $3 $5}
                | Receive '(' Msg ')'                              { Receive $3 }
                | Receive '(' Msg ',' iden ')'                     { ReceiveFrom $3 $5 }


Msg             : iden                                             { Msg $1 }
                | iden '<' iden '>'                                { MsgWithArg $1 $3 }

Responses       : Response1                                        { [$1] }
                | Responses Response1                              { $2 : $1 }

Response1       : Response ';'                                     { $1 }

Response        : Mail                                              { Response $1 }
                | Trans '(' iden ')'                                { Trans (State $3) }
                | Assignment                                        { Update $1 }
                | iden                                              { Other $1 }

Assignment      : iden '=' iden                                     { Var $1 $3 }
                | iden '=' num                                      { VarNum $1 $3 }









{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseAst :: String -> Ast
parseAst = ast . scanTokens

}

-- vim: set ft=haskell :
