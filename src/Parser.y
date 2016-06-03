{
module Parser where

import Lexer
import Ast
}

%name ast
%tokentype { Token }
%error { parseError }

%token

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
    '?'           { TokenQsMark }
    '!'           { TokenExclMark}
    '*'           { TokenStar }
    ':'           { TokenColon }
    '.'           { TokenFullStop }


    machine     { TokenMachine }
    boolean     { TokenBoolean }
    int         { TokenInt }


    Issue       { TokenIssue }
    Receive     { TokenReceive }
    Send        { TokenSend }
    Stall       { TokenStall }
    Trans       { TokenTrans }


    num         { TokenNum $$ }
    iden        { TokenIdentifier $$ }
    idenNoBr    { TokenIdentifierNoBr $$ }

%%

Model           : Machines                                          { Model $1 }


Machines        : Machine                                           { [$1] }
                | Machines Machine                                  { $2 : $1 }


Machine         : machine iden ':' Range '{' Fields States_Guards '}'  { Machine $2 $4 $6 $7 }


Fields          : {-- empty --}                                     { [] }
                | Fields1 ';'                                       { $1 }


Fields1         : Field                                             { [$1] }
                | Fields1 ',' Field                                 { $3 : $1 }


Field           : boolean iden                                      { Boolean $2 }
                | int '[' num '.' '.' num ']' iden                  { Integer $8 $3 $6 }
                | iden '{' IdenList '}'                             { Enum $1 $3 }
                | iden iden                                         { Node $1 $2 }
                | '[' ']' Field                                     { Array $3 }


IdenList        : iden                                              { [$1] }
                | IdenList ',' iden                                 { $3 : $1 }


Range           : '[' num ']'                                       { $2 }


States_Guards   : State_Guard                                       { [$1] }
                | States_Guards State_Guard                         { $2 : $1 }


State_Guard     : '(' iden ',' Guard ')' '{' Responses '}'              { (State $2, $4, Nothing, $7) }
                | '(' iden ',' Guard ',' iden ')' '{' Responses '}'     { (State $2, $4, Just (State $6), $9) }


Guard           : Mail                                              { Guard $1 }


Mail            : Issue '(' Msg ')'                                { Issue $3 }
                | Send '(' Msg ',' iden ')'                        { Send $3 $5}
                | Receive '(' Msg ')'                              { ReceiveFrom $3 Nothing}
                | Receive '(' Msg ',' iden ')'                     { ReceiveFrom $3 (Just $5) }
                | '*' Msg                                          { Issue $2 }
                | iden '!' Msg                                     { Send $3 $1 }
                | iden '?' Msg                                     { ReceiveFrom $3 (Just $1) }


Msg             : iden                                             { Msg $1 []}
                | iden '<' MsgArgs '>'                             { Msg $1 $3 }


MsgArgs         : MsgArg                                           { [$1] }
                | MsgArgs ',' MsgArg                               { $3 : $1 }


MsgArg          : iden iden                                        { MsgArg $1 $2 }


Responses       : {-- empty --}                                    { [] }
                | Responses Response1                              { $2 : $1 }


Response1       : Response ';'                                     { $1 }


Response        : Mail                                              { Response $1 }
                | Assignment                                        { Update $1 }
                | iden                                              { SelfIssue $1 }


Assignment      : iden '=' iden                                     { Var $1 $3 }
                | iden '=' num                                      { VarNum $1 $3 }









{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseAst :: String -> Ast
parseAst = ast . scanTokens

}

-- vim: set ft=haskell :
