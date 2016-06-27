{
module Parser where
import Lexer
import Ast
import Data.List
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


    global        { TokenGlobal }
    channels      { TokenChannels }
    networks      { TokenNetworks }
    ordered       { TokenOrdered }
    unordered     { TokenUnordered }
    machine       { TokenMachine }
    nonsymmetric  { TokenNonsymmetric }
    boolean       { TokenBoolean }
    int           { TokenInt }
    set           { TokenSet }


    Issue       { TokenIssue }
    Receive     { TokenReceive }
    Send        { TokenSend }
    broadcast   { TokenBroadcast }
    stall       { TokenStall }
    Trans       { TokenTrans }
    add         { TokenAdd }
    del         { TokenDel }


    num         { TokenNum $$ }
    iden        { TokenIdentifier $$ }



%%


Model           : Globals Channels Networks Machines                 { Model $1 $2 $3 $4 }

Globals         : {-- empty --}                                      { [] }
                | global ':' Globals1 ';'                            { $3 }

Globals1        : TypeDecl                                           { [$1] }
                | Globals1 ',' TypeDecl                              { $3 : $1 }

Channels        : {-- empty --}                                      { [] }
                | channels ':' Channels1 ';'                         { $3 }

Channels1       : iden                                               { [Channel $1] }
                | Channels1 ',' iden                                 { (Channel $3) : $1 }

Networks        : {-- empty --}                                     { [] }
                | networks ':' Networks1 ';'                        { $3 }


Networks1       : Network                                           { [$1] }
                | Networks1 ',' Network                             { $3 : $1 }



Network         : ordered iden '{' IdenList '}'                     { Network Ord $2 (map (Channel) $4) }
                | unordered iden '{' IdenList '}'                   { Network Unord $2 (map (Channel) $4) }


Machines        : Machine                                           { [$1] }
                | Machines Machine                                  { $2 : $1 }


Machine         : Sym                                               { $1 }
                | Nonsym                                            { $1 }


Nonsym          : machine iden '{' Fields States_Guards '}'                         { Machine Nonsymmetric $2 1 $4 $5 }
                | nonsymmetric machine iden ':' Range '{' Fields States_Guards '}'  { Machine Nonsymmetric $3 $5 $7 $8 }


Sym             : machine iden ':' Range '{' Fields States_Guards '}'               { Machine Symmetric $2 $4 $6 $7 }





Fields          : {-- empty --}                                     { [] }
                | Fields1 ';'                                       { $1 }


Fields1         : Field                                             { [$1] }
                | Fields1 ',' Field                                 { $3 : $1 }

Field           : TypeDecl                                          { $1 }

-- may change to <iden> : <type> if hard to convert to target ast
-- where iden is first arg (arrays are the problem)

TypeDecl        : boolean iden                                      { Boolean $2 }
                | int '[' num '.' '.' num ']' iden                  { Integer $8 $3 $6 }
                | iden '{' IdenList '}'                             { Enum    $1 $3 }
                | iden iden                                         { Vertex    $1 $2 }
                | '[' num ']' TypeDecl                              { Array   $2 $4 }
                | '[' iden ']' TypeDecl                             { Map     $2 $4 }
                | set ':' num  TypeDecl                             { SetNum  $3 $4}
                | set ':' iden  TypeDecl                            { SetName $3 $4 }


IdenList        : iden                                              { [$1] }
                | IdenList ',' iden                                 { $3 : $1 }


Range           : '[' num ']'                                       { $2 }


States_Guards   : State_Guard                                       { [$1] }
                | States_Guards State_Guard                         { $2 : $1 }


State_Guard     : '(' iden ',' Guard ')' '{' Responses '}'              { (State $2, $4, Nothing, $7) }
                | '(' iden ',' Guard ',' iden ')' '{' Responses '}'     { (State $2, $4, Just (State $6), $9) }


Guard           : Mail                                              { Guard $1 }


Mail            : Issue '(' Msg ')'                                 { Issue $3 }
                | Send '(' Msg ',' Param ')'                        { Send $3 $5}
                | Receive '(' Msg ')'                               { ReceiveFrom $3 Nothing}
                | Receive '(' Msg ',' Param ')'                     { ReceiveFrom $3 (Just $5) }
                | broadcast '(' Param ',' Param ',' Msg ')'         { Broadcast $3 $5 $7 }
                | '*' Msg                                           { Issue $2 }
                | Param '!' Msg                                     { Send $3 $1 }
                | Param '?' Msg                                     { ReceiveFrom $3 (Just $1) }


Msg             : iden                                              { Msg $1 []}
                | iden '<' MsgArgs '>'                              { Msg $1 $3 }


MsgArgs         : MsgArg                                           { [$1] }
                | MsgArgs ',' MsgArg                               { $3 : $1 }


MsgArg          : TypeDecl                                         { $1 }


Responses       : {-- empty --}                                    { [] }
                | Responses Response1                              { $2 : $1 }


Response1       : Response ';'                                     { $1 }


Response        : Mail                                             { Response $1 }
                | Assignment                                       { Update $1 }
                | iden                                             { SelfIssue $1 }
                | iden '.' add '(' Param ')'                       { Add $1 (Left $5)}
                | iden '.' del '(' Param ')'                       { Del $1 (Left $5) }
                | iden '.' add '(' num ')'                         { Add $1 (Right $5) }
                | iden '.' del '(' num ')'                         { Del $1 (Right $5) }
                | stall                                            { Stall }


Assignment      : Param '=' Param                                  { Assign $1 $3 }
                | Param '=' num                                    { AssignNum $1 $3 }


Param           : iden '[' num ']'                              { Node $1 $3 }
                | iden                                          { Variable $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error "

parseAst :: String -> Ast

parseAst = ast . scanTokens

}

-- vim: set ft=haskell :
