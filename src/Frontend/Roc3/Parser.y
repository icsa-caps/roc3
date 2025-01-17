{
-- if any change is made to the parser, type at the terminal
-- happy Parser.y
-- to replace Parser.hs file. The cabal file should generate this automatically,
-- but it doesn't; this happened after introducing multiple directories.
module Frontend.Roc3.Parser where
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
    doubleEq      { TokenDoubleEq }
    notEq         { TokenNotEq}
    '&'           { TokenAnd }
    '|'           { TokenOr }
    '?'           { TokenQsMark }
    '!'           { TokenExclMark}
    '+'           { TokenPlus }
    '-'           { TokenMinus }
    '*'           { TokenStar }
    '/'           { TokenSlash }
    ':'           { TokenColon }
    '.'           { TokenFullStop }
    '@'           { TokenAt }


    global        { TokenGlobal }
    networks      { TokenNetworks }
    ordered       { TokenOrdered }
    unordered     { TokenUnordered }
    machine       { TokenMachine }
    nonsymmetric  { TokenNonsymmetric }
    startstate    { TokenStartstate }
    boolean       { TokenBoolean }
    int           { TokenInt }
    set           { TokenSet }


    clear       { TokenClear }
    src         { TokenSrc }
    stall       { TokenStall }
    add         { TokenAdd }
    del         { TokenDel }
    contains    { TokenContains }
    count       { TokenCount }
    all         { TokenAll }
    self        { TokenSelf }

    num         { TokenNum $$ }
    iden        { TokenIdentifier $$ }





%left '+' '-'
%left '*'
%left '/'

%%


Model           : Networks Machines                           { Model $1 $2 }


Globals         : {-- empty --}                               { [] }
                | global ':' Globals1 ';'                     { $3 }

Globals1        : TypeDecl                                    { [$1] }
                | Globals1 ',' TypeDecl                       { $3 : $1 }


Networks        : {-- empty --}                               { [] }
                | networks ':' NetworkList ';'                { $3 }

NetworkList     : Network                                     { [$1] }
                | NetworkList ',' Network                     { $3 : $1 }

Network         : ordered iden '{' VCs '}'               { Network Ord $2 $4 }
                | unordered iden '{' VCs '}'             { Network Unord $2 $4 }
                | ordered '{' VCs '}'                    { Network Ord "SingleNet" $3 }
                | unordered '{' VCs '}'                  { Network Unord "SingleNet" $3 }


VCs             : VC                                          { [$1] }
                | VCs ',' VC                                  { ($3) : $1 }

VC              : iden                                        { VC $1 }


Machines        : Machine                                     { [$1] }
                | Machines Machine                            { $2 : $1 }

Machine         : Sym                                         { $1 }
                | Nonsym                                      { $1 }

Nonsym          : machine iden '{' Startstate Fields States_Guards '}'                         { Machine Nonsymmetric $2 1 $4 $5 $6 }
                | nonsymmetric machine iden Range '{' Startstate Fields States_Guards '}'      { Machine Nonsymmetric $3 $4 $6 $7 $8 }

Sym             : machine iden  Range '{' Startstate Fields States_Guards '}'                  { Machine Symmetric $2 $3 $5 $6 $7 }


Startstate      : startstate ':' iden ';'                           { State $3 }


Fields          : {-- empty --}                                     { [] }
                | Fields1 ';'                                       { $1 }

Fields1         : {-- empty --}                                     { [] }
                | Fields1 ',' Field                                 { $1 ++ [$3] }
                | Field                                             { [$1] }

Field           : TypeDecl  StartVal                                { Field $1 $2 }


StartVal        : {-- empty --}                                     { Nothing }
                | '(' iden ')'                                      { Just $2 }
                | '(' num ')'                                       { Just (show $2) }


TypeDecl        : boolean iden                                      { Boolean $2 }
                | int '[' num '.' '.' num ']' iden                  { Integer $8 $3 $6 }
                | iden '{' IdenList '}'                             { Enum    $1 $3 }
                | iden iden                                         { Vertex    $1 $2 }
                | '[' num ']' TypeDecl                              { Array  (Left $2)  $4 }
                | '[' iden ']' TypeDecl                             { Array  (Right $2) $4 }
                | set Range TypeDecl                                { Set  (Left $2) $3}
                | set '[' iden ']' TypeDecl                         { Set (Right $3) $5 }


IdenList        : {-- empty --}                                     { [] }
                | IdenList ',' iden                                 { $1 ++ [$3] }
                | iden                                              { [$1] }


Range           : '[' num ']'                                       { $2 }


States_Guards   : State_Guard                                       { [$1] }
                | States_Guards State_Guard                         { $1 ++ [$2] }

State_Guard     : '(' iden ',' Guard ')' '{' Responses '}'              { (State $2, $4, Nothing, $7) }
                | '(' iden ',' Guard ',' iden ')' '{' Responses '}'     { (State $2, $4, Just (State $6), $9) }


Guard           : src '?' Msg '@' VC                                   { ReceiveFrom $3 (Nothing) (Just $5) }
                | src '?' Msg                                          { ReceiveFrom $3 (Nothing) (Nothing) }
                | Param '?' Msg '@' VC                                 { ReceiveFrom $3 (Just $1) (Just $5) }
                | Param '?' Msg                                        { ReceiveFrom $3 (Just $1) (Nothing) }
                | '*' iden                                             { Issue $2 }
                | Param doubleEq Param                                 { Equals $1 (Left $3) }
                | Param doubleEq IntExp                                { Equals $1 (Right $3) }
                | Param '>' Param                                      { Greater $1 (Left $3) }
                | Param '>' IntExp                                     { Greater $1 (Right $3) }
                | Param '<' Param                                      { Less $1 (Left $3) }
                | Param '<' IntExp                                     { Less $1 (Right $3) }
                | Param notEq Param                                    { NotEq $1 (Left $3) }
                | Param notEq IntExp                                   { NotEq $1 (Right $3) }
                | '!' Guard                                            { Not $2 }
                | Guard '&' Guard                                      { $1 :&: $3}
                | Guard '|' Guard                                      { $1 :|: $3}
                | '(' Guard ')'                                        { $2 }
                | iden '.' contains '(' Param ')'                      { IsIn $1 $5 }
                | iden '.' contains '(' num ')'                        { IsIn $1 (VarOrVal (show $5))}


Msg             : iden                                              { Msg $1 []}
                | iden '<' MsgArgs '>'                              { Msg $1 $3 }


MsgArgs         : {-- empty --}                                    { [] }
                | MsgArgs ',' MsgArg                               { $1 ++ [$3] }
                | MsgArg                                           { [$1] }

MsgArg          : TypeDecl '=' Param                               { GuardAssign $1 $3 }
                | TypeDecl                                         { MsgArg $1 }


Responses       : {-- empty --}                                    { [] }
                | Responses Response1                              { $1 ++ [$2] }

Response1       : Response ';'                                     { $1 }

Response        : Param '!' Msg '@' VC                                 { Send $3 $1 ($5) }
                | Assignment                                           { $1 }
                | iden                                                 { EmptyResp $1 }
                | iden '.' add '(' Param ')'                           { Add $1 (Left $5)}
                | iden '.' del '(' Param ')'                           { Del $1 (Left $5) }
                | iden '.' add '(' num ')'                             { Add $1 (Right $5) }
                | iden '.' del '(' num ')'                             { Del $1 (Right $5) }
                | stall                                                { Stall }
                | clear iden                                           { Clear $2 }


Assignment      : Param    '=' Param                               { Assign $1 $3 }
                | Param    '=' IntExp                              { AssignNum $1 $3 }
                | TypeDecl '=' Param                               { AssignLocal $1 $3 }
                | TypeDecl '=' IntExp                              { AssignLocalNum $1 $3 }


Param           : iden '[' num ']'                              { ArrayElem $1 $3 }
                | src                                           { VarOrVal "src" }
                | iden                                          { VarOrVal $1 }
                | iden '.' count                                { SetSize $1 }
		| all                                           { VarOrVal "all" }
                | self                                          { VarOrVal "self" }


IntExp          : IntExp '+' IntExp                             { Sum $1 $3 }
                | IntExp '-' IntExp                             { Minus $1 $3 }
                | '(' IntExp ')'                                { Group $2 }
                | IntExp '*' IntExp                             { Times $1 $3 }
                | IntExp '/' IntExp                             { Div $1 $3 }
                | num                                           { Const $1 }
                | Param                                         { IntVar $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error "

parseAst :: String -> Ast

parseAst = ast . scanTokens

}

-- vim: set ft=haskell :
