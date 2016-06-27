
{
{-# OPTIONS_GHC -w #-}
module Lexer where
}


%wrapper "basic"

$digit    = 0-9
@digits   = [$digit]+
$alpha    = [a-zA-Z]
$eol      = [\n]
@iden = $alpha [$alpha $digit \_ ]*
@comment  = \/ \/ [^\n]* \n

tokens :-

    $eol            ;
    $white+         ;
    @comment        ;

    \{              { \s -> TokenOpCrlBracket }
    \}              { \s -> TokenClCrlBracket }
    \(              { \s -> TokenOpBracket }
    \)              { \s -> TokenClBracket }
    \[              { \s -> TokenOpSqBracket }
    \]              { \s -> TokenClSqBracket }
    \<              { \s -> TokenOpAngleBracket }
    \>              { \s -> TokenClAngleBracket }
    \;              { \s -> TokenSemiColon }
    \,              { \s -> TokenComa }
    \=              { \s -> TokenEq }
    \?              { \s -> TokenQsMark }
    \!              { \s -> TokenExclMark }
    \*              { \s -> TokenStar }
    \:              { \s -> TokenColon }
    \.              { \s -> TokenFullStop }

    global          { \s -> TokenGlobal }
    channels        { \s -> TokenChannels }
    networks        { \s -> TokenNetworks }
    ordered         { \s -> TokenOrdered }
    unordered       { \s -> TokenUnordered }
    machine         { \s -> TokenMachine }
    nonsymmetric    { \s -> TokenNonsymmetric }
    boolean         { \s -> TokenBoolean }
    int             { \s -> TokenInt }
    set             { \s -> TokenSet }

    Issue           { \s -> TokenIssue }
    Receive         { \s -> TokenReceive }
    Send            { \s -> TokenSend }
    broadcast       { \s -> TokenBroadcast }
    stall           { \s -> TokenStall }
    Trans           { \s -> TokenTrans }
    add             { \s -> TokenAdd }
    del             { \s -> TokenDel }

    @digits         { \s -> TokenNum (read s) }
    @iden           { \s -> TokenIdentifier s }
    '1'             { \s -> TokenNumOne }




{

data Token = TokenBegin
           | TokenEnd
           | TokenOpCrlBracket
           | TokenClCrlBracket
           | TokenOpBracket
           | TokenClBracket
           | TokenOpSqBracket
           | TokenClSqBracket
           | TokenOpAngleBracket
           | TokenClAngleBracket
           | TokenSemiColon
           | TokenComa
           | TokenEq
           | TokenQsMark
           | TokenExclMark
           | TokenStar
           | TokenColon
           | TokenFullStop
           | TokenGlobal
           | TokenChannels
           | TokenNetworks
           | TokenOrdered
           | TokenUnordered
           | TokenMachine
           | TokenNonsymmetric
           | TokenBoolean
           | TokenInt
           | TokenSet
           | TokenIssue
           | TokenReceive
           | TokenSend
           | TokenBroadcast
           | TokenStall
           | TokenTrans
           | TokenAdd
           | TokenDel
           | TokenIdentifier String
           | TokenNum Int
           | TokenNumOne

           deriving (Eq,Show)

scanTokens = alexScanTokens

}
