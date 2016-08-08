
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
    !=              { \s -> TokenNotEq}
    ==              { \s -> TokenDoubleEq }
    \&              { \s -> TokenAnd }
    \|              { \s -> TokenOr }
    \?              { \s -> TokenQsMark }
    \!              { \s -> TokenExclMark }
    \*              { \s -> TokenStar }
    \/              { \s -> TokenSlash }
    \+              { \s -> TokenPlus }
    \-              { \s -> TokenMinus }
    \:              { \s -> TokenColon }
    \.              { \s -> TokenFullStop }
    \@              { \s -> TokenAt }

    global          { \s -> TokenGlobal }
    vcs             { \s -> TokenVCs }
    networks        { \s -> TokenNetworks }
    ordered         { \s -> TokenOrdered }
    unordered       { \s -> TokenUnordered }
    machine         { \s -> TokenMachine }
    nonsymmetric    { \s -> TokenNonsymmetric }
    startstate      { \s -> TokenStartstate }
    boolean         { \s -> TokenBoolean }
    int             { \s -> TokenInt }
    set             { \s -> TokenSet }

    Issue           { \s -> TokenIssue }
    clear           { \s -> TokenClear }
    Receive         { \s -> TokenReceive }
    src             { \s -> TokenSrc }
    Send            { \s -> TokenSend }
    broadcast       { \s -> TokenBroadcast }
    stall           { \s -> TokenStall }
    Trans           { \s -> TokenTrans }
    add             { \s -> TokenAdd }
    del             { \s -> TokenDel }

    @digits         { \s -> TokenNum (read s) }
    @iden           { \s -> TokenIdentifier s }




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
           | TokenDoubleEq
           | TokenNotEq
           | TokenAnd
           | TokenOr
           | TokenQsMark
           | TokenExclMark
           | TokenStar
           | TokenSlash
           | TokenPlus
           | TokenMinus
           | TokenColon
           | TokenFullStop
           | TokenAt
           | TokenGlobal
           | TokenVCs
           | TokenNetworks
           | TokenOrdered
           | TokenUnordered
           | TokenMachine
           | TokenNonsymmetric
           | TokenStartstate
           | TokenBoolean
           | TokenInt
           | TokenSet
           | TokenIssue
           | TokenClear
           | TokenReceive
           | TokenSrc
           | TokenSend
           | TokenBroadcast
           | TokenStall
           | TokenTrans
           | TokenAdd
           | TokenDel
           | TokenIdentifier String
           | TokenNum Int
             deriving (Eq,Show)

scanTokens = alexScanTokens

}
