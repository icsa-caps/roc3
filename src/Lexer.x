
{
{-# OPTIONS_GHC -w #-}
module Lexer where
}


%wrapper "basic"

$digit = 0-9
@digits = [$digit]+
$alpha = [a-zA-Z]
$eol   = [\n]
@iden  = $alpha [$alpha $digit \_ ]* (  \[  ( @digits )   \]  )?
@idenNoBr  = $alpha [$alpha $digit \_ ]*
@comment   = \/ \/ [^\n]* \n

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

    channels        { \s -> TokenChannels }
    networks        { \s -> TokenNetworks }
    ordered         { \s -> TokenOrdered }
    unordered       { \s -> TokenUnordered }
    machine         { \s -> TokenMachine }
    boolean         { \s -> TokenBoolean }
    int             { \s -> TokenInt }
    set             { \s -> TokenSet }
    Issue           { \s -> TokenIssue }
    Receive         { \s -> TokenReceive }
    Send            { \s -> TokenSend }
    Stall           { \s -> TokenStall }
    Trans           { \s -> TokenTrans }
    add             { \s -> TokenAdd }
    rid             { \s -> TokenRid }

    @digits         { \s -> TokenNum (read s) }
    @iden           { \s -> TokenIdentifier s }
    @idenNoBr       { \s -> TokenIdentifierNoBr s }



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
           | TokenChannels
           | TokenNetworks
           | TokenOrdered
           | TokenUnordered
           | TokenMachine
           | TokenBoolean
           | TokenInt
           | TokenSet
           | TokenIssue
           | TokenReceive
           | TokenSend
           | TokenStall
           | TokenTrans
           | TokenAdd
           | TokenRid
           | TokenIdentifier String
           | TokenIdentifierNoBr String
           | TokenNum Int
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
