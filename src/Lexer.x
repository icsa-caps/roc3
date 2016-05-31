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

    machine         { \s -> TokenMachine }
    Issue           { \s -> TokenIssue }
    Receive         { \s -> TokenReceive }
    Send            { \s -> TokenSend }
    Stall           { \s -> TokenStall }
    Trans           { \s -> TokenTrans }

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
           | TokenMachine
           | TokenIssue
           | TokenReceive
           | TokenSend
           | TokenStall
           | TokenTrans
           | TokenIdentifier String
           | TokenIdentifierNoBr String
           | TokenNum Int
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
