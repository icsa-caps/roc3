{
{-# OPTIONS_GHC -w #-}
module Lexer where
}

%wrapper "basic"

$digit = 0-9
@digits = [$digit]+
$alpha = [a-zA-Z]
$eol   = [\n]
@iden  = $alpha [$alpha $digit \_]*

tokens :-

    $eol            ;
    $white+         ;
    BEGIN           { \s -> TokenBegin }
    END             { \s -> TokenEnd }

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

    machine         { \s -> TokenMachine }
    Issue           { \s -> TokenIssue }
    Receive         { \s -> TokenReceive }
    Send            { \s -> TokenSend }
    Stall           { \s -> TokenStall }
    Trans           { \s -> TokenTrans }

    @iden           { \s -> TokenIdentifier s }
    @digits         { \s -> TokenNum (read s) }


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
           | TokenMachine
           | TokenIssue
           | TokenReceive
           | TokenSend
           | TokenStall
           | TokenTrans
           | TokenIdentifier String
           | TokenNum Int
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
