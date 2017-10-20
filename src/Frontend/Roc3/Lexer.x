
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
    networks        { \s -> TokenNetworks }
    ordered         { \s -> TokenOrdered }
    unordered       { \s -> TokenUnordered }
    machine         { \s -> TokenMachine }
    nonsymmetric    { \s -> TokenNonsymmetric }
    startstate      { \s -> TokenStartstate }
    boolean         { \s -> TokenBoolean }
    int             { \s -> TokenInt }
    set             { \s -> TokenSet }

    clear           { \s -> TokenClear }
    src             { \s -> TokenSrc }
    stall           { \s -> TokenStall }
    add             { \s -> TokenAdd }
    del             { \s -> TokenDel }
    contains        { \s -> TokenContains }
    count           { \s -> TokenCount }
    all		    { \s -> TokenAll }
    self	    { \s -> TokenSelf }

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
           | TokenClear
           | TokenSrc
           | TokenStall
           | TokenAdd
           | TokenDel
           | TokenContains
           | TokenCount
	   | TokenAll
	   | TokenSelf
           | TokenIdentifier String
           | TokenNum Int
             deriving (Eq,Show)

scanTokens = alexScanTokens

}
