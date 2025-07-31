{
module Vectorial.Lexer (Token(..), alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]

tokens :-

  $white+                       ; -- ignore whitespace
  "--".*                        ; -- ignore comments
  
  -- Keywords
  "let"                         { \_ -> TokenLet }
  "in"                          { \_ -> TokenIn }
  "tt"                          { \_ -> TokenTrue }
  "ff"                          { \_ -> TokenFalse }
  
  -- Operators and punctuation
  "="                           { \_ -> TokenEq }
  "("                           { \_ -> TokenLParen }
  ")"                           { \_ -> TokenRParen }
  ","                           { \_ -> TokenComma }
  "{"                           { \_ -> TokenLBrace }
  "}"                           { \_ -> TokenRBrace }
  "<->"                         { \_ -> TokenBiArrow }
  "|"                           { \_ -> TokenPipe }
  
  -- Vector operators
  "+"                           { \_ -> TokenPlus }
  "*"                           { \_ -> TokenTimes }
  
  -- Identifiers (variables and gate names)
  $alpha [$alphanum \_]*        { \s -> TokenVar s }
  
  -- Numbers (for coefficients)
  $digit+ ("." $digit+)?        { \s -> TokenNum (read s) }

{
data Token 
  = TokenLet
  | TokenIn
  | TokenTrue
  | TokenFalse
  | TokenEq
  | TokenLParen
  | TokenRParen
  | TokenComma
  | TokenLBrace
  | TokenRBrace
  | TokenBiArrow
  | TokenPipe
  | TokenPlus
  | TokenTimes
  | TokenVar String
  | TokenNum Double
  | TokenEOF
  deriving (Eq, Show)
}