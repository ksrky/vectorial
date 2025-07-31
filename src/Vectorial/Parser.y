{
module Vectorial.Parser (parseTerm, parsePattern, parseIso) where

import Vectorial.Gates
import Vectorial.Syntax
import Vectorial.Lexer
import Vectorial.Vector as V
import Data.Complex
import Data.Maybe
}

%name parseTerm Term
%name parsePattern Pattern
%name parseIso Iso
%tokentype { Token }
%error { parseError }

%token
  let           { TokenLet }
  in            { TokenIn }
  'tt'          { TokenTrue }
  'ff'          { TokenFalse }
  '='           { TokenEq }
  '('           { TokenLParen }
  ')'           { TokenRParen }
  ','           { TokenComma }
  '{'           { TokenLBrace }
  '}'           { TokenRBrace }
  '<->'         { TokenBiArrow }
  '|'           { TokenPipe }
  '+'           { TokenPlus }
  '*'           { TokenTimes }
  VAR           { TokenVar $$ }
  NUM           { TokenNum $$ }

%right in
%left '|'
%left '<->'
%left '+'
%left '*'
%left APP

%%

Term :: { Term }
Term : Bit                              { Bit $1 }
     | VAR                              { Var $1 }
     | Iso VTerm %prec APP              { IsoApp $1 $2 }
     | '(' VTerm ',' VTerm ')'          { Pair $2 $4 }
     | let Pattern '=' VTerm in VTerm   { Let $2 $4 $6 }
     | '(' Term ')'                     { $2 }

Bit :: { Bool }
Bit : 'tt'                              { True }
    | 'ff'                              { False }

VTerm :: { VTerm }
VTerm : Term                            { V [(1.0, $1)] }
      | NUM '*' Term                    { V [(CC ($1 :+ 0), $3)] }
      | VTerm '+' VTerm                 { $1 <> $3 }
      | '(' VTerm ')'                   { $2 }

Pattern :: { Pattern }
Pattern : Bit                           { PBit $1 }
        | VAR                           { PVar $1 }
        | '(' Pattern ',' Pattern ')'   { PPair $2 $4 }
        | '(' Pattern ')'               { $2 }

VPattern :: { VPattern }
VPattern : Pattern                      { V [(1.0, $1)] }
         | NUM '*' Pattern              { V [(CC ($1 :+ 0), $3)] }
         | VPattern '+' VPattern        { $1 <> $3 }
         | '(' VPattern ')'             { $2 }

Iso :: { Iso }
Iso : VAR                               { error ("Unknown gate: " ++ $1) `fromMaybe` lookup $1 gateTable}
    | '{' Clauses '}'                   { reverse $2 }

Clauses :: { Iso }
Clauses : Clause                        { [$1] }
        | Clauses '|' Clause            { $3 : $1 }

Clause :: { (Pattern, VPattern) }
Clause : Pattern '<->' VPattern         { ($1, $3) }

{
parseError :: [Token] -> a
parseError [] = error "Parse error at end of input"
parseError (t:_) = error $ "Parse error at token: " ++ show t

gateTable :: [(String, Iso)]
gateTable = [("X", pauliX), ("Z", pauliZ), ("H", hadamard), ("S", phaseS), ("T", phaseT), ("CNOT", cnot), ("CZ", cz)]
}
