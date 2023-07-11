module Tokens where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

-- Parsers para os Tokens

beginToken :: ParsecT [Token] st IO Token
beginToken = tokenPrim show update_pos get_token where
  get_token (Begin p) = Just (Begin p)
  get_token _ = Nothing

endToken :: ParsecT [Token] st IO Token
endToken = tokenPrim show update_pos get_token where
  get_token (End p) = Just (End p)
  get_token _ = Nothing

mainToken :: ParsecT [Token] st IO Token
mainToken = tokenPrim show update_pos get_token where
  get_token (Main p) = Just (Main p)
  get_token _ = Nothing

functionToken :: ParsecT [Token] st IO Token
functionToken = tokenPrim show update_pos get_token where
  get_token (Function p) = Just (Function p)
  get_token _ = Nothing

procedureToken :: ParsecT [Token] st IO Token
procedureToken = tokenPrim show update_pos get_token where
  get_token (Procedure p) = Just (Procedure p)
  get_token _ = Nothing



constToken :: ParsecT [Token] st IO Token
constToken = tokenPrim show update_pos get_token where
  get_token (Const p) = Just (Const p)
  get_token _ = Nothing

returnToken :: ParsecT [Token] st IO Token
returnToken = tokenPrim show update_pos get_token where
  get_token (Return p) = Just (Return p)
  get_token _ = Nothing



printToken :: ParsecT [Token] st IO Token
printToken = tokenPrim show update_pos get_token where
  get_token (Print p) = Just (Print p)
  get_token _ = Nothing

scanToken :: ParsecT [Token] st IO Token
scanToken = tokenPrim show update_pos get_token where
  get_token (Scan p) = Just (Scan p)
  get_token _ = Nothing



ifToken :: ParsecT [Token] st IO Token
ifToken = tokenPrim show update_pos get_token where
  get_token (If p) = Just (If p)
  get_token _ = Nothing

elifToken :: ParsecT [Token] st IO Token
elifToken = tokenPrim show update_pos get_token where
  get_token (Elif p) = Just (Elif p)
  get_token _ = Nothing

elseToken :: ParsecT [Token] st IO Token
elseToken = tokenPrim show update_pos get_token where
  get_token (Else p) = Just (Else p)
  get_token _ = Nothing



switchToken :: ParsecT [Token] st IO Token
switchToken = tokenPrim show update_pos get_token where
  get_token (Switch p) = Just (Switch p)
  get_token _ = Nothing

caseToken :: ParsecT [Token] st IO Token
caseToken = tokenPrim show update_pos get_token where
  get_token (Case p) = Just (Case p)
  get_token _ = Nothing



whileToken :: ParsecT [Token] st IO Token
whileToken = tokenPrim show update_pos get_token where
  get_token (While p) = Just (While p)
  get_token _ = Nothing

forToken :: ParsecT [Token] st IO Token
forToken = tokenPrim show update_pos get_token where
  get_token (For p) = Just (For p)
  get_token _ = Nothing




colonToken :: ParsecT [Token] st IO Token
colonToken = tokenPrim show update_pos get_token where
  get_token (Colon p) = Just (Colon p)
  get_token _ = Nothing

semicolonToken :: ParsecT [Token] st IO Token
semicolonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _ = Nothing

commaToken :: ParsecT [Token] st IO Token
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma p) = Just (Comma p)
  get_token _ = Nothing

parLToken :: ParsecT [Token] st IO Token
parLToken = tokenPrim show update_pos get_token where
  get_token (ParL p) = Just (ParL p)
  get_token _ = Nothing

parRToken :: ParsecT [Token] st IO Token
parRToken = tokenPrim show update_pos get_token where
  get_token (ParR p) = Just (ParR p)
  get_token _ = Nothing

bracketLToken :: ParsecT [Token] st IO Token
bracketLToken = tokenPrim show update_pos get_token where
  get_token (BracketL p) = Just (BracketL p)
  get_token _ = Nothing

bracketRToken :: ParsecT [Token] st IO Token
bracketRToken = tokenPrim show update_pos get_token where
  get_token (BracketR p) = Just (BracketR p)
  get_token _ = Nothing



typeToken :: ParsecT [Token] st IO Token
typeToken = tokenPrim show update_pos get_token where
  get_token (Type x p) = Just (Type x p)
  get_token _        = Nothing

intToken :: ParsecT [Token] st IO Token
intToken = tokenPrim show update_pos get_token where
  get_token (Int x p) = Just (Int x p)
  get_token _       = Nothing

floatToken :: ParsecT [Token] st IO Token
floatToken = tokenPrim show update_pos get_token where
  get_token (Float x p) = Just (Float x p)
  get_token _       = Nothing

boolToken :: ParsecT [Token] st IO Token
boolToken = tokenPrim show update_pos get_token where
  get_token (Bool x p) = Just (Bool x p)
  get_token _       = Nothing

charToken :: ParsecT [Token] st IO Token
charToken = tokenPrim show update_pos get_token where
  get_token (Char x p) = Just (Char x p)
  get_token _       = Nothing

stringToken :: ParsecT [Token] st IO Token
stringToken = tokenPrim show update_pos get_token where
  get_token (String x p) = Just (String x p)
  get_token _       = Nothing

arrayToken :: ParsecT [Token] st IO Token
arrayToken = tokenPrim show update_pos get_token where
  get_token (Array p) = Just (Array p)
  get_token _       = Nothing

matrixToken :: ParsecT [Token] st IO Token
matrixToken = tokenPrim show update_pos get_token where
  get_token (Matrix p) = Just (Matrix p)
  get_token _       = Nothing

assignToken :: ParsecT [Token] st IO Token
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _ = Nothing

addToken :: ParsecT [Token] st IO Token
addToken = tokenPrim show update_pos get_token where
  get_token (Add p) = Just (Add p)
  get_token _ = Nothing

addUnaryToken :: ParsecT [Token] st IO Token
addUnaryToken = tokenPrim show update_pos get_token where
  get_token (AddUnary p) = Just (AddUnary p)
  get_token _ = Nothing

subToken :: ParsecT [Token] st IO Token
subToken = tokenPrim show update_pos get_token where
  get_token (Sub p) = Just (Sub p)
  get_token _ = Nothing

subUnaryToken :: ParsecT [Token] st IO Token
subUnaryToken = tokenPrim show update_pos get_token where
  get_token (SubUnary p) = Just (SubUnary p)
  get_token _ = Nothing

multToken :: ParsecT [Token] st IO Token
multToken = tokenPrim show update_pos get_token where
  get_token (Mult p) = Just (Mult p)
  get_token _ = Nothing

divToken :: ParsecT [Token] st IO Token
divToken = tokenPrim show update_pos get_token where
  get_token (Div p) = Just (Div p)
  get_token _ = Nothing

powerToken :: ParsecT [Token] st IO Token
powerToken = tokenPrim show update_pos get_token where
  get_token (Power p) = Just (Power p)
  get_token _ = Nothing

modToken :: ParsecT [Token] st IO Token
modToken = tokenPrim show update_pos get_token where
  get_token (Mod p) = Just (Mod p)
  get_token _ = Nothing



equalsToken :: ParsecT [Token] st IO Token
equalsToken = tokenPrim show update_pos get_token where
  get_token (Equals p) = Just (Equals p)
  get_token _ = Nothing

differentToken :: ParsecT [Token] st IO Token
differentToken = tokenPrim show update_pos get_token where
  get_token (Different p) = Just (Different p)
  get_token _ = Nothing

greaterToken :: ParsecT [Token] st IO Token
greaterToken = tokenPrim show update_pos get_token where
  get_token (Greater p) = Just (Greater p)
  get_token _ = Nothing

greaterOrEqualToken :: ParsecT [Token] st IO Token
greaterOrEqualToken = tokenPrim show update_pos get_token where
  get_token (GreaterOrEqual p) = Just (GreaterOrEqual p)
  get_token _ = Nothing

lessToken :: ParsecT [Token] st IO Token
lessToken = tokenPrim show update_pos get_token where
  get_token (Less p) = Just (Less p)
  get_token _ = Nothing

lessOrEqualToken :: ParsecT [Token] st IO Token
lessOrEqualToken = tokenPrim show update_pos get_token where
  get_token (LessOrEqual p) = Just (LessOrEqual p)
  get_token _ = Nothing



logicNotToken :: ParsecT [Token] st IO Token
logicNotToken = tokenPrim show update_pos get_token where
  get_token (LogicNot p) = Just (LogicNot p)
  get_token _ = Nothing

logicAndToken :: ParsecT [Token] st IO Token
logicAndToken = tokenPrim show update_pos get_token where
  get_token (LogicAnd p) = Just (LogicAnd p)
  get_token _ = Nothing

logicOrToken :: ParsecT [Token] st IO Token
logicOrToken = tokenPrim show update_pos get_token where
  get_token (LogicOr p) = Just (LogicOr p)
  get_token _ = Nothing

logicXorToken :: ParsecT [Token] st IO Token
logicXorToken = tokenPrim show update_pos get_token where
  get_token (LogicXor p) = Just (LogicXor p)
  get_token _ = Nothing



idToken :: ParsecT [Token] st IO Token
idToken = tokenPrim show update_pos get_token where
  get_token (Id x p) = Just (Id x p)
  get_token _      = Nothing



update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos
