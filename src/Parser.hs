{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Parser where

import Tokens
import Lexer
import Evaluation
import Memory
import StatementParser

import Text.Parsec
import Control.Monad.IO.Class
import Data.IntMap (update)

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program (0, [], [], [], [], True) "Error message" tokens

program :: ParsecT [Token] Memory IO [Token]
program = do
  s <- getState
  updateState ( insertScope (getCurrentScope s) )
  a <- preMain
  b <- mainProgram
  return (a ++ b)

mainProgram :: ParsecT [Token] Memory IO [Token]
mainProgram = do
  t <- typeToken
  m1 <- mainToken
  s <- getState
  updateState ( insertScope (getCurrentScope s + 1) )
  pl <- parLToken
  pr <- parRToken
  b <- beginToken
  st <- stmts
  e <- endToken
  m2 <- mainToken
  eof
  return (t:m1:pl:pr:b:st ++ e:[m2])

preMain :: ParsecT [Token] Memory IO [Token]
preMain = try (do
  a <- varDecl <|> function <|> procedure
  b <- preMain
  return (a ++ b)) <|> return []

function :: ParsecT [Token] Memory IO [Token]
function = do
  fun <- functionToken
  ret <- typeToken
  (Id name p) <- idToken
  pl <- parLToken
  pa <- paramsParse
  pr <- parRToken
  aux <- getInput
  s <- getState
  modifyState (insertFunctionOnMem (name, (False, getType ret s), pa, aux))
  s <- getState
  if getIsExecOn s then (do
    updateState setExecOnOff
    st <- stmts
    updateState setExecOnOff
    et <- endToken
    funt <- functionToken
    return (fun : ret : Id name p : pl : pa ++ pr : st ++ et : [funt]))
  else (do
    st <- stmts
    et <- endToken
    funt <- functionToken
    return (fun : ret : Id name p : pl : pa ++ pr : st ++ et : [funt]))

procedure :: ParsecT [Token] Memory IO [Token]
procedure = do
  prod <- procedureToken
  (Id name p) <- idToken
  pl <- parLToken
  pa <- paramsParse
  pr <- parRToken
  aux <- getInput
  s <- getState
  modifyState (insertFunctionOnMem (name, (False, VoidType), pa, aux))
  s <- getState
  if getIsExecOn s then (do
    updateState setExecOnOff
    st <- stmts
    updateState setExecOnOff
    et <- endToken
    funt <- procedureToken
    return (prod : Id name p : pl : pa ++ pr : st ++ et : [funt]))
  else (do
    st <- stmts
    et <- endToken
    funt <- procedureToken
    return (prod : Id name p : pl : pa ++ pr : st ++ et : [funt]))
