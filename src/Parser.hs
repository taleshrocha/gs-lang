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
parser tokens = runParserT program ("--", [], [], [], [], False) "Error message" tokens

program :: ParsecT [Token] Memory IO ([Token])
program = do
  a <- mainProgram
  return a

mainProgram :: ParsecT [Token] Memory IO ([Token])
mainProgram = do
  t <- typeToken 
  m1 <- mainToken 
  updateState( insertScope "main" )
  pl <- parLToken
  pr <- parRToken
  b <- beginToken
  updateState setExecOnOff
  s <- stmts
  e <- endToken
  m2 <- mainToken 
  eof
  return (t:m1:pl:pr:b:s ++ e:[m2])
