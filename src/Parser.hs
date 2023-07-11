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

recordParser :: ParsecT [Token] Memory IO ([Token])
recordParser = do
  rt1 <- recordToken 
  (Id name p) <- idToken 
  b <- beginToken

  fields <- fieldsParser

  e <- endToken 
  rt2 <- recordToken 
  modifyState (insertTypeOnMem (RecordType (name, fields)))
  s <- getState
  liftIO $ print s
  return ([rt1])

program :: ParsecT [Token] Memory IO ([Token])
program = do
  r <- recordParser
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
