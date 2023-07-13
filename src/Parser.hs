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
import Text.Printf (printf)
import Data.List (find)

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program (0, [], [], [], [], True) "Error message" tokens

program :: ParsecT [Token] Memory IO [Token]
program = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "Parser" "Call" "program"
  s <- getState
  updateState ( insertScope (getCurrentScope s) )
  a <- preMain
  b <- mainProgram
  return (a ++ b)

mainProgram :: ParsecT [Token] Memory IO [Token]
mainProgram = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "Parser" "Call" "mainProgram"
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
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "Parser" "Call" "preMain"
  a <- function <|> procedure <|> record <|> varDecl
  b <- preMain
  return (a ++ b)) <|> return []

function :: ParsecT [Token] Memory IO [Token]
function = do
  fun <- functionToken
  ret <- typeToken
  (Id name p) <- idToken
  pl <- parLToken
  updateState setExecOnOff
  pa <- parseParams
  updateState setExecOnOff
  s<- getState
  liftIO $ print (show pa)
  pr <- parRToken
  aux <- getInput
  s <- getState
  modifyState (insertFunctionOnMem (name,  (False, getTypeAux ret (getCurrentScope s) (getVariables s), ret), pa, aux))
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
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "Parser" "Call" "procedure"
  (Procedure p2) <- procedureToken
  (Id name p) <- idToken
  pl <- parLToken
  aux <- getInput
  updateState setExecOnOff
  pa <- parseParams
  updateState setExecOnOff
  pr <- parRToken
  aux2 <- getInput
  s <- getState
  modifyState (insertFunctionOnMem (name, (False, VoidType,Procedure p2), aux, aux2))
  s <- getState
  if getIsExecOn s then (do
    updateState setExecOnOff
    st <- stmts
    updateState setExecOnOff
    et <- endToken
    funt <- procedureToken
    return (Procedure p2 : Id name p : pl : pa ++ pr : st ++ et : [funt]))
  else (do
    st <- stmts
    et <- endToken
    funt <- procedureToken
    return (Procedure p2 : Id name p : pl : pa ++ pr : st ++ et : [funt]))

record :: ParsecT [Token] Memory IO [Token]
record = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "Parser" "Call" "record"
  rt1 <- recordToken 
  (Id name p) <- idToken 
  b <- beginToken

  fields <- fieldsParser -- TODO check if all the fields have diferent names

  if hasRepeatedFields fields then
    error ("Error on Parser -- fieldsParser: duplicates fields")

  else do
    e <- endToken 
    rt2 <- recordToken 
    modifyState (insertTypeOnMem (RecordType (name, fields)))
    s <- getState
    liftIO $ print s
    return (rt1 : Id name p : b : e : [rt2])
  where
    hasRepeatedFields :: [(String, Types)] -> Bool
    hasRepeatedFields [] = False
    hasRepeatedFields ((name, _):rest) =
      case find ((== name) . fst) rest of
        Just _ -> True
        Nothing -> hasRepeatedFields rest
