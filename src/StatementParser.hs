{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module StatementParser where

import Tokens
import Lexer
import Memory
import Evaluation

import Text.Parsec
import Control.Monad.IO.Class
import System.IO.Unsafe
import Text.Printf (printf)
import Data.Typeable (typeOf)

stmts :: ParsecT [Token] Memory IO [Token]
stmts = try (do
    a <- varDecl <|> assign <|> printFun <|> scanFun <|> ifStatement <|> returnExp
    b <- stmts
    return (a ++ b))
  <|> (do
    a <- functionCall
    b <- stmts
    return (a ++ b))
  <|> return []

varDecl :: ParsecT [Token] Memory IO [Token]
varDecl = do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "varDecl"
  t <- typeToken
  (Id name p) <- idToken
  b <- optionMaybe assignToken
  case b of
    Just b -> do
      exp <- exprs
      e <- semicolonToken
      s <- getState
      if getIsExecOn s then (do
        modifyState (insertVariableOnMem (name, getCurrentScope s, getDefaultValue t, False))
        s <- getState
        updateState (updateVarOnMem (name, getCurrentScope s, getType exp s, False))
        --liftIO $ print s
        return (t : Id name p : b : exp : [e])) else return []

    Nothing -> do
      e <- semicolonToken
      s <- getState
      if getIsExecOn s then (do
        modifyState (insertVariableOnMem (name, getCurrentScope s, getDefaultValue t, False))
        s <- getState
        --liftIO $ print s
        return (t : Id name p : [e])) else return []

fieldsParser :: ParsecT [Token] Memory IO [(Types, String)]
fieldsParser = do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "varDecl"
  t <- typeToken
  (Id name p) <- idToken
  e <- semicolonToken
  fp <- try fieldsParser
  return ((getDefaultValue t, name) : fp)

assign :: ParsecT [Token] Memory IO [Token]
assign = do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "assign"
  (Id name p) <- idToken
  b <- assignToken
  exp <- exprs
  d <- semicolonToken
  s <- getState
  if getIsExecOn s then (do
    updateState (updateVarOnMem (name, getCurrentScope s, getType exp s, False))
    s <- getState
    --liftIO $ print s
    return (Id name p : b : exp : [d])) else return []

printFun :: ParsecT [Token] Memory IO [Token]
printFun = try (do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "printFun"
  a <- printToken
  pl <- parLToken
  exp <- exprs
  cmm <- commaToken
  prt <- printRemaining
  s <- getState
  if getIsExecOn s then (do
    liftIO $ print (getType exp s)
    return (a : pl : exp : cmm : prt)) else return []) <|> (do
  a <- printToken
  pl <- parLToken
  exp <- exprs
  pr <- parRToken
  c <- semicolonToken
  s <- getState
  if getIsExecOn s then (do
    liftIO $ print (getType exp s)
    return (a : pl : exp : pr : [c])) else return [])

printRemaining :: ParsecT [Token] Memory IO [Token]
printRemaining = try (do
  liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "printRemaining"
  exp <- exprs
  cmm <- commaToken
  prt <- printRemaining
  s <- getState
  if getIsExecOn s then (do
    liftIO $ print (getType exp s)
    return (exp : cmm : prt)) else return []) <|> (do
  exp <- exprs
  pr <- parRToken
  c <- semicolonToken
  s <- getState
  if getIsExecOn s then (do
    liftIO $ print (getType exp s)
    return (exp : pr : [c])) else return [])

scanFun :: ParsecT [Token] Memory IO [Token]
scanFun = do
  a <- scanToken
  pl <- parLToken
  b <- idToken
  pr <- parRToken
  c <- semicolonToken
  s <- getState
  v <- liftIO getLine
  -- liftIO $ printf "\n%-20s%-10s\n" (show (typeOf value)) (show value)
  if getIsExecOn s then (do
    v <- liftIO getLine
    return (a : pl : b : pr : [c])) else return []

ifStatement :: ParsecT [Token] Memory IO [Token]
ifStatement = do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "ifStatement"
  f <- ifToken
  pl <- parLToken
  exp <- exprs
  pr <- parRToken
  s <- getState
  if getBoolValue exp then
    do
      st <- stmts
      et <- endToken
      eit <- ifToken
      return (f : pl : exp : pr : st ++ [et] ++ [eit])
  else
    do
      updateState setExecOnOff
      st <- stmts
      et <- endToken
      eit <- ifToken
      updateState setExecOnOff
      return (f : pl : exp : pr : st ++ [et] ++ [eit])

returnExp :: ParsecT [Token] Memory IO [Token]
returnExp = do
  a <- returnToken
  exp <- exprs
  c <- semicolonToken
  return (a : exp : [c])

argsList :: ParsecT [Token] Memory IO [Token]
argsList = (do
    exp <- exprs
    b <- commaToken
    c <- argsList
    return (exp : b : c))
  <|> (do
    a <- exprs
    b <- argsList
    return (a : b))
  <|> return []

functionCall :: ParsecT [Token] Memory IO [Token]
functionCall = do
  z <- lessToken
  a <- idToken
  b <- parLToken
  c <- argsList
  d <- parRToken
  w <- greaterToken
  return (z : a : b : c ++ d : [w])

whileStatement :: ParsecT [Token] Memory IO [Token]
whileStatement = do
  f <- whileToken
  a <- parLToken
  exp <- exprs
  c <- parRToken
  st <- stmts
  et <- endToken
  ew <- whileToken
  return (f : a : exp : c : st ++ [et] ++ [ew])

-------- Expressions ----------------------------------------------------------

exprs :: ParsecT [Token] Memory IO Token
exprs = try (do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "exprs"
  trm1f <- term1
  op <- logicAndToken <|> logicOrToken <|> logicXorToken <|> equalsToken <|> differentToken <|> greaterToken <|> greaterOrEqualToken <|> lessToken <|> lessOrEqualToken
  s <- getState
  eval s trm1f op <$> exprs
  ) <|> term1

term1 :: ParsecT [Token] Memory IO Token
term1 = try (do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "term1"
  trm2f <- term2
  op <- addToken <|> subToken
  s <- getState
  eval s trm2f op <$> term1
  ) <|> term2

term2 :: ParsecT [Token] Memory IO Token
term2 = try (do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "term2"
  trm <- factor
  op <- multToken <|> divToken <|> modToken
  s <- getState
  eval s trm op <$> term2
  ) <|> factor

factor :: ParsecT [Token] Memory IO Token
factor = try (do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "factor"
  ex <- expT
  op <- powerToken
  s <- getState
  eval s ex op <$> factor
  ) <|> expT

expT :: ParsecT [Token] Memory IO Token
expT = try (do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "expT"
  pl <- parLToken
  ex <- exprs
  pr <- parRToken
  return ex
  ) <|> idToken <|> intToken <|> floatToken <|> boolToken <|> stringToken
