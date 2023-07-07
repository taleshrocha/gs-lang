module StatementParser where

import Tokens
import Lexer
import Memory
import Evaluation

import Text.Parsec
import Control.Monad.IO.Class
import System.IO.Unsafe
import Text.Printf (printf)

stmts :: ParsecT [Token] Memory IO [Token]
stmts = try (do
    a <- assign <|> varDecl <|> printFun <|> scanFun <|> ifStatement <|> returnExp
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
  e <- semicolonToken
  s <- getState
  modifyState (insertVariableOnMem (name, getCurrentScope s, getDefaultValue t, False))
  s <- getState
  --liftIO $ print s
  return (t : (Id name p) : [e])

assign :: ParsecT [Token] Memory IO [Token]
assign = do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "assign"
  (Id name p) <- idToken
  b <- assignToken
  exp <- expressions
  d <- semicolonToken
  s <- getState
  updateState (updateVarOnMem (name, getCurrentScope s, getType exp s, False))
  s <- getState
  --liftIO $ print s
  return ((Id name p) : b : exp : [d])

printFun :: ParsecT [Token] Memory IO [Token]
printFun = try (do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "printFun"
  a <- printToken
  pl <- parLToken
  exp <- expressions
  cmm <- commaToken
  prt <- printRemaining
  s <- getState
  liftIO $ putStrLn $ show (getType exp s)
  return (a : pl : exp : cmm : prt)) <|> (do
  a <- printToken
  pl <- parLToken
  exp <- expressions
  pr <- parRToken
  c <- semicolonToken
  s <- getState
  liftIO $ putStrLn $ show (getType exp s)
  return (a : pl : exp : pr : [c]))

printRemaining :: ParsecT [Token] Memory IO [Token]
printRemaining = try (do
  liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "printRemaining"
  exp <- expressions
  cmm <- commaToken
  prt <- printRemaining
  s <- getState
  liftIO $ putStrLn $ show (getType exp s)
  return (exp : cmm : prt)) <|> (do
  exp <- expressions
  pr <- parRToken
  c <- semicolonToken
  s <- getState
  liftIO $ putStrLn $ show (getType exp s)
  return (exp : pr : [c]))

scanFun :: ParsecT [Token] Memory IO [Token]
scanFun = do
  a <- scanToken
  b <- greaterToken
  c <- idToken
  d <- semicolonToken
  return (a : b : c : [d])

ifStatement :: ParsecT [Token] Memory IO [Token]
ifStatement = do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "ifStatement"
  f <- ifToken
  pl <- parLToken
  exp <- expressions
  pr <- parRToken
  s <- getState
  let val = getBoolValue exp
  if(val) then 
    do
      st <- stmts
      et <- endToken
      eit <- ifToken
      return (f : pl : exp : pr : st ++ [et] ++ [eit])
  else 
    do 
      st <- stmts
      et <- endToken
      eit <- ifToken
      return []

returnExp :: ParsecT [Token] Memory IO [Token]
returnExp = do
  a <- returnToken
  exp <- expressions
  c <- semicolonToken
  return (a : exp : [c])

argsList :: ParsecT [Token] Memory IO [Token]
argsList = (do
    exp <- expressions
    b <- commaToken
    c <- argsList
    return (exp : b : c))
  <|> (do
    a <- expressions
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
  exp <- expressions
  c <- parRToken
  st <- stmts
  et <- endToken
  ew <- whileToken
  return (f : a : exp : c : st ++ [et] ++ [ew])

-------- Expressions ----------------------------------------------------------

expressions :: ParsecT [Token] Memory IO Token
expressions = try (do 
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "expressions"
  trm1f <- term1
  op <- logicAndToken <|> logicOrToken <|> logicXorToken
  trm1l <- term1
  return (eval trm1f op trm1l)
  ) <|> (do
  trm1 <- term1
  return trm1)

term1 :: ParsecT [Token] Memory IO Token
term1 = try (do 
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "term1"
  trm2f <- term2
  op <- addToken <|> subToken
  trm2l <- term2
  return (eval trm2f op trm2l)
  ) <|> (do
  trm2 <- term2
  return trm2)

term2 :: ParsecT [Token] Memory IO Token
term2 = try (do 
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "term2"
  trm <- factor
  op <- multToken <|> divToken
  fctr <- factor
  return (eval trm op fctr)
  ) <|> (do
  fct <- factor
  return fct)

factor :: ParsecT [Token] Memory IO Token
factor = try (do 
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "factor"
  ex <- expression
  op <- powerToken
  fctr <- expression
  return (eval ex op fctr)
  ) <|> (do
  xpr <- expression
  return xpr)

expression :: ParsecT [Token] Memory IO Token
expression = try (do 
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "expression"
  pl <- parLToken
  ex <- expression
  pr <- parRToken
  return ex
  ) <|> (do
  id <- idToken <|> intToken <|> floatToken <|> boolToken <|> stringToken
  return id)
