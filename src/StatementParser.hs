module StatementParser where

import Tokens
import Lexer
import Memory

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
  liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "varDecl"
  t <- typeToken
  (Id name p) <- idToken
  e <- semicolonToken
  s <- getState
  modifyState (insertVariableOnMem (name, getCurrentScope s, getDefaultValue t, False))
  s <- getState
  liftIO $ print s
  return (t : (Id name p) : [e])

assign :: ParsecT [Token] Memory IO [Token]
assign = do
  liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "assign"
  (Id name p) <- idToken
  b <- assignToken
  c <- expression
  d <- semicolonToken
  s <- getState
  updateState (updateVarOnMem (name, getCurrentScope s, getType c s, False))
  s <- getState
  liftIO $ print s
  return ((Id name p) : b : c : [d])

printFun :: ParsecT [Token] Memory IO [Token]
printFun = do
  liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "printFun"
  a <- printToken
  pl <- parLToken
  exp <- expression
  pr <- parRToken
  c <- semicolonToken
  s <- getState
  let expType = getType exp s
  liftIO $ putStrLn $ show expType
  return (a : pl : exp : pr : [c])

scanFun :: ParsecT [Token] Memory IO [Token]
scanFun = do
  a <- scanToken
  b <- greaterToken
  c <- idToken
  d <- semicolonToken
  return (a : b : c : [d])

ifStatement :: ParsecT [Token] Memory IO [Token]
ifStatement = do
  f <- ifToken
  pl <- parLToken
  e <- expression
  pr <- parRToken
  st <- stmts
  et <- endToken
  eit <- ifToken
  return (f : pl : e : pr : st ++ [eit] ++ [eit])

returnExp :: ParsecT [Token] Memory IO [Token]
returnExp = do
  a <- returnToken
  b <- expression
  c <- semicolonToken
  return (a : b : [c])

argsList :: ParsecT [Token] Memory IO [Token]
argsList = (do
    a <- expression
    b <- commaToken
    c <- argsList
    return (a : b : c))
  <|> (do
    a <- expression
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
  b <- expression
  c <- parRToken
  st <- stmts
  et <- endToken
  ew <- whileToken
  return (f : a : b : c : st ++ [et] ++ [ew])

-------- Expressions ----------------------------------------------------------

-- 1st Precedence -------------------------------------------------------------

term1 :: ParsecT [Token] Memory IO [Token]
term1 = try binTerm1 <|> unaTerm1

unaTerm1 :: ParsecT [Token] Memory IO [Token]
unaTerm1 = do
  op <- powerToken <|> addUnaryToken
  a <- intToken <|> boolToken <|> floatToken <|> stringToken <|> idToken
  return (op : [a])

binTerm1 :: ParsecT [Token] Memory IO [Token]
binTerm1 = (do
    n1 <- intToken <|> boolToken <|> floatToken <|> stringToken <|> idToken
    result <- evalTerm1
    return (n1 : result))
  <|> (do
    --n1 <- try listLiteral <|> matrixLiteral <|> parenExpr
    result <- evalTerm1
    --return (n1 ++ result))
    return (result))

evalTerm1 :: ParsecT [Token] Memory IO [Token]
evalTerm1 = (do
    op <- powerToken <|> addUnaryToken
    n2 <- intToken <|> boolToken <|> floatToken <|> stringToken <|> idToken
    result <- evalTerm1
    return (op : n2 : result)
  <|> return [])

-- 2nd Precedence -------------------------------------------------------------

term2 :: ParsecT [Token] Memory IO [Token]
term2 = try binTerm2 <|> unaTerm2

unaTerm2 :: ParsecT [Token] Memory IO [Token]
unaTerm2 = do
  op <- multToken <|> divToken <|> modToken
  a <- term1
  return (op : a)

binTerm2 :: ParsecT [Token] Memory IO [Token]
binTerm2 = do
  n1 <- term1
  result <- evalTerm2
  return (n1 ++ result)

evalTerm2 :: ParsecT [Token] Memory IO [Token]
evalTerm2 = (do
    op <- multToken <|> divToken <|> modToken
    n2 <- term1
    result <- evalTerm2
    return (op : n2 ++ result))
  <|> return []

-- 3rd Precedence -------------------------------------------------------------

term3 :: ParsecT [Token] Memory IO [Token]
term3 = try binTerm3 <|> unaTerm3

unaTerm3 :: ParsecT [Token] Memory IO [Token]
unaTerm3 = do
  op <- addToken <|> subToken
  a <- term2
  return (op : a)

binTerm3 :: ParsecT [Token] Memory IO [Token]
binTerm3 = do
  n1 <- term2
  result <- evalTerm2
  return (n1 ++ result)

evalTerm3 :: ParsecT [Token] Memory IO [Token]
evalTerm3 = do
  op <- addToken <|> subToken
  n2 <- term3
  result <- evalTerm3
  return (op : n2 ++ result)
  <|> return []

-- 4th Precedence -------------------------------------------------------------

term4 :: ParsecT [Token] Memory IO [Token]
term4 = try binTerm4 <|> unaTerm4

unaTerm4 :: ParsecT [Token] Memory IO [Token]
unaTerm4 = do
  op <- equalsToken <|> differentToken <|> greaterToken <|> lessToken <|> greaterOrEqualToken <|> lessOrEqualToken
  a <- term3
  return (op : a)

binTerm4 :: ParsecT [Token] Memory IO [Token]
binTerm4 = do
  n1 <- term3
  result <- evalTerm4
  return (n1 ++ result)

evalTerm4 :: ParsecT [Token] Memory IO [Token]
evalTerm4 = do
  op <- equalsToken <|> differentToken <|> greaterToken <|> lessToken <|> greaterOrEqualToken <|> lessOrEqualToken
  n2 <- term3
  result <- evalTerm4
  return (op : n2 ++ result)
  <|> return []

-- 5th Precedence -------------------------------------------------------------

--expression :: ParsecT [Token] Memory IO([Token])
expression :: ParsecT [Token] Memory IO Token
expression = try unaExpTest -- <|> binExpression <|> unaExpression

unaExpTest :: ParsecT [Token] Memory IO Token
unaExpTest = do
  a <- intToken <|> floatToken <|> idToken
  --(_, _, varL, _, _) <- getState
  return a

unaExpression :: ParsecT [Token] Memory IO [Token]
unaExpression = do
  op <- logicAndToken <|> logicOrToken <|> logicXorToken
  a <- term4
  return (op : a)

binExpression :: ParsecT [Token] Memory IO [Token]
binExpression = do
  n1 <- term4
  result <- evalRemaining
  return (n1 ++ result)

evalRemaining :: ParsecT [Token] Memory IO [Token]
evalRemaining = do
    --op <- opandToken <|> oporToken <|> opxorToken
    op <- logicAndToken <|> logicOrToken <|> logicXorToken
    n2 <- term4
    result <- evalRemaining
    return (op : n2 ++ result)
  <|> return []
