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
    a <- varDecl <|> assign <|> printFun <|> scanFun <|> ifStatement <|> returnExp <|> addEle <|> addArr
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
  bl <- optionMaybe bracketLToken
  case bl of
    Just bl -> do
      (Int v _) <- intToken
      br <- bracketRToken
      (Id name p) <- idToken
      e <- semicolonToken
      s <- getState
      if getIsExecOn s then (do
        modifyState (insertVariableOnMem (name, getCurrentScope s, ArrayType (getTypeStr t, v, 0, []), False))
        s <- getState
        liftIO $ print s
        return (t : Id name p : t : [e])) else return []
    Nothing -> do
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
              s <- getState
              liftIO $ print s
              return (t : Id name p : b : exp : [e])) else return []
          Nothing -> do
              e <- semicolonToken
              s <- getState
              if getIsExecOn s then (do
                modifyState (insertVariableOnMem (name, getCurrentScope s, getDefaultValue t, False))
                s <- getState
                liftIO $ print s
                return (t : Id name p : [e])) else return []

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
  --(Id name p) <- idToken
  b <- idToken
  --(Id name p) <- b
  pr <- parRToken
  c <- semicolonToken
  s <- getState
  --v <- liftIO $ getLine
  -- liftIO $ printf "\n%-20s%-10s\n" (show (typeOf value)) (show value)
  if getIsExecOn s then (do
    v <- liftIO $ getLine
    updateState (updateVarOnMem ((getName b), getCurrentScope s, (turnType (getType b s) v), False))
    return (a : pl : pr : [c])) else return []


getName :: Token -> String
getName (Id name _) = name


turnType :: Types -> String -> Types
turnType (IntType _) s = (IntType (read s))
turnType (FloatType _) s = (FloatType (read s))
turnType (BoolType _) s = (BoolType (read s))
turnType (CharType _) s = (CharType (read s))
turnType (StringType _) s = (StringType s)


addEle :: ParsecT [Token] Memory IO [Token]
addEle = do
  ad <- addElementToken
  c <- doubleColonToken
  id <- idToken
  pl <- parLToken
  exp <- exprs
  pr <- parRToken
  sc <- semicolonToken
  s <- getState
  --var <- getVariable (getName id) (getCurrentScope s) (getVariables s)
  --arr <- getVariableType var
  --arr ++ exp
  if(typeCompatible (getVariableType (getVariable (getName id) (getCurrentScope s) (getVariables s))) exp) then
    if(arrayFull (getVariableType (getVariable (getName id) (getCurrentScope s) (getVariables s)))) then
        error "Array full, can't add more :("
      else
      updateState (updateVarOnMem (getName id, getCurrentScope s, arrangeAdd s (getVariableType (getVariable (getName id) (getCurrentScope s) (getVariables s))) exp, False))
  else
    error "Can't add this element on this array -- Incompatible type"
  return ([id])
  

getEle :: ParsecT [Token] Memory IO Token
getEle = do
  get <- getElementToken
  c <- doubleColonToken
  id <- idToken
  pl <- parLToken
  exp <- exprs
  pr <- parRToken
  s <- getState
  if(getCurrentSize (getVariableType (getVariable (getName id) (getCurrentScope s) (getVariables s))) > getIntValue exp) then
    return (retEleFromArr (getVariableType (getVariable (getName id) (getCurrentScope s) (getVariables s))) (getIntValue exp))
    --updateState (updateVarOnMem (getName id, getCurrentScope s, arrangeAdd s (getVariableType (getVariable (getName id) (getCurrentScope s) (getVariables s))) exp, False))
  else
    error "Can't get this element on this array -- wrong index?"



addArr :: ParsecT [Token] Memory IO [Token]
addArr = do
  ad <- addArrayToken
  c <- doubleColonToken
  id <- idToken
  pl <- parLToken
  arrId <- idToken
  pr <- parRToken
  sc <- semicolonToken
  s <- getState

  if(arrayFull (getVariableType (getVariable (getName id) (getCurrentScope s) (getVariables s)))) then
    error "Matrix full, can't add more :("
  else
    updateState (updateVarOnMem (getName id, getCurrentScope s, matrixAdd s (getVariableType (getVariable (getName id) (getCurrentScope s) (getVariables s))) (getVariableType (getVariable (getName arrId) (getCurrentScope s) (getVariables s))), False))
  return ([id])
  

--getEle :: ParsecT [Token] Memory IO Token
--getEle = do
--  get <- getElementToken
--  c <- doubleColonToken
--  id <- idToken
--  pl <- parLToken
--  exp <- exprs
--  pr <- parRToken
--  s <- getState
--  if(getCurrentSize (getVariableType (getVariable (getName id) (getCurrentScope s) (getVariables s))) > getIntValue exp) then
--    return (retEleFromArr (getVariableType (getVariable (getName id) (getCurrentScope s) (getVariables s))) (getIntValue exp))
    --updateState (updateVarOnMem (getName id, getCurrentScope s, arrangeAdd s (getVariableType (getVariable (getName id) (getCurrentScope s) (getVariables s))) exp, False))
--  else
--    error "Can't get this element on this array -- wrong index?"



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
  op <- powerToken <|> addUnaryToken
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
  ) <|> idToken <|> intToken <|> floatToken <|> boolToken <|> stringToken <|> getEle -- <|> arrayToken <|> matrixToken
  
arrayDec :: ParsecT [Token] Memory IO Token
arrayDec = try (do
  t <- typeToken
  bl <- bracketLToken
  --n <- intToken
  --br <- bracketRToken
  return (t))
  
matDec :: ParsecT [Token] Memory IO Token
matDec = try (do
  bl <- bracketLToken
  n <- intToken
  br <- bracketRToken
  return (n))
