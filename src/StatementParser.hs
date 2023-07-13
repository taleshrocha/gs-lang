{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
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
import Control.Monad (when)

stmts :: ParsecT [Token] Memory IO [Token]
stmts = try (do
    a <- varDecl <|> assign <|> printFun <|> scanFun <|> ifStatement <|> whileStatement <|> procedureCall <|> returnExp
    b <- stmts
    return (a ++ b))
  <|> try (do
    a <- functionCall
    b <- stmts
    return (a : b))
  <|> continueBreakStatement
  <|> expressionStatement
  <|> return []

----- Declaração de variáveis ------------------------------------

decl :: ParsecT [Token] Memory IO [Token]
decl = try setPointers <|> varDecl

setPointers ::  ParsecT [Token] Memory IO [Token]
setPointers = try (do
  (Id name1 p1) <- idToken
  b <- assignToken
  amper <- amperToken
  (Id name2 p2) <- idToken
  co <- optionMaybe colonToken
  case co of
    Just co -> (do
      s <- getState
      st <- setPointers
      when (getIsExecOn s) (do
        updateState (insertVariableOnMem (getType (Id name2 p2) s) (name1, getCurrentScope s, getVariableType (getVariableMem name2 (getCurrentScope s) s) , getIsVariableConst (getVariableMem name2 (getCurrentScope s) s), (name2, getVariableScope (getVariableMem name2 (getCurrentScope s) s), True))))
      return (Id name1 p1 : b : amper : Id name2 p2 : co : st))

    Nothing -> (do
      e <- semicolonToken
      s <- getState
      when (getIsExecOn s) (do
        updateState (insertVariableOnMem (getType (Id name2 p2) s) (name1, getCurrentScope s, getVariableType (getVariableMem name2 (getCurrentScope s) s) , getIsVariableConst (getVariableMem name2 (getCurrentScope s) s), (name2, getVariableScope (getVariableMem name2 (getCurrentScope s) s), True))))
      return (Id name1 p1 : b : amper : Id name2 p2 : [e]))) <|> return []

varDecl :: ParsecT [Token] Memory IO [Token]
varDecl = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "varDecl"
  ct <- optionMaybe constToken
  let isConst = case ct of
        Just _  -> True
        Nothing -> False
  r <- optionMaybe recordToken
  case r of
    Just r -> do
      t <- idToken
      (Id name p) <- idToken
      e <- semicolonToken
      s <- getState
      when (getIsExecOn s) (do
        updateState (insertRecOnMem (name, getCurrentScope s, getDefaultRecordValue t s, isConst, ("",0,False)))
        --s <- getState
        --liftIO $ print s
        )
      case ct of
        Just ct  -> return (ct : t : Id name p : [e])
        Nothing -> return (t : Id name p : [e])

    Nothing -> do
      t <- typeToken
      (Id name p) <- idToken
      bl <- optionMaybe bracketLToken -- Array
      case bl of
        Just bl -> do
          (Int v p) <- intToken
          br <- bracketRToken
          bl2 <- optionMaybe bracketLToken -- Matrix
          case bl2 of
            Just bl2 -> do
              (Int v2 p) <- intToken
              br <- bracketRToken
              e <- semicolonToken
              s <- getState
              when (getIsExecOn s) (do
                updateState (
                  insertArrOnMem (name, getCurrentScope s
                  , getDefaultArrayValue (ArrayType (getDefaultValue t, v, 0, []))
                  , isConst, ("", 0, False)))
                --s <- getState
                --liftIO $ print s
                )
              case ct of
                Just ct  -> return ((Id name p) : ct : bl : (Int v p) : br : (Id name p) : [e])
                Nothing -> return ((Id name p) : bl : (Int v p) : br : (Id name p) : [e])

            Nothing -> do
              e <- semicolonToken
              s <- getState
              when (getIsExecOn s) (do
                updateState (
                  insertArrOnMem (name, getCurrentScope s
                  , getDefaultArrayValue (ArrayType (getDefaultValue t, v, 0, []))
                  , isConst, ("", 0, False)))
                --s <- getState
                --liftIO $ print s
                )
              case ct of
                Just ct  -> return ((Id name p) : ct : bl : (Int v p) : br : (Id name p) : [e])
                Nothing -> return ((Id name p) : bl : (Int v p) : br : (Id name p) : [e])

        Nothing -> do
          s <- getState
          b <- optionMaybe assignToken
          case b of
            Just b -> do
              exp <- expression
              co <- optionMaybe commaToken
              case co of
                Just co -> (do
                  s <- getState
                  st <- try varDecl <|> varDeclRemaining t
                  when (getIsExecOn s) (do
                    updateState (insertVariableOnMem (getType t s) (name, getCurrentScope s, getType exp s, isConst, ("",0,False))))
                  case ct of
                    Just ct  -> return (ct : t : Id name p : b : exp : co : st)
                    Nothing -> return (t : Id name p : b : exp : co : st))

                Nothing -> (do
                  e <- semicolonToken
                  s <- getState
                  when (getIsExecOn s) (do
                    updateState (insertVariableOnMem (getType t s) (name, getCurrentScope s, getType exp s, isConst, ("",0,False)))
                    --s <- getState
                    --liftIO $ print s
                    )
                  case ct of
                    Just ct  -> return (ct : t : Id name p : b : exp : [e])
                    Nothing -> return (t : Id name p : b : exp : [e]))

            Nothing -> do
              co <- optionMaybe commaToken
              case co of
                Just co -> (do
                  s <- getState
                  st <- try varDecl <|> varDeclRemaining t
                  when (getIsExecOn s) (do
                    updateState (insertVariableOnMem (getType t s) (name, getCurrentScope s, getDefaultValue t, isConst, ("",0,False))))
                  case ct of
                    Just ct  -> return (ct : t : Id name p : co : st)
                    Nothing -> return (t : Id name p : co : st))

                Nothing -> (do
                  e <- semicolonToken
                  s <- getState
                  when (getIsExecOn s) (do
                    updateState (insertVariableOnMem (getType t s) (name, getCurrentScope s, getDefaultValue t, isConst, ("",0,False)))
                    --s <- getState
                    --liftIO $ print s
                    )
                  case ct of
                    Just ct  -> return (ct : t : Id name p : [e])
                    Nothing -> return (t : Id name p : [e]))

varDeclRemaining :: Token -> ParsecT [Token] Memory IO [Token]
varDeclRemaining t@(Type _ _) = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "varDeclRemaning"
  ct <- optionMaybe constToken
  let isConst = case ct of
        Just _  -> True
        Nothing -> False
  t1 <- optionMaybe typeToken
  let hasType = case t1 of
        Just _  -> True
        Nothing -> False
  (Id name p) <- idToken
  s <- getState
  b <- optionMaybe assignToken
  case b of
    Just b -> do
      exp <- expression
      co <- optionMaybe commaToken
      case co of
        Just co -> (do
          s <- getState
          when (getIsExecOn s) (do
            updateState (insertVariableOnMem (getType (returnType hasType t1 t) s) (name, getCurrentScope s, getType exp s, isConst, ("",0,False))))
          st <- try varDecl <|> varDeclRemaining (returnType hasType t1 t)
          case ct of
            Just ct  -> return (ct : t : Id name p : b : exp : co : st)
            Nothing -> return (t : Id name p : b : exp : co : st))

        Nothing -> (do
          e <- semicolonToken
          s <- getState
          when (getIsExecOn s) (do
            updateState (insertVariableOnMem (getType (returnType hasType t1 t) s) (name, getCurrentScope s, getType exp s, isConst, ("",0,False))))
          case ct of
            Just ct  -> return (ct : t : Id name p : b : exp : [e])
            Nothing -> return (t : Id name p : b : exp : [e]))

    Nothing -> do
      co <- optionMaybe commaToken
      case co of
        Just co -> (do
          s <- getState
          when (getIsExecOn s) (do
            updateState (insertVariableOnMem (getType (returnType hasType t1 t) s) (name, getCurrentScope s, getDefaultValue t, isConst, ("",0,False))))
          st <- try varDecl <|> varDeclRemaining t
          case ct of
            Just ct  -> return (ct : t : Id name p : co : st)
            Nothing -> return (t : Id name p : co : st))

        Nothing -> (do
          e <- semicolonToken
          s <- getState
          when (getIsExecOn s) (do
            updateState (insertVariableOnMem (getType (returnType hasType t1 t) s) (name, getCurrentScope s, getDefaultValue t, isConst, ("",0,False))))
          case ct of
            Just ct  -> return (ct : t : Id name p : [e])
            Nothing -> return (t : Id name p : [e]))

returnType :: Bool -> Maybe Token -> Token -> Token
returnType hasType t1 t2 = do
  if hasType then (do
    case t1 of
      Just x -> x
      Nothing -> Void (0,0))
  else t2

fieldsParser :: ParsecT [Token] Memory IO [(String, Types)]
fieldsParser = do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "fieldsParser"
  t <- typeToken
  (Id name p) <- idToken
  asg <- optionMaybe assignToken
  case asg of
    Just asg -> do
      exp <- expression
      e <- semicolonToken
      s <- getState
      fds <- fieldsParser <|> return []
      return ((name, getType exp s) : fds)

    Nothing -> do
      e <- semicolonToken
      fds <- fieldsParser <|> return []
      return ((name, getDefaultValue t) : fds)

----- Assign ------------------------------------

assign :: ParsecT [Token] Memory IO [Token]
assign = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "assign"
  (Id name p) <- idToken
  dot <- optionMaybe dotToken
  case dot of
    Just dot -> do
      (Id fName p) <- idToken
      b <- assignToken
      exp <- expression
      d <- semicolonToken
      s <- getState
      when (getIsExecOn s) (do
        liftIO $ print (fName ++ " " ++ name ++ " " ++ show (getType exp s))
        updateState (updateRecOnMem fName (name, getCurrentScope s, getType exp s, False, ("",0,False)))
        --s <- getState
        --liftIO $ print s
        )
      return (Id name p : dot : (Id fName p) : exp : [d])
    Nothing -> do
      bl <- optionMaybe bracketLToken
      case bl of
        Just bl -> do
          (Int v p) <- intToken
          br <- bracketRToken
          b <- assignToken
          exp <- exprs
          d <- semicolonToken
          s <- getState
          when (getIsExecOn s) (do
            updateState (updateArrOnMem(name, getCurrentScope s, getType exp s, False, ("", 0, False)) v))
          return (bl : (Int v p) : br : b : exp : [d])
        Nothing -> do
          b <- assignToken
          exp <- expression
          co <- optionMaybe commaToken
          case co of
            Just co -> do
              s <- getState
              when (getIsExecOn s) (do
                updateState (updateVarOnMem (name, getCurrentScope s, getType exp s, False, ("",0,False))))
              st <- assign
              return (Id name p : b : exp : co : st)
            Nothing -> do
              d <- semicolonToken
              s <- getState
              when (getIsExecOn s) (do
                updateState (updateVarOnMem (name, getCurrentScope s, getType exp s, False, ("",0,False))))
              return (Id name p : b : exp : [d])

printFun :: ParsecT [Token] Memory IO [Token]
printFun = try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "printFun"
  a <- printToken
  pl <- parLToken
  exp <- expression
  cmm <- commaToken
  prt <- printRemaining
  s <- getState
  when (getIsExecOn s) (do
    liftIO $ print (getType exp s))
  return (a : pl : exp : cmm : prt)) <|> (do
  a <- printToken
  pl <- parLToken
  exp <- expression
  pr <- parRToken
  c <- semicolonToken
  s <- getState
  when (getIsExecOn s) (do
    s <- getState
    liftIO $ print (getType exp s))
  return (a : pl : exp : pr : [c]))

printRemaining :: ParsecT [Token] Memory IO [Token]
printRemaining = try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "printRemaining"
  exp <- expression
  cmm <- commaToken
  prt <- printRemaining
  s <- getState
  when (getIsExecOn s) (do
    liftIO $ print (getType exp s))
  return (exp : cmm : prt)) <|> (do
  exp <- expression
  pr <- parRToken
  c <- semicolonToken
  s <- getState
  when (getIsExecOn s) (do
    liftIO $ print (getType exp s))
  return (exp : pr : [c]))

scanFun :: ParsecT [Token] Memory IO [Token]
scanFun = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "scanFun"
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
    return (a : pl : b : pr : [c])) else return (a : pl : b : pr : [c])

ifStatement :: ParsecT [Token] Memory IO [Token]
ifStatement = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "ifStatement"
  f <- ifToken
  pl <- parLToken
  exp <- expression
  pr <- parRToken
  s <- getState
  if getIsExecOn s then
    if getBoolValue exp then
      do
        st <- stmts
        updateState setExecOnOff
        elif <- elifStatement
        els <- elseStatement
        updateState setExecOnOff
        et <- endToken
        eit <- ifToken
        return (f : pl : exp : pr : st ++ elif ++ els ++ et : [eit])
    else
      do
        updateState setExecOnOff
        st <- stmts
        updateState setExecOnOff
        elif <- elifStatement
        els <- elseStatement
        et <- endToken
        eit <- ifToken
        return (f : pl : exp : pr : st ++ elif ++ els ++ et : [eit])
  else
    do
      st <- stmts
      elif <- elifStatement
      els <- elseStatement
      et <- endToken
      eit <- ifToken
      return (f : pl : exp : pr : st ++ elif ++ els ++ et : [eit])

elifStatement :: ParsecT [Token] Memory IO [Token]
elifStatement = try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "elifStatement"
  f <- elifToken
  pl <- parLToken
  exp <- expression
  pr <- parRToken
  s <- getState
  if getIsExecOn s then
    if getBoolValue exp then
      do
        st <- stmts
        updateState setExecOnOff
        elif <- elifStatement
        els <- elseStatement
        updateState setExecOnOff
        return (f : pl : exp : pr : st ++ elif ++ els)
    else
      do
        updateState setExecOnOff
        st <- stmts
        updateState setExecOnOff
        elif <- elifStatement
        return (f : pl : exp : pr : st ++ elif)
    else
      do
        st <- stmts
        elif <- elifStatement
        els <- elseStatement
        return (f : pl : exp : pr : st ++ elif ++ els)) <|> return []

elseStatement :: ParsecT [Token] Memory IO [Token]
elseStatement = try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "elseStatement"
  f <- elseToken
  st <- stmts
  return (f : st)) <|> return []

whileStatement :: ParsecT [Token] Memory IO [Token]
whileStatement = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "whileStatement"
  aux <- getInput
  wh <- whileToken
  pl <- parLToken
  exp <- expression
  pr <- parRToken
  s <- getState
  if getIsExecOn s then
    if getBoolValue exp then (do
        st <- stmts
        et <- endToken
        wht <- whileToken
        s <- getState
        if getIsExecOn s then (do
          setInput aux
          return (wh : pl : exp : pr : st ++ et : [wht]))
        else (do
          updateState setExecOnOff
          return (wh : pl : exp : pr : st ++ et : [wht])))
    else (do
        updateState setExecOnOff
        st <- stmts
        updateState setExecOnOff
        et <- endToken
        wht <- whileToken
        return (wh : pl : exp : pr : st ++ et : [wht]))
  else (do
      st <- stmts
      et <- endToken
      wht <- whileToken
      return (wh : pl : exp : pr : st ++ et : [wht]))

continueBreakStatement :: ParsecT [Token] Memory IO [Token]
continueBreakStatement = try breakStatement <|> continueStatement

breakStatement :: ParsecT [Token] Memory IO [Token]
breakStatement = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "breakStatement"
  brk <- breakToken
  sc <- semicolonToken
  s <- getState
  if getIsExecOn s then (do
    updateState setExecOnOff
    st <- stmts
    return (brk : sc : st))
  else (do
    st <- stmts
    return (brk : sc : st))

continueStatement :: ParsecT [Token] Memory IO [Token]
continueStatement = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "continueStatement"
  cont <- continueToken
  sc <- semicolonToken
  s <- getState
  if getIsExecOn s then (do
    updateState setExecOnOff
    st <- stmts
    updateState setExecOnOff
    return (cont : sc : st))
  else (do
    st <- stmts
    return (cont : sc : st))

returnExp :: ParsecT [Token] Memory IO [Token]
returnExp = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "returnExp"
  a <- returnToken
  exp <- expression
  c <- semicolonToken
  return (a : exp : [c])

paramsParse :: ParsecT [Token] Memory IO [Token]
paramsParse = try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "paramsParse"
  ct <- optionMaybe constToken
  let isConst = case ct of
        Just _  -> True
        Nothing -> False
  t <- typeToken
  pointer <- optionMaybe multToken
  case pointer of
    Just pointer -> (do
      id <- idToken
      co <- optionMaybe commaToken
      case co of
        Just co -> (do
          paP <- paramsParse
          return (maybeToToken [ct] ++ t : pointer : id : co : paP))

        Nothing -> return (maybeToToken [ct] ++ t : pointer : [id]))

    Nothing -> (do
      id <- idToken
      co <- optionMaybe commaToken
      case co of
        Just co -> (do
          paP <- paramsParse
          return (maybeToToken [ct] ++ t : id : co : paP))

        Nothing -> return (maybeToToken [ct] ++ t : [id]))) <|> return []

maybeToToken :: [Maybe Token] -> [Token]
maybeToToken (maybeValue : tail)
  | null tail =
     case maybeValue of
        Just x -> [x]
        Nothing -> []

  | otherwise =
     case maybeValue of
        Just x -> x : maybeToToken tail
        Nothing -> maybeToToken tail

createVarFromFunc :: String -> Memory -> ParsecT [Token] Memory IO [Token]
createVarFromFunc name (currentScope, scopes, varTable, (funcName, ret, params, body) : tail, typeTable, isOn)
  | name == funcName = do
      aux <- getInput
      s <- getState
      setInput params
      setInput aux
      return []
  | null tail = error ("Error on Memory -- getFunctionBody: function (" ++ show name ++ ") not declared!")
  | otherwise = createVarFromFunc name (currentScope, scopes, varTable, tail, typeTable, isOn)

paramsParseCall :: String -> ParsecT [Token] Memory IO [Token]
paramsParseCall name = try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "paramsParseCall"
  id <- idToken
  co <- optionMaybe commaToken
  case co of
    Just co -> (do
      paP <- paramsParseCall name
      return (id : co : paP))

    Nothing -> return [id])

functionCall :: ParsecT [Token] Memory IO Token
functionCall = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "functionCall"
  s <- getState
  updateState ( insertScope (getCurrentScope s + 1) )
  (Id name p) <- idToken
  pl <- parLToken
  pa <- paramsParseCall name
  pr <- parRToken
  aux <- getInput
  s <- getState
  setInput (getFunctionBody name s)
  st <- stmts
  et <- endToken
  funt <- functionToken
  setInput aux
  s <- getState
  updateState (cleanMemFromScope (getCurrentScope s))
  s <- getState
  updateState removeScope
  returnToken

recordCall :: ParsecT [Token] Memory IO Token
recordCall = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "recordCall"
  (Id id1 p) <- idToken
  dot <- dotToken
  (Id id2 p) <- idToken
  st <- getState
  return (getRecMem id1 id2 (getCurrentScope st) st)

arrayCall :: ParsecT [Token] Memory IO Token
arrayCall = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "arrayCall"
  (Id id1 p) <- idToken
  bl <- bracketLToken
  (Int value p) <- intToken
  br <- bracketRToken
  st <- getState
  return (getArrMem id1 value (getCurrentScope st) st)

procedureCall :: ParsecT [Token] Memory IO [Token]
procedureCall = do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "procedureCall"
  s <- getState
  updateState ( insertScope (getCurrentScope s + 1) )
  (Id name p) <- idToken
  pl <- parLToken
  pa <- paramsParseCall name
  pr <- parRToken
  sc <- semicolonToken
  aux <- getInput
  s <- getState
  setInput (getFunctionBody name s)
  st <- stmts
  et <- endToken
  proct <- procedureToken
  setInput aux
  s <- getState
  updateState (cleanMemFromScope (getCurrentScope s))
  s <- getState
  updateState removeScope
  return (Id name p : pl : pa ++ pr : sc : st ++ et : [proct])

expressionStatement :: ParsecT [Token] Memory IO [Token]
expressionStatement = try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "expressionStatement"
  exp <- expression
  sc <- semicolonToken
  st <- stmts
  return (exp : sc : st)) <|> return []

-------- Expressions ----------------------------------------------------------

expression :: ParsecT [Token] Memory IO Token
expression = try unaryExpression <|> binExpression

unaryExpression :: ParsecT [Token] Memory IO Token
unaryExpression = try (do
  --liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "unaryExpression"
  op <- subToken <|> addUnaryToken <|> subUnaryToken
  expr <- expT
  s <- getState
  let result = evalUnary s op expr
  s <- getState
  s <- getState
  when (getIsExecOn s) (
    updateState (processUnary op expr (getCurrentScope s) result))
  return result)
  <|> do
    expr <- expT
    op <- addUnaryToken <|> subUnaryToken
    s <- getState
    let result = evalUnary s op expr
    s <- getState
    when (getIsExecOn s) (
      updateState (processUnary op expr (getCurrentScope s) result))
    return result

processUnary :: Token -> Token -> Scope -> Token -> Memory -> Memory
processUnary (AddUnary _) (Id name _) scp type1 mem = updateVarOnMem (name, scp, getType type1 mem, False, ("",0,False)) mem
processUnary (SubUnary _) (Id name _) scp type1 mem = updateVarOnMem (name, scp, getType type1 mem, False, ("",0,False)) mem
processUnary _ _ _ _ mem = mem

binExpression :: ParsecT [Token] Memory IO Token
binExpression = try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "binExpression"
  exp1f <- exprs
  op <- logicAndToken <|> logicOrToken <|> logicXorToken
  s <- getState
  let result = eval s exp1f op <$> binExpression
  result) <|> exprs

exprs :: ParsecT [Token] Memory IO Token
exprs = try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "exprs"
  trm1f <- term1
  op <- equalsToken <|> differentToken <|> greaterToken <|> greaterOrEqualToken <|> lessToken <|> lessOrEqualToken
  s <- getState
  let result = eval s trm1f op <$> exprs
  result) <|> term1

term1 :: ParsecT [Token] Memory IO Token
term1 = try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "term1"
  trm2f <- term2
  op <- addToken <|> subToken
  s <- getState
  let result = eval s trm2f op <$> term1
  result) <|> term2

term2 :: ParsecT [Token] Memory IO Token
term2 = try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "term2"
  fctr <- factor
  op <- multToken <|> divToken <|> modToken
  s <- getState
  let result = eval s fctr op <$> term2
  result) <|> factor

factor :: ParsecT [Token] Memory IO Token
factor = try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "factor"
  exT <- expT
  op <- powerToken
  s <- getState
  let result = eval s exT op <$> factor
  result) <|> expT

expT :: ParsecT [Token] Memory IO Token
expT = try functionCall <|> try recordCall <|> try arrayCall <|> try (do
  -- liftIO $ printf "\n%-20s%-10s%-20s\n" "StatementParser" "Call" "expT"
  pl <- parLToken
  ex <- expression
  pr <- parRToken
  return ex) <|> idToken <|> intToken <|> floatToken <|> boolToken <|> stringToken
