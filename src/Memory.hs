module Memory where
import Lexer

-- [id, value, isConst]
type Variable = (String, Token, Bool)

-- [id, returnType, params, body]
type Function = (String, Token, [Variable], [Token])

type Scope = String

type Memory = (Scope, [Scope], [Variable], [Function], Bool)

insertVariable :: Variable -> Memory -> Memory
insertVariable var (currentScope, scopes, varTable, funcTable, is_on) =
  (currentScope, scopes, varTable ++ [var], funcTable, is_on)

insertFunction :: Function -> Memory -> Memory
insertFunction func (currentScope, scopes, varTable, funcTable, is_on) =
  (currentScope, scopes, varTable, funcTable ++ [func], is_on)

insertScope :: Scope -> Memory -> Memory
insertScope scope (currentScope, scopes, varTable, funcTable, is_on) =
  (scope, scope : scopes, varTable, funcTable, is_on)

updateVarOnMem :: Variable -> Memory -> Memory
updateVarOnMem var (currentScope, scopes, varTable, funcTable, is_on) =
  (currentScope, scopes, updateVariable var varTable, funcTable, is_on)

updateVariable :: Variable -> [Variable] -> [Variable]
updateVariable _ [] = error ("Error: variable not declared!")

updateVariable (id1, Type value1 pos1, is_const) ((id2, Type value2 pos2, True) : tail) =
  if id1 == id2 then (id1, Type value2 pos1, True) : tail
  else (id2, Type value2 pos2, True) : updateVariable (id1, Type value1 pos1, is_const) tail

updateVariable (id1, Type value1 pos1, is_const) ((id2, Type value2 pos2, False) : tail) =
  if id1 == id2 then error ("Error: trying to change the value of a constant!")
  else (id2, Type value2 pos2, True) : updateVariable (id1, Type value1 pos1, is_const) tail

updateVariable _ _ = error ("Error: invalid input pattern!") 

removeTable :: Variable -> Memory -> Memory
removeTable var (currentScope, scopes, varTable, funcTable, is_on) =
  (currentScope, scopes, removeVariable var varTable, funcTable, is_on)

removeVariable :: Variable -> [Variable] -> [Variable]
removeVariable (id1, type1, is_const1) ((id2, type2, is_const2) : tail) =
  if id1 == id2 then tail
  else (id2, type2, is_const2) : removeVariable (id1, type1, is_const1) tail

getValue :: Token -> [Variable] -> Float
getValue _ [] = error ("Error: variable not declared!")
getValue (Id id1 pos1) ((id2, (Float value2 p), b) : tail) = 
  if id1 == id2 then value2
  else getValue (Id id1 pos1) tail

getDefaultValue :: Token -> Token
getDefaultValue (Type "int" (l, c)) = Int 0 (l, c)
getDefaultValue (Type "float" (l, c)) = Float 0.0 (l, c)
getDefaultValue (Type "bool" (l, c)) = Bool False (l, c)
getDefaultValue (Type "char" (l, c)) = Char 'a' (l, c)
getDefaultValue (Type "string" (l, c)) = String "" (l, c)

compatible :: Token -> Token -> Bool
compatible (Int _ _) (Int _ _) = True
compatible (Float _ _) (Float _ _) = True
compatible (Bool _ _) (Bool _ _) = True
compatible (Char _ _) (Char _ _) = True
compatible (String _ _) (String _ _) = True

compatible (Float _ _) (Int _ _) = True
compatible (Int _ _) (Float _ _) = True

compatible _ _ = error ("Error: type mismatch!")
