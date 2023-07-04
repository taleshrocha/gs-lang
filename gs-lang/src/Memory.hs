module Memory where
import Lexer

type Memory = (Scope, [Scope], [Variable], [Function], [Types], Bool)

type Scope = String

-- [id, value, isConst]
type Variable = (String, Types, Bool)

-- [id, returnType, params, body]
type Function = (String, Token, [Variable], [Token])

data Types =
  IntType Int |
  FloatType Float |
  BoolType Bool |
  CharType Char |
  StringType String |
  RecordType (String, [(String, Types)]) |
  ArrayType (String, Int, Int, [Types])  -- name, max_size, current_size, load


insertVariableOnMem :: Variable -> Memory -> Memory
insertVariableOnMem var (currentScope, scopes, varTable, funcTable, typeTable, is_on)  =
  (currentScope, scopes, insertVariable var varTable, funcTable, typeTable, is_on)

insertVariable :: Variable -> [Variable] -> [Variable]
insertVariable var [] = [var]

insertVariable (id1, type1, is_const) ((id2, type2, True) : tail) =
  if id1 == id2 then error ("Error: variable already declared!")
  else (id2, type2, True) : insertVariable (id1, type1, is_const) tail

insertFunctionOnMem :: Function -> Memory -> Memory
insertFunctionOnMem func (currentScope, scopes, varTable, funcTable, typeTable, is_on) =
  (currentScope, scopes, varTable, insertFunction func funcTable, typeTable, is_on)

insertFunction :: Function -> [Function] -> [Function]
insertFunction func [] = [func]

insertFunction (id1, return1, params1, body1) ((id2, return2, params2, body2) : tail) =
  if id1 == id2 then error ("Error: function already exists!")
  else (id2, return2, params2, body2) : insertFunction (id1, return1, params1, body1) tail

insertScope :: Scope -> Memory -> Memory
insertScope scope (currentScope, scopes, varTable, funcTable, typeTable, is_on) =
  (scope, scope : scopes, varTable, funcTable, typeTable, is_on)

updateVarOnMem :: Variable -> Memory -> Memory
updateVarOnMem var (currentScope, scopes, varTable, funcTable, typeTable, is_on) =
  (currentScope, scopes, updateVariable var varTable, funcTable, typeTable, is_on)

updateVariable :: Variable -> [Variable] -> [Variable]
updateVariable _ [] = error ("Error: variable not declared!")

updateVariable (id1, Type value1 pos1, is_const) ((id2, Type value2 pos2, True) : tail) =
  if id1 == id2 then (id1, Type value1 pos2, True) : tail
  else (id2, Type value2 pos2, True) : updateVariable (id1, Type value1 pos1, is_const) tail

updateVariable (id1, Type value1 pos1, is_const) ((id2, Type value2 pos2, False) : tail) =
  if id1 == id2 then error ("Error: trying to change the value of a constant!")
  else (id2, Type value2 pos2, True) : updateVariable (id1, Type value1 pos1, is_const) tail

updateVariable _ _ = error ("Error: invalid input pattern on updateVariable!") 

removeVarFromMem :: Variable -> Memory -> Memory
removeVarFromMem var (currentScope, scopes, varTable, funcTable, typeTable, is_on) =
  (currentScope, scopes, removeVariable var varTable, funcTable, typeTable, is_on)

removeVariable :: Variable -> [Variable] -> [Variable]
removeVariable (id1, type1, is_const1) ((id2, type2, is_const2) : tail) =
  if id1 == id2 then tail
  else (id2, type2, is_const2) : removeVariable (id1, type1, is_const1) tail

getType :: Token -> [Variable] -> Token
getType _ [] = error ("Error: variable not declared!")

getType (Id id1 pos1) ((id2, type2, _) : tail) = 
  if id1 == id2 then type2
  else getType (Id id1 pos1) tail

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