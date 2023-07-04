module Memory where
import Lexer

-- Type and data declaration --------------------------------------------------

type Memory = (Scope, [Scope], [Variable], [Function], [Types], Bool)

type Scope = String

-- [id, scope, value, isConst]
type Variable = (String, Scope, Types, Bool)

-- [id, returnType, params, body]
type Function = (String, Types, [Variable], [Token])

data Types =
  IntType Int                             |
  FloatType Float                         |
  BoolType Bool                           |
  CharType Char                           |
  StringType String                       |
  RecordType (String, [(String, Types)])  |
  ArrayType (String, Int, Int, [Types])  -- name, maxSize, currentSize, load

-- Inserts --------------------------------------------------------------------

-- For Variables -----------------------

insertVariableOnMem :: Variable -> Memory -> Memory
insertVariableOnMem var (currentScope, scopes, varTable, funcTable, typeTable, isOn)  =
  (currentScope, scopes, insertVariable var varTable, funcTable, typeTable, isOn)

insertVariable :: Variable -> [Variable] -> [Variable]
insertVariable var [] = [var]

insertVariable (id1, scope1, type1, isConst1) ((id2, scope2, type2, True) : tail) =
  if id1 == id2 then error ("Error on Memory -- insertVariable: variable already declared!")
  else (id2, scope2, type2, True) : insertVariable (id1, scope1, type1, isConst1) tail

-- For Functions -----------------------

insertFunctionOnMem :: Function -> Memory -> Memory
insertFunctionOnMem func (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (currentScope, scopes, varTable, insertFunction func funcTable, typeTable, isOn)

insertFunction :: Function -> [Function] -> [Function]
insertFunction func [] = [func]

insertFunction (id1, return1, params1, body1) ((id2, return2, params2, body2) : tail) =
  if id1 == id2 then error ("Error: function already exists!")
  else (id2, return2, params2, body2) : insertFunction (id1, return1, params1, body1) tail

insertScope :: Scope -> Memory -> Memory
insertScope scope (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (scope, scope : scopes, varTable, funcTable, typeTable, isOn)

-- Updates --------------------------------------------------------------------

-- For Variables -----------------------

updateVarOnMem :: Variable -> Memory -> Memory
updateVarOnMem var (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (currentScope, scopes, updateVariable var varTable, funcTable, typeTable, isOn)

updateVariable :: Variable -> [Variable] -> [Variable]
updateVariable _ [] = error ("Error on Memory -- updateVariable: variable not declared!")

updateVariable (id1, scope1, type1, isConst1) ((id2, scope2, type2, True) : tail) =
  if id1 == id2 && scope1 == scope2 then error ("Error on Memory -- updateVariable: trying to change the value of a constant!")
  else (id2, scope2, type2, True) : updateVariable (id1, scope1, type1, isConst1) tail

updateVariable (id1, scope1, type1, isConst1) ((id2, scope2, type2, False) : tail) =
  if id1 == id2 && scope1 == scope2 then (id1, scope1, type1, True) : tail
  else (id2, scope2, type2, True) : updateVariable (id1, scope1, type1, isConst1) tail

-- Removes --------------------------------------------------------------------

-- For Variables -----------------------

removeVarFromMem :: Variable -> Memory -> Memory
removeVarFromMem var (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (currentScope, scopes, removeVariable var varTable, funcTable, typeTable, isOn)

removeVariable :: Variable -> [Variable] -> [Variable]
removeVariable (id1, scope1, type1, isConst1) ((id2, scope2, type2, isConst2) : tail) =
  if id1 == id2 && scope1 == scope2 then tail
  else (id2, scope2, type2, isConst2) : removeVariable (id1, scope1, type1, isConst1) tail

---- Getters ------------------------------------------------------------------

getType :: Token -> Memory -> Types
getType _ (_, _, [], _, _, _) = error ("Error on Memory -- getType: variable not declared *in this scope*!")

getType (Id id1 pos1) (currentScope, scopes, (id2, scope2, type2, _) : tail, funcs, types, isExecOn) =
  if id1 == id2 && currentScope == scope2 then type2
  else getType (Id id1 pos1) (currentScope, scopes, tail, funcs, types, isExecOn)

getDefaultValue :: Token -> Types
getDefaultValue (Type "int" (l, c)) = IntType 0
getDefaultValue (Type "float" (l, c)) = FloatType 0.0
getDefaultValue (Type "bool" (l, c)) = BoolType False
getDefaultValue (Type "char" (l, c)) = CharType 'a'
getDefaultValue (Type "string" (l, c)) = StringType ""

-- For Type Memory ---------------------

getCurrentScope :: Memory -> Scope
getCurrentScope (currentScope, _, _, _, _, _) = currentScope

getScopes :: Memory -> [Scope]
getScopes (_, scopes, _, _, _, _) = scopes

getVariables :: Memory -> [Variable]
getVariables (_, _, varTable, _, _, _) = varTable

getFunctions :: Memory -> [Function]
getFunctions (_, _, _, funcTable, _, _) = funcTable

getTypes :: Memory -> [Types]
getTypes (_, _, _, _, typeTable, _) = typeTable

getIsExecOn :: Memory -> Bool
getIsExecOn (_, _, _, _, _, isExecOn) = isExecOn

-- For Type Variable -------------------

getVariableName :: Variable -> String
getVariableName (name, _, _, _) = name

getVariableScope :: Variable -> Scope
getVariableScope (_, scope, _, _) = scope

getVariableType :: Variable -> Types
getVariableType (_, _, varType, _) = varType

getIsVariableConst :: Variable -> Bool
getIsVariableConst (_, _, _, isConst) = isConst

-- Misc -----------------------------------------------------------------------

compatible :: Token -> Token -> Bool
compatible (Int _ _) (Int _ _) = True
compatible (Float _ _) (Float _ _) = True
compatible (Bool _ _) (Bool _ _) = True
compatible (Char _ _) (Char _ _) = True
compatible (String _ _) (String _ _) = True

compatible (Float _ _) (Int _ _) = True
compatible (Int _ _) (Float _ _) = True

compatible _ _ = error ("Error on Memory -- compatible: type mismatch!")

-- Show --------------------------------

instance Show Types where
  show (IntType value) = show value
  show (FloatType value) = show value
  show (BoolType value) = show value
  show (CharType value) = show value
  show (StringType value) = show value
  show (RecordType (name, fields)) = show (name, fields)
  show (ArrayType (name, maxSize, currentSize, load)) = show (name, maxSize, currentSize, load)
