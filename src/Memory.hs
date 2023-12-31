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

insertVariable (id1, scope1, type1, isConst1) ((id2, scope2, type2, isConst2) : tail) =
  if id1 == id2 && scope1 == scope2 then error ("Error on Memory -- insertVariable: variable (" ++ show (id1, scope1, type1, isConst1) ++") already declared *in this scope*!")
  else (id2, scope2, type2, isConst2) : insertVariable (id1, scope1, type1, isConst1) tail

-- For Functions -----------------------

insertFunctionOnMem :: Function -> Memory -> Memory
insertFunctionOnMem func (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (currentScope, scopes, varTable, insertFunction func funcTable, typeTable, isOn)

insertFunction :: Function -> [Function] -> [Function]
insertFunction func [] = [func]

insertFunction (id1, return1, params1, body1) ((id2, return2, params2, body2) : tail) =
  if id1 == id2 then error "Error: function already exists!"
  else (id2, return2, params2, body2) : insertFunction (id1, return1, params1, body1) tail

-- For Scope -----------------------

insertScope :: Scope -> Memory -> Memory
insertScope scope (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (scope, scope : scopes, varTable, funcTable, typeTable, isOn)

-- Updates --------------------------------------------------------------------

-- For Variables -----------------------

updateVarOnMem :: Variable -> Memory -> Memory
updateVarOnMem var (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (currentScope, scopes, updateVariable var varTable, funcTable, typeTable, isOn)

updateVariable :: Variable -> [Variable] -> [Variable]
updateVariable var [] = error ("Error on Memory -- updateVariable: variable (" ++ show var ++ ") not declared!")

updateVariable (id1, scope1, type1, isConst1) ((id2, scope2, type2, isConst2) : tail) =
  if id1 == id2 && scope1 == scope2 then
    if isConst2 == True then error ("Error on Memory -- updateVariable: trying to change the value of the " ++ show (id2, scope2, type2, True) ++ " constant!")
    else
      if compatible type2 type1 then (id1, scope1, convertTypes type1 type2, isConst2) : tail
      else error ("Error on Memory -- updateVariable: variable "
        ++ show (id2, scope2, type2, False)
        ++ " is not compatible with Type "
        ++ show type1
        ++ ".")
  else (id2, scope2, type2, isConst2) : updateVariable (id1, scope1, type1, isConst1) tail

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

getVariable :: String -> String -> [Variable] -> Variable
getVariable var sc [] = error ("Error on Memory -- getVariable: variable (" ++ show var ++ ") not declared in " ++ sc ++ " scope!")

getVariable id scope ((id2, scope2, type2, is_const) : tail) =
  if id == id2 && scope == scope2 then (id2, scope2, type2, is_const)
  else getVariable id scope tail

getType :: Token -> Memory -> Types
getType (Id id pos) (_, _, [], _, _, _) = error ("Error on Memory -- getType: variable not declared (" ++ show (Id id pos) ++ ") *in this scope*!")

getType (Id id1 pos1) (currentScope, scopes, (id2, scope2, type2, _) : tail, funcs, types, isExecOn) =
  if id1 == id2 && currentScope == scope2 then type2
  else getType (Id id1 pos1) (currentScope, scopes, tail, funcs, types, isExecOn)

getType (Int value pos) _ = IntType value
getType (Float value pos) _ = FloatType value
getType (Bool value pos) _ = BoolType value
getType (String value pos) _ = StringType value

getDefaultValue :: Token -> Types
getDefaultValue (Type "int" (l, c)) = IntType 0
getDefaultValue (Type "float" (l, c)) = FloatType 0.0
getDefaultValue (Type "bool" (l, c)) = BoolType False
getDefaultValue (Type "char" (l, c)) = CharType 'a'
getDefaultValue (Type "string" (l, c)) = StringType ""

getBoolValue :: Token -> Bool
getBoolValue (Bool value pos) = value
getBoolValue _ = error "Error on Memory -- getBoolValue"

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

setExecOnOff :: Memory -> Memory
setExecOnOff (currentScope, scopes, varTable, funcTable, typeTable, isExecOn) = (currentScope, scopes, varTable, funcTable, typeTable, not isExecOn)

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

compatible :: Types -> Types -> Bool
compatible (IntType _) (IntType _) = True
compatible (FloatType _) (FloatType _) = True
compatible (BoolType _) (BoolType _) = True
compatible (CharType _) (CharType _) = True
compatible (StringType _) (StringType _) = True

compatible (FloatType _) (IntType _) = True
compatible (IntType _) (FloatType _) = False

compatible _ _ = error "Error on Memory -- compatible: type mismatch!"

convertTypes :: Types -> Types -> Types
convertTypes (IntType v) (FloatType _) = FloatType (fromIntegral v)
convertTypes type1 _ = type1

-- Show --------------------------------

instance Show Types where
  show (IntType value) = "(IntType, " ++ show value ++ ")"
  show (FloatType value) = "(FloatType, " ++ show value ++ ")"
  show (BoolType value) = "(BoolType, " ++ show value ++ ")"
  show (CharType value) = "(CharType, " ++ show value ++ ")"
  show (StringType value) = "(StringType, " ++ show value ++ ")"
  show (RecordType (name, fields)) = "(RecordType, " ++ show (name, fields) ++ ")"
  show (ArrayType (name, maxSize, currentSize, load)) =
    "(ArrayType, " ++ show (name, maxSize, currentSize, load) ++ ")"
