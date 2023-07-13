module Memory where
import Lexer
import Text.Parsec (getInput)
import Control.Monad (when)

-- Type and data declaration --------------------------------------------------

type Memory = (Scope, [Scope], [Variable], [Function], [Types], Bool)

type Scope = Int

type Pointer = (String, Scope, Bool)

-- [id, scope, value, isConst, pointer@(id, scope, isPointer)]
type Variable = (String, Scope, Types, Bool, Pointer)

-- [id, returnType, params, body]
type Function = (String, (Bool, Types), [Token], [Token])

data Types =
  IntType Int                             |
  FloatType Float                         |
  BoolType Bool                           |
  CharType Char                           |
  StringType String                       |
  VoidType                                |
  RecordType (String, [(String, Types)])  |
  ArrayType (String, Int, Int, [Types])  -- name, maxSize, currentSize, load

-- Inserts --------------------------------------------------------------------

-- For Variables -----------------------

insertVariableOnMem :: Types -> Variable -> Memory -> Memory
insertVariableOnMem type2 var (currentScope, scopes, varTable, funcTable, typeTable, isOn)  =
  (currentScope, scopes, insertVariable type2 var varTable, funcTable, typeTable, isOn)

insertVariable :: Types -> Variable -> [Variable] -> [Variable]
insertVariable type2 (id1, scope1, type1, isConst1, ptr1) [] = do
  if compatible type2 type1 then [(id1, scope1, convertTypes type1 type2, isConst1, ptr1)]
  else error ("Error on Memory -- insertVariable: variable "
    ++ show (id1, scope1, type2, isConst1)
    ++ " is not compatible with Type "
    ++ show type1
    ++ ".")

insertVariable type2 (id1, scope1, type1, isConst1, ptr1) ((id2, scope2, type3, isConst2, ptr) : tail) =
  if id1 == id2 && scope1 == scope2 then error ("Error on Memory -- insertVariable: variable (" ++ show (id1, scope1, type1, isConst1) ++") already declared *in this scope*!")
  else insertVariable type2 (id1, scope1, type1, isConst1, ptr1) tail ++ [(id2, scope2, type3, isConst2, ptr)]

-- For Functions -----------------------

insertFunctionOnMem :: Function -> Memory -> Memory
insertFunctionOnMem func (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (currentScope, scopes, varTable, insertFunction func funcTable, typeTable, isOn)

insertFunction :: Function -> [Function] -> [Function]
insertFunction func [] = [func]

insertFunction (id1, return1, params1, body1) ((id2, return2, params2, body2) : tail) =
  if id1 == id2 then error "Error: function already exists!"
  else insertFunction (id1, return1, params1, body1) tail ++ [(id2, return2, params2, body2)]

-- For Records -----------------------

insertRecOnMem :: Variable -> Memory -> Memory
insertRecOnMem var (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (currentScope, scopes, insertRecord var varTable, funcTable, typeTable, isOn)

insertRecord :: Variable -> [Variable] -> [Variable]
insertRecord var [] = [var]

insertRecord (id1, scope1, type1, isConst1, pt1) ((id2, scope2, RecordType (rid2, (fName2, fType2) : fds2), isConst2, pt2) : tail) =
  -- See if the variable exists
  if id1 == id2 && scope1 == scope2 then error ("Error on Memory -- insertRecord: variable (" ++ show (id1, scope1, type1, isConst1, pt1) ++") already declared *in this scope*!")
  else [(id2, scope2, RecordType (rid2, (fName2, fType2) : fds2), isConst2, pt2)] ++ insertRecord (id1, scope1, type1, isConst1, pt1) tail

-- For types -----------------------

insertTypeOnMem :: Types -> Memory -> Memory
insertTypeOnMem userType (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (currentScope, scopes, varTable, funcTable, insertType userType typeTable, isOn)

insertType :: Types -> [Types] -> [Types]
insertType userType [] = [userType]
insertType (RecordType (id1, fields1)) ((RecordType (id2, fields2)) : tail) =
  if id1 == id2 then error "Error: record already exists!"
  else insertType (RecordType (id1, fields1)) tail ++ [(RecordType (id2, fields2))]

-- For Scope -----------------------

insertScope :: Scope -> Memory -> Memory
insertScope scope (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (scope, scope : scopes, varTable, funcTable, typeTable, isOn)

removeScope :: Memory -> Memory
removeScope (currentScope, scope : scopes, varTable, funcTable, typeTable, isOn) =
  (head scopes, scopes, varTable, funcTable, typeTable, isOn)

getFunctionBody :: String -> Memory -> [Token]
getFunctionBody name (currentScope, scopes, varTable, (funcName, ret, params, body) : tail, typeTable, isOn)
  | name == funcName = body
  | null tail = error ("Error on Memory -- getFunctionBody: function (" ++ show name ++ ") not declared!")
  | otherwise = getFunctionBody name (currentScope, scopes, varTable, tail, typeTable, isOn)

-- Updates --------------------------------------------------------------------

-- For Variables -----------------------

updateVarOnMem :: Variable -> Memory -> Memory
updateVarOnMem var mem@(currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (currentScope, scopes, updateVariable mem var varTable, funcTable, typeTable, isOn)

updateVariable :: Memory -> Variable -> [Variable] -> [Variable]
updateVariable mem var [] = error ("Error on Memory -- updateVariable: variable (" ++ show var ++ ") not declared!")

updateVariable mem (id1, scope1, type1, isConst1, ptr1) ((id2, scope2, type2, isConst2, (ptrId, ptrScp, ptrTru)) : tail)
  | id1 == id2 && scope1 >= scope2 = if isConst2 then error ("Error on Memory -- updateVariable: trying to change the value of the " ++ show (id2, scope2, type2, True) ++ " constant!")
    else
      if compatible type2 type1 then do
          if ptrTru then (do
            let mem2 = updateVarOnMem (ptrId, ptrScp, type1, isConst2, ("",0,False)) mem
            getVariables (updateVarOnMem (id1, scope1, type1, isConst1, ptr1) mem2))
          else (id2, scope2, convertTypes type1 type2, isConst2, (ptrId, ptrScp, ptrTru)) : tail
      else error ("Error on Memory -- updateVariable: variable "
        ++ show (id2, scope2, type2, isConst2)
        ++ " is not compatible with Type "
        ++ show type1
        ++ ".")
  | null tail = error ("Error on Memory -- updateVariable: variable (" ++ show (id1, scope1, type1, isConst1) ++ ") not declared!")
  | otherwise = (id2, scope2, type2, isConst2, (ptrId, ptrScp, ptrTru)) : updateVariable mem (id1, scope1, type1, isConst1, ptr1) tail

-- For Records -----------------------

updateRecOnMem :: String -> Variable -> Memory -> Memory
updateRecOnMem fn var mem@(currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (currentScope, scopes, updateRecord mem fn var varTable, funcTable, typeTable, isOn)

updateRecord :: Memory -> String -> Variable -> [Variable] -> [Variable]
updateRecord mem fn var [] = error ("Error on Memory -- updateRecord: variable (" ++ show var ++ ") not declared!")

--updateRecord mem fName1 (id1, scope1, type1, isConst1, ptr1) ((id2, scope2, RecordType (rid2, []), isConst2, (ptrId, ptrScp, ptrTru)) : tail) =
--  error ("Error on Memory -- updateRecord: field (" ++ show fName1 ++ ") not found!")

updateRecord mem fName1 (id1, scope1, type1, isConst1, ptr1) ((id2, scope2, RecordType (rid2, (fName2, fType2) : fds2), isConst2, (ptrId, ptrScp, ptrTru)) : tail)
  | id1 == id2 && scope1 >= scope2 = 
    if isConst2 then 
      error ("Error on Memory -- updateRecord: trying to change the value of the " 
        ++ show (id2, scope2, RecordType (rid2, (fName2, fType2) : fds2), isConst2, (ptrId, ptrScp, ptrTru)) ++ " constant!")
    else 
      return (id2, scope2, RecordType (rid2, (updateFields fName1 (id1, scope1, type1, isConst1, ptr1) ((fName2, fType2) : fds2))), isConst2, (ptrId, ptrScp, ptrTru))
  | null tail = error ("Error on Memory -- updateRecord: variable (" ++ show (id1, scope1, type1, isConst1) ++ ") not declared!")
  | otherwise = (id2, scope2, RecordType (rid2, (fName2, fType2) : fds2), isConst2, (ptrId, ptrScp, ptrTru)) : updateRecord mem fName1 (id1, scope1, type1, isConst1, ptr1) tail

updateFields :: String -> Variable -> [(String, Types)] -> [(String, Types)]
updateFields fName1 (id1, scope1, type1, isConst1, ptr1)  [] = 
  error ("Error on Memory -- updateField: field (" ++ show fName1 ++ ") not found!")

updateFields fName1 (id1, scope1, type1, isConst1, ptr1) ((fName2, fType2) : fds2) =
  if fName1 == fName2 && compatible fType2 type1 then
    ((fName2, convertTypes type1 fType2) : fds2)
  else 
    ((fName2, fType2) : updateFields fName1 (id1, scope1, type1, isConst1, ptr1) fds2)
    
-- Removes --------------------------------------------------------------------

-- For Variables -----------------------

cleanMemFromScope :: Scope -> Memory -> Memory
cleanMemFromScope scp (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (currentScope, scopes, cleanMem scp varTable, funcTable, typeTable, isOn)

cleanMem :: Scope -> [Variable] -> [Variable]
cleanMem scp ((id, scope, typ, isConst, ptr) : tail)
  | scp == scope = tail
  | otherwise = (id, scope, typ, isConst, ptr) : cleanMem scp tail

removeVarFromMem :: Variable -> Memory -> Memory
removeVarFromMem var (currentScope, scopes, varTable, funcTable, typeTable, isOn) =
  (currentScope, scopes, removeVariable var varTable, funcTable, typeTable, isOn)

removeVariable :: Variable -> [Variable] -> [Variable]
removeVariable (id1, scope1, type1, isConst1, ptr1) ((id2, scope2, type2, isConst2, ptr2) : tail) =
  if id1 == id2 && scope1 == scope2 then tail
  else (id2, scope2, type2, isConst2, ptr2) : removeVariable (id1, scope1, type1, isConst1, ptr1) tail

---- Getters ------------------------------------------------------------------

getVariableMem :: String -> Scope -> Memory -> Variable
getVariableMem tk sco (currentScope, scopes, varTable, funcTable, typeTable, isOn) = getVariable tk sco varTable

getVariable :: String -> Scope -> [Variable] -> Variable
getVariable var sc [] = error ("Error on Memory -- getVariable: variable (" ++ show var ++ ") not declared!")

getVariable id scope ((id2, scope2, type2, is_const, ptr) : tail) =
  if id == id2 && scope >= scope2 then (id2, scope2, type2, is_const, ptr)
  else getVariable id scope tail

getType :: Token -> Memory -> Types
getType tkn (currentScope, scopes, varTable, funcTable, typeTable, isOn) = getTypeAux tkn currentScope varTable

getTypeAux :: Token -> Scope -> [Variable] -> Types
getTypeAux (Id id pos) sco [] = error ("Error on Memory -- getType: variable not declared (" ++ show (Id id pos) ++ ") *in this scope*!")

getTypeAux (Id id pos) sco ((id2, scope2, type2, is_const, ptr) : tail)
  | id == id2 && sco >= scope2 = type2
  | null tail = error ("Error on Memory -- getType: variable not declared (" ++ show (Id id pos) ++ ") *in this scope*!")
  | otherwise = getTypeAux (Id id pos) sco tail

getTypeAux (Int value pos) _ _ = IntType value
getTypeAux (Float value pos) _ _ = FloatType value
getTypeAux (Bool value pos) _ _ = BoolType value
getTypeAux (String value pos) _ _ = StringType value

getTypeAux (Type "int" (l, c)) _ _ = IntType 0
getTypeAux (Type "float" (l, c)) _ _ = FloatType 0.0
getTypeAux (Type "bool" (l, c)) _ _ = BoolType False
getTypeAux (Type "char" (l, c)) _ _ = CharType 'a'
getTypeAux (Type "string" (l, c)) _ _ = StringType ""

getTypeAux _ _ _ = error "Error on Memory -- getType: invalid Token to Types convertion!"

getDefaultValue :: Token -> Types
getDefaultValue (Type "int" (l, c)) = IntType 0
getDefaultValue (Type "float" (l, c)) = FloatType 0.0
getDefaultValue (Type "bool" (l, c)) = BoolType False
getDefaultValue (Type "char" (l, c)) = CharType 'a'
getDefaultValue (Type "string" (l, c)) = StringType ""

-- TODO add this to getDefaultValue
getDefaultRecordValue :: Token -> Memory -> Types
getDefaultRecordValue (Id id p) (_, _, _, _, [], _) = error ("Error on Memory -- getDefautValue: variable no such type (" ++ show (Id id p) ++ ") *in this scope*!")

getDefaultRecordValue (Id id1 pos1) (currentScope, scopes, vars, funcs, RecordType (id2, fd) : tail, isExecFn) =
  if id1 == id2 then RecordType (id1, fd)
  else getDefaultRecordValue (Id id1 pos1) (currentScope, scopes, vars, funcs, tail, isExecFn)

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
getVariableName (name, _, _, _, _) = name

getVariableScope :: Variable -> Scope
getVariableScope (_, scope, _, _, _) = scope

getVariableType :: Variable -> Types
getVariableType (_, _, varType, _, _) = varType

getIsVariableConst :: Variable -> Bool
getIsVariableConst (_, _, _, isConst, _) = isConst

getIsVariablePointer :: Variable -> Pointer
getIsVariablePointer (_, _, _, _, ptr1) = ptr1

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
