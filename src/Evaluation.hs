module Evaluation where

import Lexer
import Tokens
import Memory
import Data.Bits
import Data.Fixed (mod')

idTokenToTypeToken :: Token -> String -> [Variable] -> Token
idTokenToTypeToken (Id id _) sc [] = error ("Error on eval _uation -- eval _Aux: variable (" ++ show id ++ ") not declared in " ++ sc ++ " scope!")

idTokenToTypeToken (Id id p) scope ((id2, scope2, type2, is_const) : tail) =
  if id == id2 && scope == scope2 then
    case type2 of
        (IntType x) -> Int x p
        (FloatType x) -> Float x p
        (BoolType x) -> Bool x p
        (CharType x) -> Char x p
        (StringType x) -> String x p
        (ArrayType (a, b, c, value)) -> Array p
        (MatrixType x) -> Matrix p
        _ -> error "Error on Evaluation -- idTokenToTypeToken: invalid variable type!"
  else idTokenToTypeToken (Id id p) scope tail

--eval _ _ _ = error "Error on eval -- cannot match types!"

eval :: Memory -> Token -> Token -> Token -> Token
eval mem (Id id1 p1) opToken (Id id2 p2) = eval mem (idTokenToTypeToken (Id id1 p1) (getCurrentScope mem) (getVariables mem)) opToken (idTokenToTypeToken (Id id2 p2) (getCurrentScope mem) (getVariables mem))
eval mem (Id id p) opToken typeToken = eval mem (idTokenToTypeToken (Id id p) (getCurrentScope mem) (getVariables mem)) opToken typeToken
eval mem typeToken opToken (Id id p) = eval mem typeToken opToken (idTokenToTypeToken (Id id p) (getCurrentScope mem) (getVariables mem))

eval _ (Int x p) (Add _) (Int y _) = Int (x + y) p
eval _ (Float x p) (Add _) (Float y _) = Float (x + y) p
eval _ (Int x p) (Add _) (Float y _) = Float (fromIntegral x + y) p
eval _ (Float x p) (Add _) (Int y _) = Float (x + fromIntegral y) p
eval _ (String x p) (Add _) (String y _) = String (x ++ y) p

eval _ (Int x p) (Sub _) (Int y _) = Int (x - y) p
eval _ (Float x p) (Sub _) (Float y _) = Float (x - y) p
eval _ (Int x p) (Sub _) (Float y _) = Float (fromIntegral x - y) p
eval _ (Float x p) (Sub _) (Int y _) = Float (x - fromIntegral y) p

eval _ (Int x p) (Mult _) (Int y _) = Int (x * y) p
eval _ (Float x p) (Mult _) (Float y _) = Float (x * y) p
eval _ (Int x p) (Mult _) (Float y _) = Float (fromIntegral x * y) p
eval _ (Float x p) (Mult _) (Int y _) = Float (x * fromIntegral y) p

eval _ (Int x p) (Div _) (Int y _) = Int (x `quot` y) p
eval _ (Float x p) (Div _) (Float y _) = Float (x / y) p
eval _ (Int x p) (Div _) (Float y _) = Float (fromIntegral x / y) p
eval _ (Float x p) (Div _) (Int y _) = Float (x / fromIntegral y) p

eval _ _ (Div _) (Int 0 _) = error "Error: division by zero!"
eval _ _ (Div _) (Float 0.0 _) = error "Error: division by zero!"

eval _ (Int x p) (Power _) (Int y _) = Int (x ^ y) p
eval _ (Float x p) (Power _) (Float y _) = Float (x ** y) p
eval _ (Int x p) (Power _) (Float y _) = Float (fromIntegral x ** y) p
eval _ (Float x p) (Power _) (Int y _) = Float (x ** fromIntegral y) p

eval _ (Int x p) (Mod _) (Int y _) = Int (x `mod` y) p

eval _ _ (Mod _) (Int 0 _) = error "Error: modulus operation is not defined when the divisor is zero!"
eval _ _ (Mod _) (Float 0.0 _) = error "Error: modulus operation is not defined when the divisor is zero!"

eval _ _ (LogicNot _) (Bool y p) = Bool (not y) p
eval _ (Bool x p) (LogicAnd _) (Bool y _) = Bool (x && y) p
eval _ (Bool x p) (LogicOr _) (Bool y _) = Bool (x || y) p
eval _ (Bool x p) (LogicXor _) (Bool y _) = Bool (x `xor` y) p

eval _ (Int x p) (Equals _) (Int y _) = Bool (x == y) p
eval _ (Float x p) (Equals _) (Float y _) = Bool (x == y) p
eval _ (Int x p) (Equals _) (Float y _) = Bool (fromIntegral x == y) p
eval _ (Float x p) (Equals _) (Int y _) = Bool (x == fromIntegral y) p
eval _ (Bool x p) (Equals _) (Bool y _) = Bool (x == y) p
eval _ (Char x p) (Equals _) (Char y _) = Bool (x == y) p
eval _ (String x p) (Equals _) (String y _) = Bool (x == y) p

eval _ (Int x p) (Different _) (Int y _) = Bool (x /= y) p
eval _ (Float x p) (Different _) (Float y _) = Bool (x /= y) p
eval _ (Int x p) (Different _) (Float y _) = Bool (fromIntegral x /= y) p
eval _ (Float x p) (Different _) (Int y _) = Bool (x /= fromIntegral y) p
eval _ (Bool x p) (Different _) (Bool y _) = Bool (x /= y) p
eval _ (Char x p) (Different _) (Char y _) = Bool (x /= y) p
eval _ (String x p) (Different _) (String y _) = Bool (x /= y) p

eval _ (Int x p) (Greater _) (Int y _) = Bool (x > y) p
eval _ (Float x p) (Greater _) (Float y _) = Bool (x > y) p
eval _ (Int x p) (Greater _) (Float y _) = Bool (fromIntegral x > y) p
eval _ (Float x p) (Greater _) (Int y _) = Bool (x > fromIntegral y) p
eval _ (Char x p) (Greater _) (Char y _) = Bool (x > y) p
eval _ (String x p) (Greater _) (String y _) = Bool (x > y) p

eval _ (Int x p) (GreaterOrEqual _) (Int y _) = Bool (x >= y) p
eval _ (Float x p) (GreaterOrEqual _) (Float y _) = Bool (x >= y) p
eval _ (Int x p) (GreaterOrEqual _) (Float y _) = Bool (fromIntegral x >= y) p
eval _ (Float x p) (GreaterOrEqual _) (Int y _) = Bool (x >= fromIntegral y) p
eval _ (Char x p) (GreaterOrEqual _) (Char y _) = Bool (x >= y) p
eval _ (String x p) (GreaterOrEqual _) (String y _) = Bool (x >= y) p

eval _ (Int x p) (Less _) (Int y _) = Bool (x < y) p
eval _ (Float x p) (Less _) (Float y _) = Bool (x < y) p
eval _ (Int x p) (Less _) (Float y _) = Bool (fromIntegral x < y) p
eval _ (Float x p) (Less _) (Int y _) = Bool (x < fromIntegral y) p
eval _ (Char x p) (Less _) (Char y _) = Bool (x < y) p
eval _ (String x p) (Less _) (String y _) = Bool (x < y) p

eval _ (Int x p) (LessOrEqual _) (Int y _) = Bool (x <= y) p
eval _ (Float x p) (LessOrEqual _) (Float y _) = Bool (x <= y) p
eval _ (Int x p) (LessOrEqual _) (Float y _) = Bool (fromIntegral x <= y) p
eval _ (Float x p) (LessOrEqual _) (Int y _) = Bool (x <= fromIntegral y) p
eval _ (Char x p) (LessOrEqual _) (Char y _) = Bool (x <= y) p
eval _ (String x p) (LessOrEqual _) (String y _) = Bool (x <= y) p

--eval _ (ArrayType (t, m, c, [])) (AddUnary _) (Float y _) = ArrayType (t, m, c, [y])
--eval _ (ArrayType a p) (AddUnary _) (Float y _) = Array (a ++ [y]) p
--eval _ (Matrix [] p) (AddUnary _) (Array x _) = Matrix ([x]) p
--eval _ (Matrix m p) (AddUnary _) (Array x _) = Matrix (m ++ [x]) p

eval _ _ _ _ = error "Error on eval _ -- cannot match types!"


arrangeAdd :: Memory -> Types -> Token -> Types
arrangeAdd s (ArrayType (t, m, c, [])) y = ArrayType (t, m, c, [getType y s])

arrangeEval :: Memory -> Types -> Token -> Token -> Types
arrangeEval s (ArrayType (t, m, c, [])) (AddUnary _) y = ArrayType (t, m, c, [getType y s])
