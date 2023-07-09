module Evaluation where

import Lexer
import Tokens
import Memory
import Data.Bits
import Data.Fixed (mod')

eval :: Types -> Token -> Types -> Types
eval (IntType x) (Add _) (IntType y) = IntType (x + y)
eval (FloatType x) (Add _) (FloatType y) = FloatType (x + y)
eval (IntType x) (Add _) (FloatType y) = FloatType ((fromIntegral x) + y)
eval (FloatType x) (Add _) (IntType y) = FloatType (x + (fromIntegral y))

eval (IntType x) (Sub _) (IntType y) = IntType (x - y)
eval (FloatType x) (Sub _) (FloatType y) = FloatType (x - y)
eval (IntType x) (Sub _) (FloatType y) = FloatType ((fromIntegral x) - y)
eval (FloatType x) (Sub _) (IntType y) = FloatType (x - (fromIntegral y))

eval (IntType x) (Mult _) (IntType y) = IntType (x * y)
eval (FloatType x) (Mult _) (FloatType y) = FloatType (x * y)
eval (IntType x) (Mult _) (FloatType y) = FloatType ((fromIntegral x) * y)
eval (FloatType x) (Mult _) (IntType y) = FloatType (x * (fromIntegral y))

eval (IntType x) (Div _) (IntType y)
  | y /= 0 = IntType (x `quot` y)
  | otherwise = error "Error: division by zero!"
eval (FloatType x) (Div _) (FloatType y)
  | y /= 0.0 = FloatType (x / y)
  | otherwise = error "Error: division by zero!"
eval (IntType x) (Div _) (FloatType y)
  | y /= 0.0 = FloatType ((fromIntegral x) / y)
  | otherwise = error "Error: division by zero!"
eval (FloatType x) (Div _) (IntType y)
  | y /= 0 = FloatType (x / (fromIntegral y))
  | otherwise = error "Error: division by zero!"

eval _ (Div _) (IntType 0) = error "Error: division by zero!"
eval _ (Div _) (FloatType 0.0) = error "Error: division by zero!"

eval (IntType x) (Power _) (IntType y) = IntType (x ^ y)
eval (FloatType x) (Power _) (FloatType y) = FloatType (x ** y)
eval (IntType x) (Power _) (FloatType y) = FloatType ((fromIntegral x) ** y)
eval (FloatType x) (Power _) (IntType y) = FloatType (x ** (fromIntegral y))

eval (IntType x) (Mod _) (IntType y) = IntType (x `mod` y)

eval (IntType x) (Mod _) (IntType y)
  | y /= 0 = IntType (x `mod` y)
  | otherwise = error "Error: modulus operation is not defined when the divisor is zero!"
eval (FloatType x) (Div _) (FloatType y)
  | y /= 0.0 = FloatType (x `mod'` y)
  | otherwise = error "Error: modulus operation is not defined when the divisor is zero!"
eval (IntType x) (Div _) (FloatType y)
  | y /= 0.0 = FloatType ((fromIntegral x) `mod'` y)
  | otherwise = error "Error: modulus operation is not defined when the divisor is zero!"
eval (FloatType x) (Div _) (IntType y)
  | y /= 0 = FloatType (x `mod'` (fromIntegral y))
  | otherwise = error "Error: modulus operation is not defined when the divisor is zero!"

eval _ (Mod _) (IntType 0) = error "Error: division by zero!"
eval _ (Mod _) (FloatType 0.0) = error "Error: division by zero!"

eval (BoolType x) (LogicAnd _) (BoolType y) = BoolType (x && y)
eval (BoolType x) (LogicOr _) (BoolType y) = BoolType (x || y)
eval (BoolType x) (LogicXor _) (BoolType y) = BoolType (x `xor` y)

eval (IntType x) (Equals _) (IntType y) = BoolType (x == y)
eval (FloatType x) (Equals _) (FloatType y) = BoolType (x == y)
eval (IntType x) (Equals _) (FloatType y) = BoolType ((fromIntegral x) == y)
eval (FloatType x) (Equals _) (IntType y) = BoolType (x == (fromIntegral y))
eval (BoolType x) (Equals _) (BoolType y) = BoolType (x == y)
eval (CharType x) (Equals _) (CharType y) = BoolType (x == y)
eval (StringType x) (Equals _) (StringType y) = BoolType (x == y)

eval (IntType x) (Different _) (IntType y) = BoolType (x /= y)
eval (FloatType x) (Different _) (FloatType y) = BoolType (x /= y)
eval (IntType x) (Different _) (FloatType y) = BoolType ((fromIntegral x) /= y)
eval (FloatType x) (Different _) (IntType y) = BoolType (x /= (fromIntegral y))
eval (BoolType x) (Different _) (BoolType y) = BoolType (x /= y)
eval (CharType x) (Different _) (CharType y) = BoolType (x /= y)
eval (StringType x) (Different _) (StringType y) = BoolType (x /= y)

eval (IntType x) (Greater _) (IntType y) = BoolType (x > y)
eval (FloatType x) (Greater _) (FloatType y) = BoolType (x > y)
eval (IntType x) (Greater _) (FloatType y) = BoolType ((fromIntegral x) > y)
eval (FloatType x) (Greater _) (IntType y) = BoolType (x > (fromIntegral y))
eval (CharType x) (Greater _) (CharType y) = BoolType (x > y)
eval (StringType x) (Greater _) (StringType y) = BoolType (x > y)

eval (IntType x) (GreaterOrEqual _) (IntType y) = BoolType (x >= y)
eval (FloatType x) (GreaterOrEqual _) (FloatType y) = BoolType (x >= y)
eval (IntType x) (GreaterOrEqual _) (FloatType y) = BoolType ((fromIntegral x) >= y)
eval (FloatType x) (GreaterOrEqual _) (IntType y) = BoolType (x >= (fromIntegral y))
eval (CharType x) (GreaterOrEqual _) (CharType y) = BoolType (x >= y)
eval (StringType x) (GreaterOrEqual _) (StringType y) = BoolType (x >= y)

eval (IntType x) (Less _) (IntType y) = BoolType (x < y)
eval (FloatType x) (Less _) (FloatType y) = BoolType (x < y)
eval (IntType x) (Less _) (FloatType y) = BoolType ((fromIntegral x) < y)
eval (FloatType x) (Less _) (IntType y) = BoolType (x < (fromIntegral y))
eval (CharType x) (Less _) (CharType y) = BoolType (x < y)
eval (StringType x) (Less _) (StringType y) = BoolType (x < y)

eval (IntType x) (LessOrEqual _) (IntType y) = BoolType (x <= y)
eval (FloatType x) (LessOrEqual _) (FloatType y) = BoolType (x <= y)
eval (IntType x) (LessOrEqual _) (FloatType y) = BoolType ((fromIntegral x) <= y)
eval (FloatType x) (LessOrEqual _) (IntType y) = BoolType (x <= (fromIntegral y))
eval (CharType x) (LessOrEqual _) (CharType y) = BoolType (x <= y)
eval (StringType x) (LessOrEqual _) (StringType y) = BoolType (x <= y)

eval _ _ _ = error "Error on eval -- cannot match types!"

evaluateExpression :: [Token] -> Memory -> Types
evaluateExpression  [] _ = error("Error on evaluateExpression -- No expression!")
evaluateExpression  (op : []) m = getType op m
evaluateExpression  (op1 : s : op2 : t) m = eval (getType op1 m) s (getType op2 m)

