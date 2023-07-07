module Evaluation where

import Lexer
import Tokens
import Memory
import Data.Bits

eval :: Token -> Token -> Token -> Token
eval (Int x p) (Add _) (Int y _) = Int (x + y) p
eval (Float x p) (Add _) (Float y _) = Float (x + y) p
eval (Int x p) (Add _) (Float y _) = Float ((fromIntegral x) + y) p
eval (Float x p) (Add _) (Int y _) = Float (x + (fromIntegral y)) p

eval (Int x p) (Sub _) (Int y _) = Int (x - y) p
eval (Float x p) (Sub _) (Float y _) = Float (x - y) p
eval (Int x p) (Sub _) (Float y _) = Float ((fromIntegral x) - y) p
eval (Float x p) (Sub _) (Int y _) = Float (x - (fromIntegral y)) p

eval (Int x p) (Mult _) (Int y _) = Int (x * y) p
eval (Float x p) (Mult _) (Float y _) = Float (x * y) p
eval (Int x p) (Mult _) (Float y _) = Float ((fromIntegral x) * y) p
eval (Float x p) (Mult _) (Int y _) = Float (x * (fromIntegral y)) p

eval (Int x p) (Div _) (Int y _) = Int (x `quot` y) p
eval (Float x p) (Div _) (Float y _) = Float (x / y) p
eval (Int x p) (Div _) (Float y _) = Float ((fromIntegral x) / y) p
eval (Float x p) (Div _) (Int y _) = Float (x / (fromIntegral y)) p

eval _ (Div _) (Int 0 _) = error ("Error: division by zero!")
eval _ (Div _) (Float 0.0 _) = error ("Error: division by zero!")

eval (Int x p) (Power _) (Int y _) = Int (x ^ y) p
eval (Float x p) (Power _) (Float y _) = Float (x ** y) p
eval (Int x p) (Power _) (Float y _) = Float ((fromIntegral x) ** y) p
eval (Float x p) (Power _) (Int y _) = Float (x ** (fromIntegral(y))) p

eval (Int x p) (Mod _) (Int y _) = Int (x `mod` y) p

eval _ (Mod _) (Int 0 _) = error ("Error: division by zero!")
eval _ (Mod _) (Float 0.0 _) = error ("Error: division by zero!")

eval (Bool x p) (LogicAnd _) (Bool y _) = Bool (x && y) p
eval (Bool x p) (LogicOr _) (Bool y _) = Bool (x || y) p
eval (Bool x p) (LogicXor _) (Bool y _) = Bool (x `xor` y) p

eval (Int x p) (Equals _) (Int y _) = Bool (x == y) p
eval (Float x p) (Equals _) (Float y _) = Bool (x == y) p
eval (Int x p) (Equals _) (Float y _) = Bool ((fromIntegral x) == y) p
eval (Float x p) (Equals _) (Int y _) = Bool (x == (fromIntegral y)) p
eval (Bool x p) (Equals _) (Bool y _) = Bool (x == y) p
eval (Char x p) (Equals _) (Char y _) = Bool (x == y) p
eval (String x p) (Equals _) (String y _) = Bool (x == y) p

eval (Int x p) (Different _) (Int y _) = Bool (x /= y) p
eval (Float x p) (Different _) (Float y _) = Bool (x /= y) p
eval (Int x p) (Different _) (Float y _) = Bool ((fromIntegral x) /= y) p
eval (Float x p) (Different _) (Int y _) = Bool (x /= (fromIntegral y)) p
eval (Bool x p) (Different _) (Bool y _) = Bool (x /= y) p
eval (Char x p) (Different _) (Char y _) = Bool (x /= y) p
eval (String x p) (Different _) (String y _) = Bool (x /= y) p

eval (Int x p) (Greater _) (Int y _) = Bool (x > y) p
eval (Float x p) (Greater _) (Float y _) = Bool (x > y) p
eval (Int x p) (Greater _) (Float y _) = Bool ((fromIntegral x) > y) p
eval (Float x p) (Greater _) (Int y _) = Bool (x > (fromIntegral y)) p
eval (Char x p) (Greater _) (Char y _) = Bool (x > y) p
eval (String x p) (Greater _) (String y _) = Bool (x > y) p

eval (Int x p) (GreaterOrEqual _) (Int y _) = Bool (x >= y) p
eval (Float x p) (GreaterOrEqual _) (Float y _) = Bool (x >= y) p
eval (Int x p) (GreaterOrEqual _) (Float y _) = Bool ((fromIntegral x) >= y) p
eval (Float x p) (GreaterOrEqual _) (Int y _) = Bool (x >= (fromIntegral y)) p
eval (Char x p) (GreaterOrEqual _) (Char y _) = Bool (x >= y) p
eval (String x p) (GreaterOrEqual _) (String y _) = Bool (x >= y) p

eval (Int x p) (Less _) (Int y _) = Bool (x < y) p
eval (Float x p) (Less _) (Float y _) = Bool (x < y) p
eval (Int x p) (Less _) (Float y _) = Bool ((fromIntegral x) < y) p
eval (Float x p) (Less _) (Int y _) = Bool (x < (fromIntegral y)) p
eval (Char x p) (Less _) (Char y _) = Bool (x < y) p
eval (String x p) (Less _) (String y _) = Bool (x < y) p

eval (Int x p) (LessOrEqual _) (Int y _) = Bool (x <= y) p
eval (Float x p) (LessOrEqual _) (Float y _) = Bool (x <= y) p
eval (Int x p) (LessOrEqual _) (Float y _) = Bool ((fromIntegral x) <= y) p
eval (Float x p) (LessOrEqual _) (Int y _) = Bool (x <= (fromIntegral y)) p
eval (Char x p) (LessOrEqual _) (Char y _) = Bool (x <= y) p
eval (String x p) (LessOrEqual _) (String y _) = Bool (x <= y) p

eval _ _ _ = error("Error on Evaluation -- eval: type error!")
