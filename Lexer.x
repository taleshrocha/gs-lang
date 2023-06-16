{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-

  $white+                                ;
  "--".*                                 ;
  --program                                { \p s -> Program (getLC p)}
  --var                                    { \p s -> Var (getLC p)}
  begin                                  { \p s -> Begin (getLC p)}
  end                                    { \p s -> End (getLC p)}
  --:                                      { \p s -> Colon (getLC p)}
  ";"                                    { \p s -> SemiColon (getLC p)}
  int                                    { \p s -> Type s (getLC p)}
  float                                  { \p s -> Type s (getLC p)}
  :=                                     { \p s -> Assign (getLC p)}
  --if                                     { \p s -> If (getLC p)}
  --then                                   { \p s -> Then (getLC p)}
  --write                                  { \p s -> Write (getLC p)}
  -->                                      { \p s -> Greater (getLC p)}
  "+"                                    { \p s -> Add (getLC p)}
  "-"                                    { \p s -> Sub (getLC p)}
  $digit+                                { \p s -> Int (read s) (getLC p)}
  $digit+ ("." $digit+)?                 { \p s -> Float (read s) (getLC p)}
  $alpha [$alpha $digit \_ \']*          { \p s -> Id s (getLC p)}
  \" $alpha [$alpha $digit ! \_ \']* \"  { \p s -> String s (getLC p)}

{
-- Each action has type :: AlexPosn -> String -> Token

-- The token type:
data Token =
  --Program (Int, Int)    |
  --Var (Int, Int)        |
  --Begin (Int, Int)      |
  --End (Int, Int)        |
  --Colon (Int, Int)      |
  SemiColon (Int, Int)  |
  Assign (Int, Int)     | 
  --If (Int, Int)         |
  --Then (Int, Int)       |
  --Write (Int, Int)      |
  --Greater (Int, Int)    |
  Add (Int, Int)        |
  Sub (Int, Int)        |
  Type String (Int, Int)|
  Id String (Int, Int)  |
  Int Int (Int, Int)    |
  Float Float (Int, Int)|
  String String (Int, Int)
  deriving (Eq,Show)

getLC (AlexPn _ l c) = (l, c)  

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
