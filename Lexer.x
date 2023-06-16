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
  begin                                  { \p s -> Begin (getLC p)}
  end                                    { \p s -> End (getLC p)}
  ";"                                    { \p s -> SemiColon (getLC p)}
  "("                                    { \p s -> ParL (getLC p)}
  ")"                                    { \p s -> ParR (getLC p)}
  int                                    { \p s -> Type s (getLC p)}
  float                                  { \p s -> Type s (getLC p)}
  :=                                     { \p s -> Assign (getLC p)}
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
  Begin (Int, Int)      |
  End (Int, Int)        |
  SemiColon (Int, Int)  |
  ParL      (Int, Int)  |
  ParR      (Int, Int)  |
  Assign (Int, Int)     |
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
