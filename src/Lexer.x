{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-

  $white+                                        ;
  "//".*                                         ; -- comment
  begin                                          { \p s -> Begin (getLC p) }
  end                                            { \p s -> End (getLC p) }
  main                                           { \p s -> Main  (getLC p) }
  function                                       { \p s -> Function (getLC p) }
  procedure                                      { \p s -> Procedure (getLC p) }
  record                                         { \p s -> Record (getLC p) }

  const                                          { \p s -> Const (getLC p) }
  return                                         { \p s -> Return (getLC p) }

  print                                          { \p s -> Print (getLC p) }
  scan                                           { \p s -> Scan (getLC p) }

  if                                             { \p s -> If (getLC p) }
  elif                                           { \p s -> Elif (getLC p) }
  else                                           { \p s -> Else (getLC p) }

  switch                                         { \p s -> Switch (getLC p) }
  case                                           { \p s -> Case (getLC p) }

  while                                          { \p s -> While (getLC p) }
  for                                            { \p s -> For (getLC p) }

  -- Separators
  ":"                                            { \p s -> Colon (getLC p) }
  ";"                                            { \p s -> SemiColon (getLC p) }
  ","                                            { \p s -> Comma  (getLC p) }
  "("                                            { \p s -> ParL (getLC p) }
  ")"                                            { \p s -> ParR (getLC p) }
  "["                                            { \p s -> BracketL (getLC p) }
  "]"                                            { \p s -> BracketR (getLC p) }

  -- Type Tokens
  "typedef"                                      { \p s -> TypeDef s (getLC p) }
  int                                            { \p s -> Type s (getLC p) }
  float                                          { \p s -> Type s (getLC p) }
  bool                                           { \p s -> Type s (getLC p) }
  char                                           { \p s -> Type s (getLC p) }
  string                                         { \p s -> Type s (getLC p) }

  -- Op Tokens
  ":="                                           { \p s -> Assign (getLC p) }
  "+"                                            { \p s -> Add (getLC p) }
  "++"                                           { \p s -> AddUnary (getLC p) }
  "-"                                            { \p s -> Sub (getLC p) }
  "--"                                           { \p s -> SubUnary (getLC p) }
  "*"                                            { \p s -> Mult (getLC p) }
  "/"                                            { \p s -> Div (getLC p) }
  "**"                                           { \p s -> Power (getLC p) }
  "%"                                            { \p s -> Mod (getLC p) }

  -- Relational Tokens
  "=="                                           { \p s -> Equals (getLC p) }
  "!="                                           { \p s -> Different (getLC p) }
  ">"                                            { \p s -> Greater (getLC p) }
  ">="                                           { \p s -> GreaterOrEqual (getLC p) }
  "<"                                            { \p s -> Less (getLC p) }
  "<="                                           { \p s -> LessOrEqual (getLC p) }

  -- Boolean Tokens
  not                                            { \p s -> LogicNot (getLC p) }
  and                                            { \p s -> LogicAnd (getLC p) }
  or                                             { \p s -> LogicOr (getLC p) }
  xor                                            { \p s -> LogicXor (getLC p) }

  $digit+                                        { \p s -> Int (read s)  (getLC p) }
  $digit+ ("." $digit+)?                         { \p s -> Float (read s)  (getLC p) }
  True                                           { \p s -> Bool (read s) (getLC p) }
  False                                          { \p s -> Bool (read s) (getLC p) }
  \'.\'                                          { \p s -> Char (read s)  (getLC p) }
  $alpha [$alpha $digit \_ \']*                  { \p s -> Id s  (getLC p) }
  -- \" $alpha [$alpha $digit ! \_ \']* \"          { \p s -> String s (getLC p) }
  \".*\"          { \p s -> String s (getLC p) }

{
-- Each action has type :: AlexPosn -> String -> Token
-- The token type:
data Token =
  Begin           (Int, Int) |
  End             (Int, Int) |
  Main            (Int, Int) |
  Function        (Int, Int) |
  Procedure       (Int, Int) |
  Record          (Int, Int) |

  Const           (Int, Int) |
  Return          (Int, Int) |

  Print           (Int, Int) |
  Scan            (Int, Int) |

  If              (Int, Int) |
  Elif            (Int, Int) |
  Else            (Int, Int) |

  Switch          (Int, Int) |
  Case            (Int, Int) |

  While           (Int, Int) |
  For             (Int, Int) |

  Colon           (Int, Int) |
  SemiColon       (Int, Int) |
  Comma           (Int, Int) |
  ParL            (Int, Int) |
  ParR            (Int, Int) |
  BracketL        (Int, Int) |
  BracketR        (Int, Int) |

  TypeDef String  (Int, Int) |
  Type String     (Int, Int) |
  Int  Int        (Int, Int) |
  Float Float     (Int, Int) |
  Bool Bool       (Int, Int) |
  Char Char       (Int, Int) |
  String  String  (Int, Int) |

  Assign          (Int, Int) | 
  Add             (Int, Int) | 
  AddUnary        (Int, Int) | 
  Sub             (Int, Int) | 
  SubUnary        (Int, Int) | 
  Mult            (Int, Int) | 
  Div             (Int, Int) | 
  Power           (Int, Int) | 
  Mod             (Int, Int) | 

  Equals          (Int, Int) | 
  Different       (Int, Int) | 
  Greater         (Int, Int) | 
  GreaterOrEqual  (Int, Int) | 
  Less            (Int, Int) | 
  LessOrEqual     (Int, Int) | 
 
  LogicNot        (Int, Int) |
  LogicAnd        (Int, Int) |
  LogicOr         (Int, Int) |
  LogicXor        (Int, Int) |

  Id   String     (Int, Int)
  deriving (Eq,Show)

getLC (AlexPn _ l c) = (l, c)  

getTokens fn = unsafePerformIO (getTokensAux fn)
getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
