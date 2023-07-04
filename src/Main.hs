module Main (main) where
  
import Lexer
import Parser

import System.IO.Unsafe
import System.Environment

main :: IO ()
main = do
  (fileName : args) <- getArgs
  case unsafePerformIO (parser (getTokens fileName)) of
    { Left err -> print err; 
      Right ans -> print ans -- putStr ""
    }
