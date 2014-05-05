import System.Environment
import System.IO
import System.Exit

import qualified Grammar.Par as Par 
import Grammar.ErrM
import TypeChecker
import CodeGenerator

main :: IO ()
main = do 
  args <- getArgs
  case args of
    [] -> getContents >>= compile
    fs -> mapM_ readAndCompile fs

readAndCompile :: String -> IO ()
readAndCompile fp = do
  source <- readFile fp
  compile source

compile :: String -> IO ()
compile source = do
  case process source of
    Bad s    -> do
      hPutStrLn stderr "ERROR"
      putStrLn s
      exitWith $ ExitFailure 1
    Ok llvm -> do
      hPutStrLn stderr "OK"
      putStrLn llvm

process :: String -> Err String
process source = do
  let tokens = Par.myLexer source
  tree  <- Par.pProgram tokens
  typed_tree <- typecheck tree
  compilellvm typed_tree
