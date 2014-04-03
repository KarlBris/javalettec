import System.Environment
import System.IO
import System.Exit

import qualified Grammar.Par as Par 
import Grammar.ErrM
import Grammar.Abs
import TypeChecker

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
    let tree = check source
    case tree of
      Bad s    -> do
        hPutStrLn stderr "ERROR"
        putStrLn s
        exitWith $ ExitFailure 1
      Ok  tree -> do
        hPutStrLn stderr "OK"
        print tree

check :: String -> Err Program
check source = do
    let tokens = Par.myLexer source
    tree  <- Par.pProgram tokens
    typecheck tree      
