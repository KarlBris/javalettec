import System.Environment
import System.IO

import qualified Grammar.Par as Par 
import Grammar.ErrM
import Grammar.Abs
import TypeChecker

main = do 
        args <- getArgs
        case args of
          [] -> hGetContents stdin >>= compile
          fs -> mapM_ readAndCompile fs

readAndCompile :: String -> IO ()
readAndCompile fp = do
  source <- readFile fp
  putStrLn $ "# " ++ fp ++ ":"
  compile source
  putStrLn ""

compile :: String -> IO ()
compile source = do
    let tree = check source
    case tree of
      Bad s    -> putStrLn "ERR" >> putStrLn s
      Ok  tree -> putStrLn "OK" >> print tree

check :: String -> Err Program
check source = do
    let tokens = Par.myLexer source
    tree  <- Par.pProgram tokens
    tree' <- typecheck tree
    return tree'
      
