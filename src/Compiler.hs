import System.Environment
import System.IO
import System.FilePath.Posix
import System.Exit
import System.Process

import qualified Grammar.Par as Par 
import Grammar.ErrM
import TypeChecker
import CodeGenerator

main :: IO ()
main = do 
  args <- getArgs
  case args of
    [] -> getContents >>= compile Nothing
    fs -> mapM_ readAndCompile fs

readAndCompile :: String -> IO ()
readAndCompile sourcePath = do
  source <- readFile sourcePath
  compile (Just sourcePath) source

compile :: Maybe String -> String -> IO ()
compile mbSrcFile source = do
  case process source of
    Bad s    -> do
      hPutStrLn stderr "ERROR"
      putStrLn s
      exitWith $ ExitFailure 1
    Ok llvm -> do
      hPutStrLn stderr "OK"
      putStrLn llvm
      case mbSrcFile of
        Nothing -> return ()
        Just srcPath -> do
          writeFile (dropExtension srcPath ++ ".ll") llvm
          _ <- system $ "llvm-as " ++ (dropExtension srcPath ++ ".ll")
          return ()

process :: String -> Err String
process source = do
  let tokens = Par.myLexer source
  tree  <- Par.pProgram tokens
  typed_tree <- typecheck tree
  compilellvm typed_tree
