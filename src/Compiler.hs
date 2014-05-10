import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.FilePath.Posix (dropExtension)
import System.Exit (exitWith, ExitCode(..))
import System.Process (system)

import Grammar.Par (myLexer, pProgram)
import Grammar.ErrM (Err(Bad, Ok))
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
  let tokens = myLexer source
  tree  <- pProgram tokens
  typed_tree <- typecheck tree
  compilellvm typed_tree
