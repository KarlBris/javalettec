module CodeGenerator where

--import qualified Data.Map.Lazy as M
import Control.Monad
import Control.Monad.State
--import System.Environment (getArgs)
--import System.Exit (exitFailure)

import Grammar.Abs
import Grammar.Print
import Grammar.ErrM

compile :: String -> Program -> String
compile name p = unlines $ reverse $ code $ execState (compileProgram name p) emptyEnv

compileProgram :: String -> Program -> State Env ()
compileProgram name (Program topDefs) = undefined

compileTopDefs :: [TopDef] -> State Env ()
compileTopDefs [] = return ()
compileTopDefs defs@((FnDef t (Ident id) args block):rest) = undefined
    
compileStmt :: Stmt -> State Env ()
compileStmt s = case s of
  Empty                     -> undefined
  BStmt block               -> undefined
  Decl typ items            -> undefined
  Ass indent expr           -> undefined
  Incr ident                -> undefined
  Decr ident                -> undefined
  Ret expr                  -> undefined
  VRet                      -> undefined
  Cond expr stmt            -> undefined
  CondElse expr stmt1 stmt2 -> undefined
  While expr stmt           -> undefined
  SExp expr                 -> undefined

compileExpr :: Expr -> State Env ()
compileExpr e = case e of
  EVar ident             -> undefined
  ELitInt integer        -> undefined
  ELitDoub double        -> undefined
  ELitTrue               -> undefined
  ELitFalse              -> undefined
  EApp ident exprs       -> undefined
  EString string         -> undefined
  Neg expr               -> undefined
  Not expr               -> undefined
  EMul expr1 mulOp expr2 -> undefined
  EAdd expr1 addOp expr2 -> undefined
  ERel expr1 relOp expr2 -> undefined
  EAnd expr1 expr2       -> undefined
  EOr expr1 expr2        -> undefined
  ETyped expr typ        -> undefined
      


 

--- Old state stuff for reference purpose ---

data Env = E {
  addresses   :: [[(Ident,Address)]],
  nextLabel   :: Int,
  nextAddress :: Address,
  maxAddress  :: Address,
  stackSize   :: Int,
  maxSize     :: Int,
  code        :: [Instruction],
  className   :: String,
  functions   :: M.Map Ident String
  }

emptyEnv :: Env
emptyEnv = E {
  addresses = [[]],
  nextLabel = 0,
  nextAddress = 0,
  maxAddress = 1,
  stackSize = 0,
  maxSize = 1,
  code = [],
  className = "",
  functions = M.empty 
  }


type Instruction = String
type Address = Int


{-
emit :: Instruction -> State Env ()
emit i = modify (\env -> env{code = i : code env})

addVar :: Id -> Type -> State Env ()
addVar x t = modify (\env -> env {
  addresses = case addresses env of (scope:rest) -> (((x,nextAddress env):scope):rest),
  nextAddress = nextAddress env + typeSize t
  })

newLabel :: State Env String
newLabel = do 
  env <- get
  let nextLbl = nextLabel env
  modify (\env -> env { nextLabel = nextLabel env + 1 })
  return ("LABEL" ++ (show nextLbl))

typeSize :: Type -> Int
typeSize t = case t of
  Type_int -> 1
  Type_double -> 2
  Type_bool -> 1

lookupVar :: Id -> State Env Address
lookupVar x = do
  env <- get
  return $ look (addresses env) x 
 where
   look [] x = error $ "Unknown variable " ++ printTree x ++ "."
   look (scope:rest) x = case lookup x scope of
     Nothing -> look rest x
     Just a  -> a

newBlock :: State Env Address
newBlock = do
  modify (\env -> env {addresses = [] : addresses env})
  env <- get
  let ret = nextAddress env
  return ret

exitBlock :: Address -> State Env ()
exitBlock a = modify (\env -> env {
   addresses = tail (addresses env),
   nextAddress = a
   })

-}