{-# LANGUAGE TemplateHaskell #-}

module CodeGenerator where

import qualified Data.Map.Lazy as M
import Control.Lens
import Control.Monad.State
import Data.List

import Grammar.Abs
import Grammar.ErrM

type GenM a = StateT Env Err a

data Env = Env {
  _code           :: [String],
  _currentExpType :: Type,

  _nextVariable   :: Int, -- getNextVariable :: State Env Int
  _nextLabel      :: Int,

  _stringLiterals :: [String],

  _llvmNames      :: M.Map String String -- TODO scoped
} deriving Show

emptyEnv :: Env
emptyEnv = Env {
  _code = [],
  _currentExpType = undefined,

  _nextVariable = 0,
  _nextLabel = 0,

  _llvmNames = M.empty,
  _stringLiterals = []
}

makeLenses ''Env

compilellvm :: Program -> Err String
compilellvm p = do
  st <- execStateT (compileProgram p) emptyEnv
  return $ unlines . reverse . (^. code) $ st 

compileProgram :: Program -> GenM ()
compileProgram (Program topDefs) = do
  mapM_ emit [
    "declare void @printInt(i32)",
    "declare void @printDouble(double)",
    "declare void @printString(i8*)",
    "declare i32 @readInt()",
    "declare double @readDouble()"
    ]
  compileTopDefs topDefs
  --emitStringLiterals

compileTopDefs :: [TopDef] -> GenM ()
compileTopDefs [] = return ()
compileTopDefs (FnDef typ (Ident name) args block : rest) = do
  emit ""
  emit $ "define " ++ makeLLVMType typ ++ " @" ++ name ++ "(" ++ llvmArgs ++ ") {"
  compileBlock block
  emit "}"
  compileTopDefs rest
  where
    llvmArgs = intercalate ", " (makeLLVMArgs args)
    makeLLVMArgs (Arg t (Ident n) : xs) = (makeLLVMType t ++ " %" ++ n) : makeLLVMArgs xs
    makeLLVMArgs [] = []

makeLLVMType :: Type -> String
makeLLVMType Int = "i32"
makeLLVMType Doub = "double"
makeLLVMType Bool = "i1"
makeLLVMType Void = "void"

compileBlock :: Block -> GenM ()
compileBlock (Block stmts) = mapM_ compileStmt stmts

compileStmt :: Stmt -> GenM ()
compileStmt s = case s of
  Empty                     -> return ()
  BStmt block               -> do
    label <- createBlock
    enterBlock label
    compileBlock
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

compileExpr :: Expr -> GenM ()
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

{-

(From slides)

We need to keep some state information during code generation.
This includes at least:

* Next number for generating register names (and labels).
* Definitions of global names for string literals.
* Lookup table to find LLVM name for Javalette variable name.
* Lookup table to find type of function.

-}

emit :: String -> GenM ()
emit s = code %= (s:)

addVar :: String -> GenM String
addVar x = do 
  nextVar <- use nextVariable
  nextVariable %= (+1)
  llvmNames %= M.insert x (show nextVar) -- TODO scoped
  return $ show nextVar

translateVar :: String -> GenM String
translateVar name = do
  env <- get
  -- todo
  return "TODO"


--- Old state stuff for reference purpose ---

{-

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