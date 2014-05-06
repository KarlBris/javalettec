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
  _currentExpType :: Maybe Type,

  _nextVariable   :: Int,
  _nextLabel      :: Int,
  _nextString     :: Int,

  _stringLiterals :: [String],
  _llvmNames      :: [M.Map String String],
  _continueTo     :: [String]
} deriving Show

emptyEnv :: Env
emptyEnv = Env {
  _code = [],
  _currentExpType = Nothing,

  _nextVariable = 0,
  _nextLabel = 0,
  _nextString = 0,

  _stringLiterals = [],
  _llvmNames = [],
  _continueTo = []
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

  scoped $ do
    args' <- llvmArgs
    emit $ "define " ++ makeLLVMType typ ++ " @" ++ name ++ "(" ++ args' ++ ") {"

    lbl <- newLabel
    emit $ "entry: branch Label " ++ lbl
    emit $ lbl ++ ":"
    compileBlock block

    emit "}"

  compileTopDefs rest
  where
    llvmArgs :: GenM String
    llvmArgs = do
      args' <- makeLLVMArgs args
      return $ intercalate ", " (args')

    makeLLVMArgs :: [Arg] -> GenM [String]
    makeLLVMArgs (Arg t (Ident n) : xs) = do
      newVar <- (addVar n)
      rest <- makeLLVMArgs xs
      return $ (makeLLVMType t ++ " %" ++ newVar) : rest
    makeLLVMArgs [] = return []

makeLLVMType :: Type -> String
makeLLVMType Int = "i32"
makeLLVMType Doub = "double"
makeLLVMType Bool = "i1"
makeLLVMType Void = "void"

compileBlock :: Block -> GenM ()
compileBlock (Block stmts) = do
  scoped $ mapM_ compileStmt stmts

compileStmt :: Stmt -> GenM ()
compileStmt s = case s of
  Empty                     -> return ()
  BStmt block               -> do
    scoped $ compileBlock block
  Decl typ ((NoInit (Ident ident)):rest)    -> do
    ident' <- addVar ident
    let typ' = makeLLVMType typ
    emit $ "%" ++ ident' ++ " = alloca " ++ typ'
    emit $ "store " ++ typ' ++ " 0, " ++ typ' ++ "* %" ++ ident'
    compileStmt (Decl typ rest)
  Decl typ ((Init (Ident ident) expr):rest) -> do
    ident' <- addVar ident
    let typ' = makeLLVMType typ
    emit $ "%" ++ ident' ++ " = alloca " ++ typ'
    compileExpr ident' expr
    compileStmt (Decl typ rest)
  Decl _ []                         -> do
    return ()
  Ass (Ident ident) expr            -> do
    ident' <- lookupVar ident
    compileExpr ident' expr
  Incr ident                -> return ()
  Decr ident                -> return ()
  Ret expr                  -> return ()
  VRet                      -> return ()
  Cond expr stmt            -> return ()
  CondElse expr stmt1 stmt2 -> return ()
  While expr stmt           -> return ()
  SExp expr                 -> return ()

compileExpr :: String -> Expr -> GenM ()
compileExpr resultLoc e = case e of
  EVar (Ident ident)             -> do
    ident' <- newVar
    typ <- getCurrentExpType
    var <- lookupVar ident
    emit $ "%" ++ ident' ++ " = load " ++ makeLLVMType typ ++ "* " ++ var
    store typ ident' resultLoc
  ELitInt integer        -> return ()
  ELitDoub double        -> return ()
  ELitTrue               -> return ()
  ELitFalse              -> return ()
  EApp ident exprs       -> return ()
  EString string         -> return ()
  Neg expr               -> return ()
  Not expr               -> return ()
  EMul expr1 mulOp expr2 -> return ()
  EAdd expr1 addOp expr2 -> return ()
  ERel expr1 relOp expr2 -> return ()
  EAnd expr1 expr2       -> return ()
  EOr expr1 expr2        -> return ()
  ETyped expr typ        -> do
    setCurrentExpType typ
    compileExpr resultLoc expr

{-

(From slides)

We need to keep some state information during code generation.
This includes at least:

* Next number for generating register names (and labels).
* Definitions of global names for string literals.
* Lookup table to find LLVM name for Javalette variable name.
* Lookup table to find type of function.

-}

store :: Type -> String -> String -> GenM ()
store typ src tgt = do
  let typ' = makeLLVMType typ
  emit $ "store " ++ typ' ++ " %" ++ src ++ ", " ++ typ' ++ "* %" ++ tgt

emit :: String -> GenM ()
emit s = code %= (s:)

addVar :: String -> GenM String
addVar x = do 
  num <- use nextVariable
  nextVariable += 1
  llvmNames %= \(s:ss) -> M.insert x (show num) s : ss
  return $ show num

newVar :: GenM String
newVar = do
  num <- use nextVariable
  nextVariable += 1
  return $ show num

lookupVar :: String -> GenM String
lookupVar name = do
  scopes <- use llvmNames
  lookIn scopes
  where
    lookIn [] = fail $ "CRITICAL ERROR: Variable " ++ name ++ " not reachable"
    lookIn (scope:ss) = case M.lookup name scope of
      Nothing -> lookIn ss
      Just newName -> return newName

newLabel :: GenM String
newLabel = do
  num <- use nextLabel
  nextLabel += 1
  return $ show num

withContinueLabel :: String -> GenM a -> GenM a
withContinueLabel name fn = do
  continueTo %= (name:)
  value <- fn
  continueTo %= tail
  return value

setCurrentExpType :: Type -> GenM ()
setCurrentExpType typ = do
  currentExpType .= Just typ

getCurrentExpType :: GenM Type
getCurrentExpType = do
  expType <- use currentExpType
  case expType of
    Nothing -> fail $ "CRITICAL ERROR: currentExpType not set"
    Just x  -> return x

enterScope :: GenM ()
enterScope = llvmNames %= (M.empty:)

exitScope :: GenM ()
exitScope = llvmNames %= tail

scoped :: GenM a -> GenM ()
scoped fn = enterScope >> fn >> exitScope

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