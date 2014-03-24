{-# LANGUAGE TemplateHaskell #-}

module TypeChecker where

import Control.Lens
import Control.Monad.Trans.State.Lazy
import Data.List
import Data.Maybe
import Grammar.Abs
import Grammar.Print
import Grammar.ErrM

-- type for mr. dubs checkum
type CheckM a = StateT SEnv Err a

data SEnv = E {
  _currentReturnType :: Type,
  _symbols           :: SymbolTable,
  _env               :: ScopeList
}

emptySEnv :: SEnv
emptySEnv = E {
	_currentReturnType = undefined,
	_symbols           = [],
	_env               = []
}

type SymbolTable = [(Ident, [Type], Type)]
type ScopeList = [[(Ident, Type)]]

makeLenses ''SEnv

typecheck :: Program -> Err Program
typecheck (Program topDefs) = undefined -- (a, s) <- runStateT asd

buildSymbolTable :: [TopDef] -> SymbolTable
buildSymbolTable []                           = primitiveFunctions
buildSymbolTable ((FnDef t ident args _):tds) = (ident, map typeOf args, t):(buildSymbolTable tds)
  where 
  	typeOf (Arg t ident) = t

primitiveFunctions :: SymbolTable
primitiveFunctions = [ (Ident "printInt",    [Int],  Void)
                     , (Ident "printDouble", [Doub], Void)
                     , (Ident "printString", [],     Void) --fix string
                     , (Ident "readInt",     [],     Int )
                     , (Ident "readDouble",  [],     Doub)]

checkSymbolTable :: SymbolTable -> Err ()
checkSymbolTable symbols | length dups == 0 = Ok ()
                         | otherwise = fail $ (show $ head dups) ++ " is already defined"
                      where
                        dups = duplicates $ map (\(ident,_,_) -> ident) symbols

checkTopDef :: TopDef -> CheckM TopDef
checkTopDef (FnDef t ident args block) = do
    currentReturnType .= t
    enterScope $ map (\(Arg t i) -> (i, t)) args
    block' <- checkBlock block
    exitScope
    return (FnDef t ident args block')

checkArg :: Arg -> CheckM Arg
checkArg a = undefined

checkBlock :: Block -> CheckM Block
checkBlock (Block stms) = mapM checkStmt stms >>= return.Block

checkStmt :: Stmt -> CheckM Stmt
checkStmt Empty = return Empty

checkStmt (BStmt block) = do
  enterScope []
  block' <- checkBlock block
  exitScope
  return (BStmt block)

checkStmt (Decl t items) = do
  items' <- mapM (\i -> checkItem t i) items
  return (Decl t items')

checkStmt (Ass ident expr) = do
  t <- lookupVar ident
  expr'@(ETyped _ t') <- inferExpr expr
  assert (t == t') $ "Invalid assignment: Expected " ++ (show t) ++ ", got " ++ (show t')
  return (Ass ident expr')

checkStmt s@(Incr ident) = do
  t <- lookupVar ident
  assert (t `elem` [Int, Doub]) $ "Invalid increment: type " ++ (show t) ++ " does not support `++`"
  return s

checkStmt s@(Decr ident) = do
  t <- lookupVar ident
  assert (t `elem` [Int, Doub]) $ "Invalid decrement: type " ++ (show t) ++ " does not support `--`"
  return s

checkStmt (Ret expr) = do
  t <- use currentReturnType
  expr'@(ETyped _ t') <- inferExpr expr
  assert (t == t') $ "Invalid return: Expected " ++ (show t) ++ ", got " ++ (show t')
  return (Ret expr')
  
--checkStmt VRet
--checkStmt Cond Expr Stmt
--checkStmt CondElse Expr Stmt Stmt
--checkStmt While Expr Stmt
--checkStmt SExp Expr

checkItem :: Type -> Item -> CheckM Item
checkItem i = undefined

inferExpr :: Expr -> CheckM Expr
inferExpr e = undefined




lookupVar :: Ident -> CheckM Type
lookupVar x = do
    scopes <- use env
    lookupVar' scopes x
  where
  	lookupVar' :: ScopeList -> Ident -> CheckM Type
  	lookupVar' [] x           = fail $ "Undefined variable " ++ (show x) 
  	lookupVar' (scope:rest) x = case lookup x scope of
                                  Nothing -> lookupVar' rest x
                                  Just t  -> return t


addVar :: Ident -> Type -> CheckM ()
addVar var t = env %= addVar' where
  addVar' []     = fail $ "Something went wrong! Tried to add variable " ++ (show var) ++ " to environment without scopes"
  addVar' (x:xs) | isJust (lookup var x) = fail $ "Redeclaration of variable " ++ (show var)
                 | otherwise = ((var, t):x):xs

enterScope :: [(Ident, Type)] -> CheckM ()
enterScope scope = env %= (scope:)

exitScope :: CheckM ()
exitScope = env %= exitScope'
  where
  	exitScope' :: ScopeList -> ScopeList
  	exitScope' [] = fail "Something went wrong! Tried to exit a block illegally"
  	exitScope' xs = tail xs

assert :: Bool -> String -> CheckM ()
assert True _    = return ()
assert False msg = fail msg


duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (x:xs) = filter (`elem` xs) [x] ++ duplicates xs