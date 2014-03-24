{-# LANGUAGE TemplateHaskell #-}

module TypeChecker where

import Control.Lens
import Control.Monad.Trans.State.Lazy
import Data.List
import Grammar.Abs
import Grammar.Print
import Grammar.ErrM

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
buildSymbolTable []                           = []
buildSymbolTable ((FnDef t ident args _):tds) = (ident, map typeOf args, t):(buildSymbolTable tds)
  where 
  	typeOf (Arg t ident) = t

checkSymbolTable :: SymbolTable -> Err ()
checkSymbolTable symbols | length dups == 0 = Ok ()
                         | otherwise = fail $ (show $ head dups) ++ " is already defined"
                      where
                        dups = duplicates $ map (\(ident,_,_) -> ident) symbols

checkTopDef :: TopDef -> CheckM TopDef
checkTopDef td = undefined

checkArg :: Arg -> CheckM Arg
checkArg a = undefined

checkBlock :: Block -> CheckM Block
checkBlock b = undefined

checkStmt :: Stmt -> CheckM Stmt
checkStmt s = undefined

checkItem :: Item -> CheckM Item
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
  addVar' []     = fail "Something went wrong! Tried to add variable to environment without scopes"
  addVar' (x:xs) = ((var, t):x):xs

enterScope :: CheckM ()
enterScope = env %= ([]:)

exitScope :: CheckM ()
exitScope = env %= exitScope'
  where
  	exitScope' :: ScopeList -> ScopeList
  	exitScope' [] = fail "Something went wrong! Tried to exit a block illegally"
  	exitScope' xs = tail xs



duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (x:xs) = filter (`elem` xs) [x] ++ duplicates xs