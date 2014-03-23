module TypeChecker where

import Control.Monad.Trans.State.Lazy
import Data.List
import Grammar.Abs
import Grammar.Print
import Grammar.ErrM

type CheckM a = StateT SEnv Err a

data SEnv = E {
  currentReturnType :: Type,
  symbols           :: SymbolTable,
  env               :: Env
}

emptySEnv :: SEnv
emptySEnv = E {
	currentReturnType = undefined,
	symbols           = [],
	env               = []
}

type SymbolTable = [(Ident, [Type], Type)]
type Env = [[(Ident, Type)]]

typecheck :: Program -> Program
typecheck (Program topDefs) = undefined -- (a, s) <- runStateT asd

buildSymbolTable :: [TopDef] -> CheckM () 
buildSymbolTable []       = undefined
buildSymbolTable (td:tds) = undefined

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
lookupVar x = do sEnv <- get
                 let e = env sEnv
                 lookupVar' e x
  where
  	lookupVar' :: Env -> Ident -> CheckM Type
  	lookupVar' [] x           = fail $ "Undefined variable " ++ (show x) 
  	lookupVar' (scope:rest) x = case lookup x scope of
                                  Nothing -> lookupVar' rest x
                                  Just t  -> return t


addVar :: Ident -> Type -> CheckM ()
addVar x t = modify (\sEnv -> sEnv {env = 
  case env sEnv of 
  	e:rest -> ((x, t):e):rest
  	[]     -> fail "Something went wrong! Tried to add variable to environment without scopes"
  })

enterScope :: CheckM ()
enterScope = modify (\sEnv -> sEnv {env = [] : (env sEnv)})

exitScope :: CheckM ()
exitScope = modify (\sEnv -> sEnv {env = exitScope' (env sEnv)})
  where
  	exitScope' :: Env -> Env
  	exitScope' []      = fail "Something went wrong! Tried to exit a block illegally"
  	exitScope' (_:env) = env  	