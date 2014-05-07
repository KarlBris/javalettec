{-# LANGUAGE TemplateHaskell #-}

module TypeChecker where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
import Grammar.Abs
import Grammar.Print
import Grammar.ErrM

type CheckM a = StateT SEnv Err a

data SEnv = E {
  _fnType     :: Type,
  _fnReturned :: Bool,
  _symbols    :: SymbolTable,
  _env        :: ScopeList
} deriving Show

emptySEnv :: SEnv
emptySEnv = E {
  _fnType            = Void,
  _fnReturned        = False,
  _symbols           = [],
  _env               = []
}

type SymbolTable = [(Ident, ([Type], Type))]
type ScopeList = [[(Ident, Type)]]

makeLenses ''SEnv

typecheck :: Program -> Err Program
typecheck (Program topDefs) = do
  let ss = buildSymbolTable topDefs
  checkSymbolTable ss
  topDefs' <- evalStateT (mapM checkTopDef topDefs) (emptySEnv{_symbols = ss})
  return $ Program topDefs'

buildSymbolTable :: [TopDef] -> SymbolTable
buildSymbolTable []                           = primitiveFunctions
buildSymbolTable (FnDef t ident args _ : tds) = (ident, (map typeOf args, t)) : buildSymbolTable tds
  where 
    typeOf (Arg t ident) = t

primitiveFunctions :: SymbolTable
primitiveFunctions = [ (Ident "printInt",    ([Int],  Void))
                     , (Ident "printDouble", ([Doub], Void))
                     , (Ident "printString", ([String], Void))
                     , (Ident "readInt",     ([],     Int ))
                     , (Ident "readDouble",  ([],     Doub))]

checkSymbolTable :: SymbolTable -> Err ()
checkSymbolTable symbols | null dups = Ok ()
                         | otherwise = fail $ show (head dups) ++ " is already defined"
                      where
                        dups = duplicates $ map fst symbols

checkTopDef :: TopDef -> CheckM TopDef
checkTopDef (FnDef t ident args block) = do
    fnType .= t
    fnReturned .= False

    enterScope
    mapM_ (\(Arg t i) -> addVar i t) args
    block' <- checkBlock block
    exitScope

    -- Check explicit return
    didReturn <- use fnReturned
    assert (didReturn || t == Void) "Implicit return for non-void function"

    return (FnDef t ident args block')

checkBlock :: Block -> CheckM Block
checkBlock (Block stms) = liftM Block (mapM checkStmt stms)

checkStmt :: Stmt -> CheckM Stmt
checkStmt Empty = return Empty

checkStmt (BStmt block) = do
  enterScope
  typedBlock <- checkBlock block
  exitScope
  return (BStmt typedBlock)

checkStmt (Decl t items) = do
  items' <- mapM (checkItem t) items
  return (Decl t items')

checkStmt (Ass ident expr) = do
  t <- lookupVar ident
  expr'@(ETyped _ t') <- inferExpr expr
  assert (t == t') $  "Invalid assignment: " ++
                      "expected " ++ show t ++ ", got " ++ show t'
  return (Ass ident expr')

checkStmt s@(Incr ident) = do
  t <- lookupVar ident
  assert (t `elem` [Int, Doub]) $ "Invalid increment: " ++
                                  "type " ++ show t ++ " does not support `++`"
  return s

checkStmt s@(Decr ident) = do
  t <- lookupVar ident
  assert (t `elem` [Int, Doub]) $ "Invalid decrement: " ++
                                  "type " ++ show t ++ " does not support `--`"
  return s

checkStmt (Ret expr) = do
  t <- use fnType
  expr'@(ETyped _ t') <- inferExpr expr
  assert (t == t') $ "Invalid return: expected " ++ show t ++ ", got " ++ show t'
  fnReturned .= True
  return (Ret expr')

checkStmt VRet = do
  t <- use fnType
  assert (t == Void) $ "Invalid empty return for function of type " ++ show t
  return VRet

checkStmt (Cond expr stmt) = do
  didReturn <- use fnReturned -- save current value of fnReturned
  fnReturned .= False

  expr'@(ETyped _ t) <- inferExpr expr
  assert (t == Bool) $ "Non-bool condition to if-statement: " ++ show t
  stmt' <- checkStmt stmt
  didReturn' <- use fnReturned

  (fnReturned .=) $ didReturn || (didReturn' && isLiterallyTrue expr')

  return (Cond expr' stmt')

checkStmt (CondElse expr stmt1 stmt2) = do
  didReturn <- use fnReturned -- save current value of fnReturned

  expr'@(ETyped _ t) <- inferExpr expr
  assert (t == Bool) $ "Non-bool condition to if-statement: " ++ show t

  fnReturned .= False
  stmt1' <- checkStmt stmt1
  didReturn1 <- use fnReturned

  fnReturned .= False
  stmt2' <- checkStmt stmt2
  didReturn2 <- use fnReturned

  (fnReturned .=) $ didReturn ||
      (didReturn1 && didReturn2) ||
      (didReturn1 && isLiterallyTrue expr') ||
      (didReturn2 && isLiterallyFalse expr')

  return (CondElse expr' stmt1' stmt2')

checkStmt (While expr stmt) = do
  didReturn <- use fnReturned -- save current value of fnReturned
  fnReturned .= False

  typedExpr@(ETyped _ t) <- inferExpr expr
  assert (t == Bool) $
      "Non-bool condition (" ++
      show t ++
      ") to while-statement: " ++
      show expr
  stmt' <- checkStmt stmt

  didReturn' <- use fnReturned
  (fnReturned .=) $ didReturn || (didReturn' && isLiterallyTrue typedExpr)
  
  return (While typedExpr stmt')

checkStmt (SExp expr) = do
  expr' <- inferExpr expr
  return $ SExp expr'


isLiterallyTrue :: Expr -> Bool
isLiterallyTrue expr = case getLiteralBool expr of
  (Bad _) -> False
  (Ok v)  -> v

isLiterallyFalse :: Expr -> Bool
isLiterallyFalse expr = case getLiteralBool expr of
  (Bad _) -> False
  (Ok v)  -> not v

getLiteralBool :: Expr -> Err Bool
getLiteralBool (ETyped expr t) | t /= Bool = fail "Not boolean"
                               | otherwise = getLiteralBool expr
getLiteralBool (ELitTrue)  = return True
getLiteralBool (ELitFalse) = return False
getLiteralBool _ = fail "Non-literal expression"


checkItem :: Type -> Item -> CheckM Item
checkItem t (NoInit ident) = do
  addVar ident t
  return $ NoInit ident
checkItem t (Init ident expr) = do
  expr'@(ETyped _ t') <- inferExpr expr
  assert (t == t') $  "Invalid variale initialization: " ++
                      "expected " ++ show t ++ ", got " ++ show t'
  addVar ident t
  return $ Init ident expr'


inferExpr :: Expr -> CheckM Expr

inferExpr expr@(EVar ident) = do
  t <- lookupVar ident
  return $ ETyped expr t

inferExpr expr@(ELitInt val) =
  return $ ETyped expr Int

inferExpr expr@(ELitDoub val) =
  return $ ETyped expr Doub

inferExpr ELitTrue =
  return $ ETyped ELitTrue Bool

inferExpr ELitFalse =
  return $ ETyped ELitFalse Bool

inferExpr expr@(EApp ident argexprs) = do
    ss <- use symbols
    typedargexprs <- mapM inferExpr argexprs

    ret <- case lookup ident ss of
      Nothing -> fail $ "Attempt to call undeclared function " ++ show ident
      Just (argtypes, rettype) -> do
        assert (length argtypes == length argexprs) $
            "Incorrect number of arguments (" ++
            show (length argexprs) ++
            ") for function call to " ++
            show ident
        mapM_ checkEqualTypes (zip argtypes typedargexprs)
        return rettype

    return $ ETyped (EApp ident typedargexprs) ret
  where
    checkEqualTypes :: (Type, Expr) -> CheckM ()
    checkEqualTypes (t1, ETyped _ t2) =
      assert (t1 == t2) $ "Incorrect argument type for function call to: " ++
        show ident ++ "; got: " ++ show t2 ++ ", expected: " ++ show t1
    checkEqualTypes (_, e) = fail $ "Internal compiler error: " ++ show e ++
      " was incorrectly type-inferred"

inferExpr expr@(EString strval) =
  return $ ETyped expr String

inferExpr (Neg expr) = do
  nexpr@(ETyped _ t) <- inferExpr expr
  assert (t `elem` [Int, Doub]) $"Attempt to negate non-numeric type " ++ show t
  return $ ETyped (Neg nexpr) t

inferExpr (Not expr) = do
  nexpr@(ETyped _ t) <- inferExpr expr
  assert (t == Bool) $ "Attempt to negate non-bool type " ++ show t
  return $ ETyped (Not nexpr) t

inferExpr (EMul expr1 op expr2) = do
  nexpr1@(ETyped _ t1) <- inferExpr expr1
  nexpr2@(ETyped _ t2) <- inferExpr expr2
  let errmsg =  "Incompatible types for operator " ++ show op ++ ": " ++
                show t1 ++ " and " ++ show t2
  assert (t1 == t2) errmsg
  assert (t1 /= Doub || op /= Mod) errmsg
  assert (t1 `elem` [Int, Doub]) errmsg
  return $ ETyped (EMul nexpr1 op nexpr2) t1

inferExpr (EAdd expr1 op expr2) = do
  nexpr1@(ETyped _ t1) <- inferExpr expr1
  nexpr2@(ETyped _ t2) <- inferExpr expr2
  let errmsg =  "Incompatible types for operator " ++ show op ++ ": " ++
                show t1 ++ " and " ++ show t2
  assert (t1 == t2) errmsg
  assert (t1 `elem` [Int, Doub]) errmsg
  return $ ETyped (EAdd nexpr1 op nexpr2) t1

inferExpr (ERel expr1 op expr2) = do
  nexpr1@(ETyped _ t1) <- inferExpr expr1
  nexpr2@(ETyped _ t2) <- inferExpr expr2
  let errmsg =  "Incompatible types for operator " ++ show op ++ ": " ++
                show t1 ++ " and " ++ show t2
  assert (t1 == t2) errmsg
  assert (t1 `elem` [Int, Doub] || (t1 == Bool && op == EQU)) errmsg
  return $ ETyped (ERel nexpr1 op nexpr2) Bool

inferExpr (EAnd expr1 expr2) = do
  nexpr1@(ETyped _ t1) <- inferExpr expr1
  nexpr2@(ETyped _ t2) <- inferExpr expr2
  let errmsg =  "Incompatible types for operator AND: " ++
                show t1 ++ " and " ++ show t2
  assert (t1 == t2) errmsg
  assert (t1 == Bool) errmsg
  return $ ETyped (EAnd nexpr1 nexpr2) t1

inferExpr (EOr expr1 expr2) = do
  nexpr1@(ETyped _ t1) <- inferExpr expr1
  nexpr2@(ETyped _ t2) <- inferExpr expr2
  let errmsg =  "Incompatible types for operator OR: " ++
                show t1 ++ " and " ++ show t2
  assert (t1 == t2) errmsg
  assert (t1 == Bool) errmsg
  return $ ETyped (EOr nexpr1 nexpr2) t1

inferExpr expr@(ETyped _ _) = return expr




lookupVar :: Ident -> CheckM Type
lookupVar x = do
    scopes <- use env
    lookupVar' scopes x
  where
    lookupVar' :: ScopeList -> Ident -> CheckM Type
    lookupVar' [] x       = fail $ "CRITICAL ERROR: No scope to find " ++ show x
    lookupVar' (s:rest) x = case lookup x s of
                              Nothing -> lookupVar' rest x
                              Just t  -> return t


addVar :: Ident -> Type -> CheckM ()
addVar var t = do
    ss <- use env
    ss' <- addVar' ss
    env .= ss'
  where
    addVar' []     = fail $ "CRITICAL: Tried to add variable " ++
                            show var ++
                            " to environment without scopes"
    addVar' (x:xs) | isJust (lookup var x) = fail $
                                              "Redeclaration of variable " ++
                                              show var
                   | otherwise = return $ ((var, t):x):xs

enterScope :: CheckM ()
enterScope = env %= ([]:)

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