{-# LANGUAGE TemplateHaskell #-}

module TypeChecker where

import Control.Lens (makeLenses, use, (.=), (%=))
import Control.Monad (liftM)
import Control.Monad.State (StateT, evalStateT)
import Data.Maybe (isJust)

import Grammar.ErrM (Err(Bad, Ok))
import Grammar.Abs


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
  typedTopDefs <- evalStateT (mapM typecheckTopDef topDefs) (emptySEnv{_symbols = ss})
  return $ Program typedTopDefs

buildSymbolTable :: [TopDef] -> SymbolTable
buildSymbolTable []                           = primitiveFunctions
buildSymbolTable (FnDef t ident args _ : tds) = (ident, (map typeOf args, t)) : buildSymbolTable tds
  where 
    typeOf (Arg a _) = a

primitiveFunctions :: SymbolTable
primitiveFunctions = 
    [ (Ident "printInt",    ([Int],  Void))
    , (Ident "printDouble", ([Doub], Void))
    , (Ident "printString", ([String], Void))
    , (Ident "readInt",     ([],     Int ))
    , (Ident "readDouble",  ([],     Doub)) ]

checkSymbolTable :: SymbolTable -> Err ()
checkSymbolTable symbolTable 
    | null dups = Ok ()
    | otherwise = fail $ show (head dups) ++ " is already defined"
  where
    dups = duplicates $ map fst symbolTable

typecheckTopDef :: TopDef -> CheckM TopDef
typecheckTopDef (FnDef t ident args block) = do
    fnType .= t
    fnReturned .= False

    enterScope
    mapM_ (\(Arg argtype argname) -> addVar argname argtype) args
    typedBlock <- checkBlock block
    exitScope

    -- Check explicit return
    didReturn <- use fnReturned
    assert (didReturn || t == Void) "Implicit return for non-void function"

    return (FnDef t ident args typedBlock)

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
  typedItems <- mapM (checkItem t) items
  return (Decl t typedItems)

checkStmt (Ass ident expr) = do
  declaredType <- lookupVar ident
  typedExpr@(ETyped _ inferredType) <- inferExpr expr
  assert (declaredType == inferredType) $  "Invalid assignment: " ++
                      "expected " ++ show declaredType ++ ", got " ++ show inferredType
  return (Ass ident typedExpr)

checkStmt (ArrAss ident indexExpr rightExpr) = do
  typedIndexExpr@(ETyped _ inferredIndex) <- inferExpr indexExpr
  typedRight@(ETyped _ inferredRight) <- inferExpr rightExpr

  t <- lookupVar ident
  assert (isArray t) $ "Attempt to index non-array variable " ++ show ident
  assert (inferredIndex == Int) $ "Attempt to access non-integer (" ++ 
      show inferredIndex ++ ") index of " ++ show ident
  let (Array elemT) = t in
    assert (elemT == inferredRight) $ "Invalid types in array assignment: " ++
      "got " ++ show inferredRight ++ ", expected " ++ show elemT

  return (ArrAss ident typedIndexExpr typedRight)

checkStmt s@(Incr ident) = do
  t <- lookupVar ident
  assert (t == Int) $ "Invalid increment: " ++
                      "type " ++ show t ++ " does not support `++`"
  return s

checkStmt s@(Decr ident) = do
  t <- lookupVar ident
  assert (t == Int) $ "Invalid decrement: " ++
                      "type " ++ show t ++ " does not support `--`"
  return s

checkStmt (Ret expr) = do
  declaredType <- use fnType
  typedExpr@(ETyped _ inferredType) <- inferExpr expr
  assert (declaredType == inferredType) $ 
      "Invalid return: expected " ++ show declaredType ++ 
      ", got " ++ show inferredType
  fnReturned .= True
  return (Ret typedExpr)

checkStmt VRet = do
  t <- use fnType
  assert (t == Void) $ "Invalid empty return for function of type " ++ show t
  return VRet

checkStmt (Cond expr stmt) = do
  alreadyReturned <- use fnReturned -- save current value of fnReturned
  fnReturned .= False

  typedExpr@(ETyped _ t) <- inferExpr expr
  assert (t == Bool) $ "Non-bool condition to if-statement: " ++ show t
  typedStmt <- checkStmt stmt
  didReturn <- use fnReturned

  (fnReturned .=) $ alreadyReturned || (didReturn && isLiterallyTrue typedExpr)

  return (Cond typedExpr typedStmt)

checkStmt (CondElse expr stmt1 stmt2) = do
  alreadyReturned <- use fnReturned -- save current value of fnReturned

  typedExpr@(ETyped _ t) <- inferExpr expr
  assert (t == Bool) $ "Non-bool condition to if-statement: " ++ show t

  fnReturned .= False
  stmt1' <- checkStmt stmt1
  didReturn1 <- use fnReturned

  fnReturned .= False
  stmt2' <- checkStmt stmt2
  didReturn2 <- use fnReturned

  (fnReturned .=) $ alreadyReturned ||
      (didReturn1 && didReturn2) ||
      (didReturn1 && isLiterallyTrue typedExpr) ||
      (didReturn2 && isLiterallyFalse typedExpr)

  return (CondElse typedExpr stmt1' stmt2')

checkStmt (While expr stmt) = do
  alreadyReturned <- use fnReturned -- save current value of fnReturned
  fnReturned .= False

  typedExpr@(ETyped _ t) <- inferExpr expr
  assert (t == Bool) $
      "Non-bool condition (" ++
      show t ++
      ") to while-statement: " ++
      show expr
  typedStmt <- checkStmt stmt

  didReturn <- use fnReturned
  (fnReturned .=) $ alreadyReturned || (didReturn && isLiterallyTrue typedExpr)
  
  return (While typedExpr typedStmt)

checkStmt (For t var expr stmt) = do
  typedExpr@(ETyped _ inferredType) <- inferExpr expr
  assert (isArray inferredType) "Attempt to iterate over non-array"
  let (Array inferredAType) = inferredType in
    assert (inferredAType == t) $ "Type mis-match in For-loop: " ++
      show t ++ " can't bind " ++ show inferredType

  enterScope
  addVar var t
  typedStmt <- checkStmt stmt
  exitScope

  return (For t var typedExpr typedStmt)

checkStmt (SExp expr) = do
  typedExpr <- inferExpr expr
  return $ SExp typedExpr


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


isArray :: Type -> Bool
isArray (Array _) = True
isArray _         = False

checkItem :: Type -> Item -> CheckM Item
checkItem t (NoInit ident) = do
  checkType t
  addVar ident t
  return $ NoInit ident

checkItem declaredType (Init ident expr) = do
  checkType declaredType
  typedExpr@(ETyped _ inferredType) <- inferExpr expr
  assert (declaredType == inferredType) $ 
      "Invalid variale initialization: " ++
      "expected " ++ show declaredType ++ ", got " ++ show inferredType
  addVar ident declaredType
  return $ Init ident typedExpr

checkType :: Type -> CheckM ()
checkType (Array (Array t)) = fail $ "Illegal type: " ++ show t ++ "[][]"
checkType _ = return ()

inferExpr :: Expr -> CheckM Expr

inferExpr expr@(EVar ident) = do
  t <- lookupVar ident
  return $ ETyped expr t

inferExpr expr@(ELitInt _) =
  return $ ETyped expr Int

inferExpr expr@(ELitDoub _) =
  return $ ETyped expr Doub

inferExpr ELitTrue =
  return $ ETyped ELitTrue Bool

inferExpr ELitFalse =
  return $ ETyped ELitFalse Bool

inferExpr (EApp ident argexprs) = do
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

inferExpr expr@(EString _) =
  return $ ETyped expr String

inferExpr (Neg expr) = do
  typedExpr@(ETyped _ t) <- inferExpr expr
  assert (t `elem` [Int, Doub]) $"Attempt to negate non-numeric type " ++ show t
  return $ ETyped (Neg typedExpr) t

inferExpr (Not expr) = do
  typedExpr@(ETyped _ t) <- inferExpr expr
  assert (t == Bool) $ "Attempt to negate non-bool type " ++ show t
  return $ ETyped (Not typedExpr) t

inferExpr (EMul expr1 op expr2) = do
  typedExpr1@(ETyped _ t1) <- inferExpr expr1
  typedExpr2@(ETyped _ t2) <- inferExpr expr2
  let errmsg =  "Incompatible types for operator " ++ show op ++ ": " ++
                show t1 ++ " and " ++ show t2
  assert (t1 == t2) errmsg
  assert (t1 /= Doub || op /= Mod) errmsg
  assert (t1 `elem` [Int, Doub]) errmsg
  return $ ETyped (EMul typedExpr1 op typedExpr2) t1

inferExpr (EAdd expr1 op expr2) = do
  typedExpr1@(ETyped _ t1) <- inferExpr expr1
  typedExpr2@(ETyped _ t2) <- inferExpr expr2
  let errmsg =  "Incompatible types for operator " ++ show op ++ ": " ++
                show t1 ++ " and " ++ show t2
  assert (t1 == t2) errmsg
  assert (t1 `elem` [Int, Doub]) errmsg
  return $ ETyped (EAdd typedExpr1 op typedExpr2) t1

inferExpr (ERel expr1 op expr2) = do
  typedExpr1@(ETyped _ t1) <- inferExpr expr1
  typedExpr2@(ETyped _ t2) <- inferExpr expr2
  let errmsg =  "Incompatible types for operator " ++ show op ++ ": " ++
                show t1 ++ " and " ++ show t2
  assert (t1 == t2) errmsg
  assert (t1 `elem` [Int, Doub] || (t1 == Bool && op `elem` [EQU, NE])) errmsg
  return $ ETyped (ERel typedExpr1 op typedExpr2) Bool

inferExpr (EAnd expr1 expr2) = do
  typedExpr1@(ETyped _ t1) <- inferExpr expr1
  typedExpr2@(ETyped _ t2) <- inferExpr expr2
  let errmsg =  "Incompatible types for operator AND: " ++
                show t1 ++ " and " ++ show t2
  assert (t1 == t2) errmsg
  assert (t1 == Bool) errmsg
  return $ ETyped (EAnd typedExpr1 typedExpr2) t1

inferExpr (EOr expr1 expr2) = do
  typedExpr1@(ETyped _ t1) <- inferExpr expr1
  typedExpr2@(ETyped _ t2) <- inferExpr expr2
  let errmsg =  "Incompatible types for operator OR: " ++
                show t1 ++ " and " ++ show t2
  assert (t1 == t2) errmsg
  assert (t1 == Bool) errmsg
  return $ ETyped (EOr typedExpr1 typedExpr2) t1


-- Array expressions

inferExpr (EIndex ident expr) = do
  t <- lookupVar ident
  assert (isArray t) "Attempt to index non-array"

  typedExpr@(ETyped _ inferredType) <- inferExpr expr
  assert (inferredType == Int) "Attempt to access non-integer index"

  let (Array elemType) = t
  return $ ETyped (EIndex ident typedExpr) elemType

inferExpr (ENew t expr) = do
  typedExpr@(ETyped _ inferredType) <- inferExpr expr
  assert (inferredType == Int) "Array length must be integer"

  return $ ETyped (ENew t typedExpr) (Array t)

inferExpr (ELength expr) = do
  typedExpr@(ETyped _ inferredType) <- inferExpr expr
  assert (isArray inferredType) "Attempt to get length of non-array"

  return $ ETyped (ELength typedExpr) Int

inferExpr _ = fail "CRITICAL: Attempting to infer already typed expression"

lookupVar :: Ident -> CheckM Type
lookupVar name = do
    scopes <- use env
    recursiveLookup scopes name
  where
    recursiveLookup :: ScopeList -> Ident -> CheckM Type
    recursiveLookup [] x       = fail $ "CRITICAL ERROR: No scope to find " ++ show x
    recursiveLookup (s:rest) x = case lookup x s of
                              Nothing -> recursiveLookup rest x
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
