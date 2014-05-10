{-# LANGUAGE TemplateHaskell #-}

module CodeGenerator where

import qualified Data.Map.Lazy as M
import Control.Lens
import Control.Monad.State
import Data.List

import Grammar.Abs
import Grammar.ErrM
--import Grammar.Print

type GenM a = StateT Env Err a

data Env = Env {
  _code           :: [String],
  _currentExpType :: Maybe Type,

  _nextVariable   :: Int,
  _nextLabel      :: Int,
  _nextString     :: Int,

  _stringLiterals :: [String],
  _llvmNames      :: [M.Map String String],
  _contStack      :: [String],
  _lastStmtStack  :: [Bool]
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
  _contStack = [],
  _lastStmtStack = []
}

makeLenses ''Env

compilellvm :: Program -> Err String
compilellvm p = do
  st <- execStateT (compileProgram p) emptyEnv
  return $ unlines . reverse . (^. code) $ st 

compileProgram :: Program -> GenM ()
compileProgram (Program topDefs) = do
  mapM_ emitLabel [
    "",
    "declare void @printInt(i32)",
    "declare void @printDouble(double)",
    "declare void @printString(i8*)",
    "declare i32 @readInt()",
    "declare double @readDouble()"
    ]
  compileTopDefs topDefs
  emitStringLiterals

compileTopDefs :: [TopDef] -> GenM ()
compileTopDefs [] = return ()
compileTopDefs (FnDef typ (Ident name) args block : rest) = do
  emit ""

  scoped $ do
    llvmArgs <- convertArgs
    emitLabel $ "define " ++ makeLLVMType typ ++ " @" ++ name ++ "(" ++ llvmArgs ++ ") {"

    lbl <- newLabel
    emitLabel $ "entry:"
    emitConvertedArgs
    emit $ "br label %" ++ lbl
    emitLabel $ lbl ++ ":"

    compileBlock block

    if typ == Void
      then emit "ret void" -- Catch that implicit return
      else return ()

    emitLabel "}"

  compileTopDefs rest
  where
    convertArgs :: GenM String
    convertArgs = makeLLVMArgs args >>= \as -> return (intercalate ", " as)

    makeLLVMArgs :: [Arg] -> GenM [String]
    makeLLVMArgs (Arg t (Ident n) : xs) = do
      newVar <- (addVar n)
      rest <- makeLLVMArgs xs
      return $ (makeLLVMType t ++ " %" ++ newVar) : rest
    makeLLVMArgs [] = return []

    emitConvertedArgs :: GenM ()
    emitConvertedArgs = do
      forM_ args $ \(Arg t (Ident n)) -> do
        newReg <- newVar
        emit $ "%" ++ newReg ++ " = alloca " ++ makeLLVMType t
        oldReg <- lookupVar n
        store t oldReg newReg
        setVar n newReg

makeLLVMType :: Type -> String
makeLLVMType Int    = "i32"
makeLLVMType Doub   = "double"
makeLLVMType Bool   = "i1"
makeLLVMType Void   = "void"
makeLLVMType String = "i8*"

compileBlock :: Block -> GenM ()
compileBlock (Block stmts) = do
  scoped $ forM_ stmts $ \stmt -> if stmt == (last stmts)
    then withLastStmt stmt
    else withInitStmt stmt

compileStmt :: Stmt -> GenM ()
compileStmt s = (emit $ "; " ++ show s) >> case s of
  Empty                     -> return ()
  BStmt block               -> do
    compileBlock block
  Decl typ ((NoInit (Ident ident)):rest)    -> do
    targetReg <- addVar ident
    let llvmType = makeLLVMType typ
    emit $ "%" ++ targetReg ++ " = alloca " ++ llvmType
    emit $ "store " ++ llvmType ++ " " ++ initValue typ ++ ", " ++ llvmType ++ "* %" ++ targetReg
    compileStmt (Decl typ rest)
  Decl typ ((Init (Ident ident) expr):rest) -> do
    let typ' = makeLLVMType typ
    ident'' <- newVar
    compileExpr ident'' expr
    ident' <- addVar ident
    emit $ "%" ++ ident' ++ " = alloca " ++ typ'
    store typ ident'' ident'
    compileStmt (Decl typ rest)
  Decl _ []                         -> do
    return ()
  Ass (Ident ident) expr@(ETyped _ t)            -> do
    exprReg <- newVar
    compileExpr exprReg expr
    retReg <- lookupVar ident
    store t exprReg retReg
  Incr (Ident ident)                -> do
    resultReg <- newVar
    varReg <- newVar
    adrReg <- lookupVar ident
    emit $ "%" ++ varReg ++ " = load i32* %" ++ adrReg
    emit $ "%" ++ resultReg ++ " = add i32 %" ++ varReg ++ ", 1"
    store Int resultReg adrReg
  Decr (Ident ident)               -> do
    resultReg <- newVar
    varReg <- newVar
    adrReg <- lookupVar ident
    emit $ "%" ++ varReg ++ " = load i32* %" ++ adrReg
    emit $ "%" ++ resultReg ++ " = sub i32 %" ++ varReg ++ ", 1"
    store Int resultReg adrReg
  Ret e@(ETyped _ typ)    -> do
    ident <- newVar
    compileExpr ident e
    emit $ "ret " ++ makeLLVMType typ ++ " %" ++ ident
  Ret (ELitInt x) -> do
    emit $ "ret " ++ makeLLVMType Int ++ " " ++ show x
  Ret (ELitDoub x) -> do
    emit $ "ret " ++ makeLLVMType Doub ++ " " ++ show x
  Ret ELitTrue -> do
    emit $ "ret " ++ makeLLVMType Bool ++ " 1"
  Ret ELitFalse -> do
    emit $ "ret " ++ makeLLVMType Bool ++ " 0"
  VRet -> do
    emit "ret void"
  Cond expr stmt -> do
    condLbl <- newLabel
    nextLbl <- newLabel
    condReg <- newVar

    compileExpr condReg expr
    emit $ "br i1 %" ++ condReg ++ ", label %" ++ condLbl ++ ", label %" ++ nextLbl
    emitLabel $ condLbl ++ ":"
    scoped $ compileStmt stmt
    emit $ "br label %" ++ nextLbl

    emitLabel $ nextLbl ++ ":"
    gameOver <- atEnd
    if gameOver
      then emit "unreachable"
      else return ()
  CondElse expr stmt1 stmt2 -> do
    lblIf <- newLabel
    lblElse <- newLabel
    nextLbl <- newLabel
    condReg <- newVar

    compileExpr condReg expr
    emit $ "br i1 %" ++ condReg ++ ", label %" ++ lblIf ++ ", label %" ++ lblElse
    emitLabel $ lblIf ++ ":"
    scoped $ compileStmt stmt1
    emit $ "br label %" ++ nextLbl

    emitLabel $ lblElse ++ ":"
    scoped $ compileStmt stmt2
    emit $ "br label %" ++ nextLbl

    emitLabel $ nextLbl ++ ":"
    gameOver <- atEnd
    if gameOver
      then emit "unreachable"
      else return ()

  While expr stmt -> do
    lblBegin <- newLabel
    lblLoop <- newLabel
    lblNext <- newLabel
    condReg <- newVar

    emit $ "br label %" ++ lblBegin
    emitLabel $ lblBegin ++ ":"
    enterScope
    compileExpr condReg expr
    emit $ "br i1 %" ++ condReg ++ ", label %" ++ lblLoop ++ ", label %" ++ lblNext
    emitLabel $ lblLoop ++ ":"
    scoped $ compileStmt stmt
    emit $ "br label %" ++ lblBegin
    exitScope

    emitLabel $ lblNext ++ ":"
    gameOver <- atEnd
    if gameOver
      then emit "unreachable"
      else return ()

  SExp expr -> do
    case expr of
      ETyped app@(EApp _ _) t -> do
        resReg <- newVar
        setCurrentExpType t
        compileExpr resReg app
      _ -> return ()

  stmt -> fail $ "CRITICAL ERROR: Unexpected stmt " ++ show stmt

compileExpr :: String -> Expr -> GenM ()
compileExpr resultReg e = case e of
  EVar (Ident ident)             -> do
    typ <- getCurrentExpType
    var <- lookupVar ident
    emit $ "%" ++ resultReg ++ " = load " ++ makeLLVMType typ ++ "* %" ++ var
  ELitInt integer        -> do
    tempReg <- newVar
    emit $ "%" ++ tempReg ++ " = alloca i32"
    emit $ "store i32 " ++ show integer ++ ", i32* %" ++ tempReg
    emit $ "%" ++ resultReg ++ " = load i32* %" ++ tempReg
  ELitDoub double        -> do
    tempReg <- newVar
    emit $ "%" ++ tempReg ++ " = alloca double"
    emit $ "store double " ++ show double ++ ", double* %" ++ tempReg
    emit $ "%" ++ resultReg ++ " = load double* %" ++ tempReg
  ELitTrue               -> do
    tempReg <- newVar
    emit $ "%" ++ tempReg ++ " = alloca i1"
    emit $ "store i1 true, i1* %" ++ tempReg
    emit $ "%" ++ resultReg ++ " = load i1* %" ++ tempReg
  ELitFalse              -> do
    tempReg <- newVar
    emit $ "%" ++ tempReg ++ " = alloca i1"
    emit $ "store i1 false, i1* %" ++ tempReg
    emit $ "%" ++ resultReg ++ " = load i1* %" ++ tempReg
  EApp (Ident ident) exprs       -> do
    typ <- getCurrentExpType
    let l = length exprs
    regs <- sequence (replicate l newVar)
    let exprPairs = zip regs exprs
    mapM_ (uncurry compileExpr) exprPairs
    let typePairs = zip regs (map (\(ETyped _ t) -> makeLLVMType t) exprs)
    let argumentList = intercalate ", " (map (\(reg, typ) -> typ ++ " %" ++ reg) typePairs)
    let prefix = case typ of Void -> ""
                             _    -> "%" ++ resultReg ++ " = "
    emit $ prefix ++ "call " ++ makeLLVMType typ ++ " @" ++ ident ++ "(" ++ argumentList ++ ")"
  EString string         -> do --emit "; String literal goes here" --addStringLiterals and such
    stringVar <- newVar
    length <- addStringLiteral stringVar string
    emit $ "%" ++ resultReg ++ " = bitcast [" ++ show length ++ " x i8]* @" ++ stringVar ++ " to i8*"
  Neg expr               -> do
    typ <- getCurrentExpType
    let op = case typ of Int  -> "sub"
                         Doub -> "fsub"
    let zero = case typ of Int  -> "0"
                           Doub -> "0.0"
    ident <- newVar
    compileExpr ident expr
    emit $ "%" ++ resultReg ++ " = " ++ op ++ " " ++ makeLLVMType typ ++ " " ++ zero ++ ", %" ++ ident
  Not expr               -> do
    ident <- newVar
    compileExpr ident expr
    emit $ "%" ++ resultReg ++ " = xor i1 %" ++ ident ++ ", true"
  EMul expr1 mulOp expr2 -> do
    typ <- getCurrentExpType
    ident1 <- newVar
    ident2 <- newVar
    compileExpr ident1 expr1
    compileExpr ident2 expr2
    emit $ "%" ++ resultReg ++ " = " ++ getMulOp typ mulOp ++ " " ++ makeLLVMType typ ++ " %" ++ ident1 ++ ", %" ++ ident2
  EAdd expr1 addOp expr2 -> do
    typ <- getCurrentExpType
    ident1 <- newVar
    ident2 <- newVar
    compileExpr ident1 expr1
    compileExpr ident2 expr2
    emit $ "%" ++ resultReg ++ " = " ++ getAddOp typ addOp ++ " " ++ makeLLVMType typ ++ " %" ++ ident1 ++ ", %" ++ ident2
  ERel expr1@(ETyped _ typ) relOp expr2 -> do
    ident1 <- newVar
    ident2 <- newVar
    compileExpr ident1 expr1
    compileExpr ident2 expr2
    emit $ "%" ++ resultReg ++ " = " ++ getRelOp typ relOp ++ " " ++ makeLLVMType typ ++ " %" ++ ident1 ++ ", %" ++ ident2
  EAnd expr1 expr2       -> do
    ident1 <- newVar
    ident2 <- newVar

    cmpMem1 <- newVar
    cmpMem2 <- newVar
    cmpReg1 <- newVar
    cmpReg2 <- newVar

    lblEval <- newLabel
    lblEnd <- newLabel

    emit $ "%" ++ cmpMem1 ++ " = alloca i1"
    emit $ "%" ++ cmpMem2 ++ " = alloca i1"
    emit $ "store i1 false, i1* %" ++ cmpMem2

    compileExpr ident1 expr1
    store Bool ident1 cmpMem1
    emit $ "br i1 %" ++ ident1 ++ ", label %" ++ lblEval ++ ", label %" ++ lblEnd

    emitLabel $ lblEval ++ ":"
    compileExpr ident2 expr2
    store Bool ident2 cmpMem2
    emit $ "br label %" ++ lblEnd

    emitLabel $ lblEnd ++ ":"
    emit $ "%" ++ cmpReg1 ++ " = load i1* %" ++ cmpMem1
    emit $ "%" ++ cmpReg2 ++ " = load i1* %" ++ cmpMem2
    emit $ "%" ++ resultReg ++ " = and i1 %" ++ cmpReg1 ++ ", %" ++ cmpReg2 

  EOr expr1 expr2        -> do
    ident1 <- newVar
    ident2 <- newVar

    cmpMem1 <- newVar
    cmpMem2 <- newVar
    cmpReg1 <- newVar
    cmpReg2 <- newVar

    lblEval <- newLabel
    lblEnd <- newLabel

    emit $ "%" ++ cmpMem1 ++ " = alloca i1"
    emit $ "%" ++ cmpMem2 ++ " = alloca i1"
    emit $ "store i1 false, i1* %" ++ cmpMem2

    compileExpr ident1 expr1
    store Bool ident1 cmpMem1
    emit $ "br i1 %" ++ ident1 ++ ", label %" ++ lblEnd ++ ", label %" ++ lblEval

    emitLabel $ lblEval ++ ":"
    compileExpr ident2 expr2
    store Bool ident2 cmpMem2
    emit $ "br label %" ++ lblEnd

    emitLabel $ lblEnd ++ ":"
    emit $ "%" ++ cmpReg1 ++ " = load i1* %" ++ cmpMem1
    emit $ "%" ++ cmpReg2 ++ " = load i1* %" ++ cmpMem2
    emit $ "%" ++ resultReg ++ " = or i1 %" ++ cmpReg1 ++ ", %" ++ cmpReg2 

  ETyped expr typ        -> do
    setCurrentExpType typ
    compileExpr resultReg expr
  e -> fail $ "copmpileExpr pattern match failed on " ++ show e

getMulOp :: Type -> MulOp -> String
getMulOp Int Times  = "mul"
getMulOp Int Div    = "sdiv"
getMulOp Int Mod    = "srem"
getMulOp Doub Times = "fmul"
getMulOp Doub Div   = "fdiv"
getMulOp Doub Mod   = "rem"

getAddOp :: Type -> AddOp -> String
getAddOp Int Plus   = "add"
getAddOp Int Minus  = "sub"
getAddOp Doub Plus  = "fadd"
getAddOp Doub Minus = "fsub"

getRelOp :: Type -> RelOp -> String
getRelOp Int LTH  = "icmp slt" 
getRelOp Int LE   = "icmp sle"
getRelOp Int GTH  = "icmp sgt"
getRelOp Int GE   = "icmp sge"
getRelOp Int EQU  = "icmp eq"
getRelOp Int NE   = "icmp ne"
getRelOp Doub LTH = "fcmp ult" 
getRelOp Doub LE  = "fcmp ule"
getRelOp Doub GTH = "fcmp ugt"
getRelOp Doub GE  = "fcmp uge"
getRelOp Doub EQU = "fcmp ueq"
getRelOp Doub NE  = "fcmp une"
getRelOp Bool EQU = "icmp eq"
getRelOp Bool NE  = "icmp ne"

initValue :: Type -> String
initValue Doub = "0.0"
initValue _ = "0"

store :: Type -> String -> String -> GenM ()
store typ src tgt = do
  let typ' = makeLLVMType typ
  emit $ "store " ++ typ' ++ " %" ++ src ++ ", " ++ typ' ++ "* %" ++ tgt

emit :: String -> GenM ()
emit s = emitLabel $ "  " ++ s

emitLabel :: String -> GenM ()
emitLabel s = code %= (s:)

emitStringLiterals :: GenM ()
emitStringLiterals = do
  literals <- use stringLiterals
  code %= (++ literals)

addVar :: String -> GenM String
addVar x = do 
  var <- newVar
  llvmNames %= \(s:ss) -> M.insert x var s : ss
  return var

newVar :: GenM String
newVar = do
  num <- use nextVariable
  nextVariable += 1
  return $ "var" ++ show num

setVar :: String -> String -> GenM ()
setVar ident reg = do
  llvmNames %= \(s:ss) -> M.insert ident reg s : ss

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
  return $ "lbl" ++ show num

withContinueLabel :: String -> GenM a -> GenM a
withContinueLabel name fn = do
  contStack %= (name:)
  value <- fn
  contStack %= tail
  return value

continueTo :: GenM String
continueTo = do
  stack <- use contStack
  case stack of
    (x:_) -> return x
    []    -> fail "CRITICAL ERROR: continueTo with empty continue stack"

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

scoped :: GenM a -> GenM a
scoped fn = do
  enterScope
  value <- fn
  exitScope
  return value

withLastStmt :: Stmt -> GenM ()
withLastStmt stmt = do
  lastStmtStack %= (True:)
  value <- compileStmt stmt
  lastStmtStack %= tail
  return value

withInitStmt :: Stmt -> GenM ()
withInitStmt stmt = do
  lastStmtStack %= (False:)
  value <- compileStmt stmt
  lastStmtStack %= tail
  return value

atEnd :: GenM Bool
atEnd = do
  st <- use lastStmtStack
  end <- emptyContStack
  case st of
    [] -> fail "CRITICAL ERROR: Empty lastStmtStack"
    (x:_) -> return (x && end)

emptyContStack :: GenM Bool
emptyContStack = do
  st <- use contStack
  case st of
    [] -> return True
    _  -> return False


addStringLiteral :: String -> String -> GenM Int
addStringLiteral stringVar string = do
  let escapedString = string ++ "\\00"
  let newLength = length string + 1
  let globalString = "@" ++ stringVar ++ " = internal constant [" ++ show newLength ++ " x i8] c\"" ++ escapedString ++ "\""
  stringLiterals %= (globalString:)
  return newLength