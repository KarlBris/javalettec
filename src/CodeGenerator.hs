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
    args' <- llvmArgs
    emitLabel $ "define " ++ makeLLVMType typ ++ " @" ++ name ++ "(" ++ args' ++ ") {"

    lbl <- newLabel
    emitLabel $ "entry: br label %" ++ lbl
    emitLabel $ lbl ++ ":"
    compileBlock block

    emitLabel "}"

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
compileStmt s = case s of
  Empty                     -> fail "You're in luck, you got an empty stmt!"
  BStmt block               -> do
    compileBlock block
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
    ident'' <- newVar
    compileExpr ident'' expr
    store typ ident'' ident'
    compileStmt (Decl typ rest)
  Decl _ []                         -> do
    return ()
  Ass (Ident ident) expr            -> do
    setReg <- newVar
    compileExpr setReg expr
    setVar ident setReg
  Incr ident                -> return ()
  Decr ident                -> return ()
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
    ident <- newVar
    emit $ "%" ++ ident ++ " = alloca i32"
    emit $ "store i32 " ++ show integer ++ ", i32* %" ++ ident
    emit $ "%" ++ resultReg ++ " = load i32* %" ++ ident
  ELitDoub double        -> do
    ident <- newVar
    emit $ "%" ++ ident ++ " = alloca double"
    emit $ "store double " ++ show double ++ ", double* %" ++ ident
    emit $ "%" ++ resultReg ++ " = load double* %" ++ ident
  ELitTrue               -> do
    ident <- newVar
    emit $ "%" ++ ident ++ " = alloca i1"
    emit $ "store i1 true, i1* %" ++ ident
    emit $ "%" ++ resultReg ++ " = load i1* %" ++ ident
  ELitFalse              -> do
    ident <- newVar
    emit $ "%" ++ ident ++ " = alloca i1"
    emit $ "store i1 false, i1* %" ++ ident
    emit $ "%" ++ resultReg ++ " = load i1* %" ++ ident
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
    emit $ "%" ++ resultReg ++ " = xor i1 %" ++ ident ++ ", %" ++ ident
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
    compileExpr ident1 expr1
    compileExpr ident2 expr2
    emit $ "%" ++ resultReg ++ " = and i1 %" ++ ident1 ++ ", %" ++ ident2
  EOr expr1 expr2        -> do
    ident1 <- newVar
    ident2 <- newVar
    compileExpr ident1 expr1
    compileExpr ident2 expr2
    emit $ "%" ++ resultReg ++ " = or i1 %" ++ ident1 ++ ", %" ++ ident2
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
  let escapedString = string ++ "\\0A\\00"
  let newLength = length string + 2
  let globalString = "@" ++ stringVar ++ " = internal constant [" ++ show newLength ++ " x i8] c\"" ++ escapedString ++ "\""
  stringLiterals %= (globalString:)
  return newLength

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