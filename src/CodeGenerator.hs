{-# LANGUAGE TemplateHaskell #-}

module CodeGenerator where

import qualified Data.Map as M (Map, empty, insert, lookup)
import Control.Lens (makeLenses, use, (.=), (%=), (^.), (+=))
import Control.Monad (forM_, when, replicateM)
import Control.Monad.State (StateT, execStateT)
import Data.List (intercalate, nub)

import Grammar.ErrM (Err)
import Grammar.Abs


type GenM a = StateT Env Err a

data Env = Env {
  _code           :: [String],
  _currentExpType :: Maybe Type,

  _nextVariable   :: Int,
  _nextLabel      :: Int,
  _nextString     :: Int,

  _stringLiterals :: [String],
  _llvmNames      :: [M.Map String String],
  _lastStmtStack  :: [Bool],
  _arrayTypes     :: [Type]
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
  _lastStmtStack = [],
  _arrayTypes = []
}

makeLenses ''Env

compilellvm :: Program -> Err String
compilellvm p = do
  st <- execStateT (compileProgram p) emptyEnv
  return $ unlines . reverse . (^. code) $ st 

compileProgram :: Program -> GenM ()
compileProgram (Program topDefs) = do
  compileTopDefs topDefs
  c <- use code
  code .= []

  emitStringLiterals
  mapM_ emit [
    "",
    "declare void @printInt(i32)",
    "declare void @printDouble(double)",
    "declare void @printString(i8*)",
    "declare i32 @readInt()",
    "declare double @readDouble()",
    "",
    "declare noalias i8* @calloc(i32, i32)",
    "declare noalias i8* @malloc(i32)",
    ""
    ]
  emitArrayTypes
  
  code %= (c++)

compileTopDefs :: [TopDef] -> GenM ()
compileTopDefs [] = return ()
compileTopDefs (FnDef typ (Ident name) args block : rest) = do
  emitInstr ""

  scoped $ do
    llvmArgs <- convertArgs
    emit $ "define " ++ makeLLVMType typ ++ " @" ++ name ++ "(" ++ llvmArgs ++ ") {"

    lbl <- newLabel
    emit "entry:"
    emitConvertedArgs
    emitInstr $ "br label %" ++ lbl
    emit $ lbl ++ ":"

    compileBlock block

    when (typ == Void) $ emitInstr "ret void" -- Catch that implicit return

    emit "}"

  compileTopDefs rest
  where
    convertArgs :: GenM String
    convertArgs = mapM makeLLVMArg args >>= \as -> return (intercalate ", " as)

    makeLLVMArg :: Arg -> GenM String
    makeLLVMArg (Arg t (Ident n)) = do
      argRegName <- createRegisterFor n
      return $ makeLLVMType t ++ " %" ++ argRegName

    emitConvertedArgs :: GenM ()
    emitConvertedArgs = forM_ args $ \(Arg t (Ident n)) -> do
      newReg <- createRegister
      emitInstr $ "%" ++ newReg ++ " = alloca " ++ makeLLVMType t
      oldReg <- registerFor n
      store t oldReg newReg
      setRegisterForName n newReg

makeLLVMType :: Type -> String
makeLLVMType Int       = "i32"
makeLLVMType Doub      = "double"
makeLLVMType Bool      = "i1"
makeLLVMType Void      = "void"
makeLLVMType String    = "i8*"
makeLLVMType (Array t) = "%__Arr_" ++ makeLLVMType t
makeLLVMType t         = error $ "makeLLVMType lacks implementation for " ++ show t

compileBlock :: Block -> GenM ()
compileBlock (Block stmts) =
  scoped $ forM_ stmts $ \stmt -> if stmt == last stmts
    then withLastStmt stmt
    else withInitStmt stmt

compileStmt :: Stmt -> GenM ()
compileStmt s = case s of
  Empty -> return ()
  BStmt block -> compileBlock block
  Decl t (NoInit (Ident name) : rest) -> do
    reg <- createRegisterFor name
    let llvmT = makeLLVMType t
    emitInstr $ "%" ++ reg ++ " = alloca " ++ llvmT
    emitInstr $ "store " ++ llvmT ++ " " ++ initValue t ++ ", " ++ llvmT ++ "* %" ++ reg
    compileStmt (Decl t rest)
  Decl t (Init (Ident declaredName) expr : rest) -> do
    let llvmT = makeLLVMType t
    exprReg <- createRegister
    compileExpr exprReg expr
    reg <- createRegisterFor declaredName
    emitInstr $ "%" ++ reg ++ " = alloca " ++ llvmT
    store t exprReg reg
    compileStmt (Decl t rest)
  Decl _ _ -> return ()
  Ass (Ident name) expr@(ETyped _ t) -> do
    exprReg <- createRegister
    compileExpr exprReg expr
    reg <- registerFor name
    store t exprReg reg
  Incr (Ident ident) -> do
    resultReg <- createRegister
    varReg <- createRegister
    adrReg <- registerFor ident
    emitInstr $ "%" ++ varReg ++ " = load i32* %" ++ adrReg
    emitInstr $ "%" ++ resultReg ++ " = add i32 %" ++ varReg ++ ", 1"
    store Int resultReg adrReg
  Decr (Ident ident) -> do
    resultReg <- createRegister
    varReg <- createRegister
    adrReg <- registerFor ident
    emitInstr $ "%" ++ varReg ++ " = load i32* %" ++ adrReg
    emitInstr $ "%" ++ resultReg ++ " = sub i32 %" ++ varReg ++ ", 1"
    store Int resultReg adrReg
  Ret e@(ETyped _ typ)    -> do
    exprReg <- createRegister
    compileExpr exprReg e
    emitInstr $ "ret " ++ makeLLVMType typ ++ " %" ++ exprReg
  Ret (ELitInt x) -> 
    emitInstr $ "ret " ++ makeLLVMType Int ++ " " ++ show x
  Ret (ELitDoub x) ->
    emitInstr $ "ret " ++ makeLLVMType Doub ++ " " ++ show x
  Ret ELitTrue ->
    emitInstr $ "ret " ++ makeLLVMType Bool ++ " 1"
  Ret ELitFalse ->
    emitInstr $ "ret " ++ makeLLVMType Bool ++ " 0"
  VRet ->
    emitInstr "ret void"
  Cond expr stmt -> do
    condLbl <- newLabel
    nextLbl <- newLabel
    exprReg <- createRegister

    compileExpr exprReg expr
    emitInstr $ "br i1 %" ++ exprReg ++ ", label %" ++ condLbl ++ ", label %" ++ nextLbl
    emit $ condLbl ++ ":"
    scoped $ compileStmt stmt
    emitInstr $ "br label %" ++ nextLbl

    emit $ nextLbl ++ ":"
    unreachable <- atEnd
    when unreachable $ emitInstr "unreachable"
  CondElse expr stmt1 stmt2 -> do
    lblIf <- newLabel
    lblElse <- newLabel
    nextLbl <- newLabel
    exprReg <- createRegister

    compileExpr exprReg expr
    emitInstr $ "br i1 %" ++ exprReg ++ ", label %" ++ lblIf ++ ", label %" ++ lblElse
    emit $ lblIf ++ ":"
    scoped $ compileStmt stmt1
    emitInstr $ "br label %" ++ nextLbl

    emit $ lblElse ++ ":"
    scoped $ compileStmt stmt2
    emitInstr $ "br label %" ++ nextLbl

    emit $ nextLbl ++ ":"
    unreachable <- atEnd
    when unreachable $ emitInstr "unreachable"

  While expr stmt -> do
    lblBegin <- newLabel
    lblLoop <- newLabel
    lblNext <- newLabel
    condReg <- createRegister

    emitInstr $ "br label %" ++ lblBegin
    emit $ lblBegin ++ ":"
    scoped $ do
      compileExpr condReg expr
      emitInstr $ "br i1 %" ++ condReg ++ ", label %" ++ lblLoop ++ ", label %" ++ lblNext
      emit $ lblLoop ++ ":"
      scoped $ compileStmt stmt
    emitInstr $ "br label %" ++ lblBegin

    emit $ lblNext ++ ":"
    unreachable <- atEnd
    when unreachable $ emitInstr "unreachable"

  For t (Ident ident) expr stmt -> do
    let llvmT     = makeLLVMType (Array t)
    let llvmElemT = makeLLVMType t

    lblBegin  <- newLabel
    lblLoop  <- newLabel
    lblNext   <- newLabel

    -- Reduce the array expression
    arrReg <- createRegister
    compileExpr arrReg expr

    -- Coerce array to array* for getelementptr
    objPtrReg <- createRegister
    emitInstr $ "%" ++ objPtrReg ++ " = alloca " ++ llvmT
    emitInstr $ "store " ++ 
        llvmT ++ " %" ++ arrReg ++ ", " ++
        llvmT ++ "* %" ++ objPtrReg

    -- Index the length field of the array struct
    sizePtrReg <- createRegister
    emitInstr $ "%" ++ sizePtrReg ++ " = getelementptr " ++ llvmT ++ "* " ++
      "%" ++ objPtrReg ++ ", i32 0, i32 0"

    -- Array length now in lenReg
    lenReg <- createRegister
    emitInstr $ "%" ++ lenReg ++ " = load i32* %" ++ sizePtrReg 

    -- Index the array field of the array struct
    arrayPtrReg <- createRegister
    emitInstr $ "%" ++ arrayPtrReg ++ " = getelementptr " ++ llvmT ++ "* " ++
      "%" ++ objPtrReg ++ ", i32 0, i32 1"

    -- Dereference pointer into array struct to point to the wrapped array
    arrayDerefReg <- createRegister
    emitInstr $ "%" ++ arrayDerefReg ++ " = load " ++ llvmElemT ++ "** %" ++ arrayPtrReg

    -- Loop setup, set i = 0
    counterReg <- createRegister
    emitInstr $ "%" ++ counterReg ++ " = alloca i32"
    emitInstr $ "store i32 0, i32* %" ++ counterReg

    -- Loop condition checking label
    emitInstr $ "br label %" ++ lblBegin
    emit $ lblBegin ++ ":"
    
    -- Compare index with array length (i < a.length)
    indexReg <- createRegister
    emitInstr $ "%" ++ indexReg ++ " = load i32* %" ++ counterReg
    condReg <- createRegister
    emitInstr $ "%" ++ condReg ++ " = icmp slt i32 %" ++ indexReg ++ ", %" ++ lenReg

    -- Loop on previous comparison
    emitInstr $ "br i1 %" ++ condReg ++ ", label %" ++ lblLoop ++ ", label %" ++ lblNext
    emit $ lblLoop ++ ":"

    -- Increment counter (i++)
    incrCountReg <- createRegister
    emitInstr $ "%" ++ incrCountReg ++ " = add i32 1, %" ++ indexReg
    emitInstr $ "store i32 %" ++ incrCountReg ++ ", i32* %" ++ counterReg

    scoped $ do
      -- Add the loop variable
      loopVarReg <- createRegisterFor ident
      emitInstr $ "%" ++ loopVarReg ++ " = getelementptr " ++ llvmElemT ++ "* " ++
        "%" ++ arrayDerefReg ++ ", i32 %" ++ indexReg

      scoped $ compileStmt stmt

    emitInstr $ "br label %" ++ lblBegin
    emit $ lblNext ++ ":"
    

  ArrAss (Ident name) indexExpr valueExpr@(ETyped _ t) -> do
    let llvmT     = makeLLVMType (Array t)
    let llvmElemT = makeLLVMType t

    -- Compile index i32
    indexReg <- createRegister
    compileExpr indexReg indexExpr

    -- Index the array struct
    reg <- registerFor name
    arrayPtrReg <- createRegister
    emitInstr $ "%" ++ arrayPtrReg ++ " = getelementptr " ++ llvmT ++ "* " ++
      "%" ++ reg ++ ", i32 0, i32 1"

    -- Dereference pointer into array struct to point to the wrapped array
    arrayDerefReg <- createRegister
    emitInstr $ "%" ++ arrayDerefReg ++ " = load " ++ llvmElemT ++ "** %" ++ arrayPtrReg

    -- Index the array
    elemPtrReg <- createRegister
    emitInstr $ "%" ++ elemPtrReg ++ " = getelementptr " ++ llvmElemT ++ "* " ++
      "%" ++ arrayDerefReg ++ ", i32 %" ++ indexReg

    -- Compile value expression
    valueExprReg <- createRegister
    compileExpr valueExprReg valueExpr

    -- Store computed value in elemented pointer
    store t valueExprReg elemPtrReg

  SExp expr ->
    case expr of
      ETyped app@(EApp _ _) t -> do
        exprReg <- createRegister
        setCurrentExpType t
        compileExpr exprReg app
      _ -> return ()

  stmt -> fail $ "CRITICAL ERROR: Unexpected stmt " ++ show stmt

compileExpr :: String -> Expr -> GenM ()
compileExpr resultReg ex = case ex of
  EVar (Ident ident) -> do
    t <- getCurrentExpType
    reg <- registerFor ident
    emitInstr $ "%" ++ resultReg ++ " = load " ++ makeLLVMType t ++ "* %" ++ reg
  ELitInt integer -> do
    tempReg <- createRegister
    emitInstr $ "%" ++ tempReg ++ " = alloca i32"
    emitInstr $ "store i32 " ++ show integer ++ ", i32* %" ++ tempReg
    emitInstr $ "%" ++ resultReg ++ " = load i32* %" ++ tempReg
  ELitDoub double -> do
    tempReg <- createRegister
    emitInstr $ "%" ++ tempReg ++ " = alloca double"
    emitInstr $ "store double " ++ show double ++ ", double* %" ++ tempReg
    emitInstr $ "%" ++ resultReg ++ " = load double* %" ++ tempReg
  ELitTrue -> do
    tempReg <- createRegister
    emitInstr $ "%" ++ tempReg ++ " = alloca i1"
    emitInstr $ "store i1 true, i1* %" ++ tempReg
    emitInstr $ "%" ++ resultReg ++ " = load i1* %" ++ tempReg
  ELitFalse -> do
    tempReg <- createRegister
    emitInstr $ "%" ++ tempReg ++ " = alloca i1"
    emitInstr $ "store i1 false, i1* %" ++ tempReg
    emitInstr $ "%" ++ resultReg ++ " = load i1* %" ++ tempReg
  EApp (Ident fnName) exprs -> do
    t <- getCurrentExpType
    let l = length exprs
    regs <- replicateM l createRegister
    let exprPairs = zip regs exprs
    mapM_ (uncurry compileExpr) exprPairs
    let typePairs = zip regs (map (\(ETyped _ exprType) -> makeLLVMType exprType) exprs)
    let argumentList = intercalate ", " (map (\(argName, argType) -> argType ++ " %" ++ argName) typePairs)
    let prefix = case t of Void -> ""
                           _    -> "%" ++ resultReg ++ " = "
    emitInstr $ prefix ++ "call " ++ 
        makeLLVMType t ++ " @" ++ fnName ++ "(" ++ argumentList ++ ")"
  EString string -> do
    stringVar <- createRegister -- TODO separate naming scheme for string literals
    strLength <- addStringLiteral stringVar string
    emitInstr $ "%" ++ resultReg ++ 
        " = bitcast [" ++ show strLength ++ " x i8]* " ++ 
        "@" ++ stringVar ++ " to i8*"
  Neg expr               -> do
    exprType <- getCurrentExpType
    let op = case exprType of Int  -> "sub"
                              Doub -> "fsub"
                              _    -> "undefined"
    exprReg <- createRegister
    compileExpr exprReg expr
    emitInstr $ "%" ++ resultReg ++ " = " ++ op ++ " " 
      ++ makeLLVMType exprType ++ " " ++ initValue exprType ++ ", %" ++ exprReg
  Not expr               -> do
    exprReg <- createRegister
    compileExpr exprReg expr
    emitInstr $ "%" ++ resultReg ++ " = xor i1 %" ++ exprReg ++ ", true"
  EMul expr1 mulOp expr2 -> do
    t <- getCurrentExpType
    exprReg1 <- createRegister
    exprReg2 <- createRegister
    compileExpr exprReg1 expr1
    compileExpr exprReg2 expr2
    emitInstr $ "%" ++ resultReg ++ " = " ++ 
        getMulOp t mulOp ++ " " ++ makeLLVMType t ++ 
        " %" ++ exprReg1 ++ ", %" ++ exprReg2
  EAdd expr1 addOp expr2 -> do
    t <- getCurrentExpType
    exprReg1 <- createRegister
    exprReg2 <- createRegister
    compileExpr exprReg1 expr1
    compileExpr exprReg2 expr2
    emitInstr $ "%" ++ resultReg ++ " = " ++ getAddOp t addOp ++ " " ++ makeLLVMType t ++ " %" ++ exprReg1 ++ ", %" ++ exprReg2
  ERel expr1@(ETyped _ typ) relOp expr2 -> do
    exprReg1 <- createRegister
    exprReg2 <- createRegister
    compileExpr exprReg1 expr1
    compileExpr exprReg2 expr2
    emitInstr $ "%" ++ resultReg ++ " = " ++ getRelOp typ relOp ++ " " ++ makeLLVMType typ ++ " %" ++ exprReg1 ++ ", %" ++ exprReg2
  EAnd expr1 expr2 -> do
    exprReg1 <- createRegister
    exprReg2 <- createRegister

    cmpMem1 <- createRegister
    cmpMem2 <- createRegister
    cmpReg1 <- createRegister
    cmpReg2 <- createRegister

    lblEval <- newLabel
    lblEnd <- newLabel

    emitInstr $ "%" ++ cmpMem1 ++ " = alloca i1"
    emitInstr $ "%" ++ cmpMem2 ++ " = alloca i1"
    emitInstr $ "store i1 false, i1* %" ++ cmpMem2

    compileExpr exprReg1 expr1
    store Bool exprReg1 cmpMem1
    emitInstr $ "br i1 %" ++ exprReg1 ++ ", label %" ++ lblEval ++ ", label %" ++ lblEnd

    emit $ lblEval ++ ":"
    compileExpr exprReg2 expr2
    store Bool exprReg2 cmpMem2
    emitInstr $ "br label %" ++ lblEnd

    emit $ lblEnd ++ ":"
    emitInstr $ "%" ++ cmpReg1 ++ " = load i1* %" ++ cmpMem1
    emitInstr $ "%" ++ cmpReg2 ++ " = load i1* %" ++ cmpMem2
    emitInstr $ "%" ++ resultReg ++ " = and i1 %" ++ cmpReg1 ++ ", %" ++ cmpReg2 

  EOr expr1 expr2 -> do
    exprReg1 <- createRegister
    exprReg2 <- createRegister

    cmpMem1 <- createRegister
    cmpMem2 <- createRegister
    cmpReg1 <- createRegister
    cmpReg2 <- createRegister

    lblEval <- newLabel
    lblEnd <- newLabel

    emitInstr $ "%" ++ cmpMem1 ++ " = alloca i1"
    emitInstr $ "%" ++ cmpMem2 ++ " = alloca i1"
    emitInstr $ "store i1 false, i1* %" ++ cmpMem2

    compileExpr exprReg1 expr1
    store Bool exprReg1 cmpMem1
    emitInstr $ "br i1 %" ++ exprReg1 ++ ", label %" ++ lblEnd ++ ", label %" ++ lblEval

    emit $ lblEval ++ ":"
    compileExpr exprReg2 expr2
    store Bool exprReg2 cmpMem2
    emitInstr $ "br label %" ++ lblEnd

    emit $ lblEnd ++ ":"
    emitInstr $ "%" ++ cmpReg1 ++ " = load i1* %" ++ cmpMem1
    emitInstr $ "%" ++ cmpReg2 ++ " = load i1* %" ++ cmpMem2
    emitInstr $ "%" ++ resultReg ++ " = or i1 %" ++ cmpReg1 ++ ", %" ++ cmpReg2 

  ENew t expr -> do
    let llvmT = makeLLVMType (Array t)
    let llvmElemT = makeLLVMType t
    addArrayType t

    exprReg <- createRegister
    compileExpr exprReg expr

    -- Create the array object with a size field
    objectReg <- createRegister
    emitInstr $ "%" ++ objectReg ++ " = alloca " ++ llvmT

    -- Create a pointer to the size field
    sizePtrReg <- createRegister
    emitInstr $ "%" ++ sizePtrReg ++ " = getelementptr " ++ llvmT ++ "* " ++
      "%" ++ objectReg ++ ", i32 0, i32 0"

    -- Populate size field
    emitInstr  $ "store i32 %" ++ exprReg ++ ", i32* %" ++ sizePtrReg

    -- Allocate array on heap
    arrReg <- createRegister
    emitInstr $ "%" ++ arrReg ++ " = call i8* @calloc(" ++ 
        "i32 " ++ sizeof llvmT ++ ", " ++ 
        "i32 %" ++ exprReg ++ ")"
    
    -- Cast calloc return type to llvmElemT* 
    castReg <- createRegister
    emitInstr $ "%" ++ castReg ++ 
        " = bitcast i8* " ++ 
        "%" ++ arrReg ++ " to " ++ llvmElemT ++ "*"

    -- Get pointer to array location
    arrPtrReg <- createRegister
    emitInstr $ "%" ++ arrPtrReg ++ " = getelementptr " ++ llvmT ++ "* " ++
        "%" ++ objectReg ++ ", i32 0, i32 1"

    -- Store address from castReg as the new array
    emitInstr $ "store " ++ llvmElemT ++ "* %" ++ castReg ++ ", " ++
        llvmElemT ++ "** %" ++ arrPtrReg

    emitInstr $ "%" ++ resultReg ++ " = load " ++ llvmT ++ "* %" ++ objectReg

  ELength expr@(ETyped _ t) -> do
    let llvmT = makeLLVMType t

    -- Compile array expression
    exprReg <- createRegister
    compileExpr exprReg expr

    -- Corce array to array*
    exprPtrReg <- createRegister
    emitInstr $ "%" ++ exprPtrReg ++ " = alloca " ++ llvmT
    emitInstr $ "store " ++ 
        llvmT ++ " %" ++ exprReg ++ ", " ++
        llvmT ++ "* %" ++ exprPtrReg

    -- Index length field
    sizePtrReg <- createRegister
    emitInstr $ "%" ++ sizePtrReg ++ " = getelementptr " ++ llvmT ++ "* " ++
      "%" ++ exprPtrReg ++ ", i32 0, i32 0"
    emitInstr $ "%" ++ resultReg ++ " = load i32* %" ++ sizePtrReg 

  EIndex (Ident ident) indexExpr -> do
    t <- getCurrentExpType
    let llvmT     = makeLLVMType (Array t)
    let llvmElemT = makeLLVMType t

    variableReg <- registerFor ident

    -- Compile index expression
    indexReg <- createRegister
    compileExpr indexReg indexExpr

    -- Index into the array field of the struct
    arrayPtrReg <- createRegister
    emitInstr $ "%" ++ arrayPtrReg ++ " = getelementptr " ++ llvmT ++ "* " ++
      "%" ++ variableReg ++ ", i32 0, i32 1"

    -- Dereference the struct[1] pointer to get the array memory
    arrayDerefReg <- createRegister
    emitInstr $ "%" ++ arrayDerefReg ++ " = load " ++ llvmElemT ++ "** %" ++ arrayPtrReg

    -- Index into the array memory
    elemPtrReg <- createRegister
    emitInstr $ "%" ++ elemPtrReg ++ " = getelementptr " ++ llvmElemT ++ "* " ++
      "%" ++ arrayDerefReg ++ ", i32 %" ++ indexReg

    emitInstr $ "%" ++ resultReg ++ " = load i32* %" ++ elemPtrReg 

  ETyped expr typ -> do
    setCurrentExpType typ
    compileExpr resultReg expr

  expr -> fail $ "compileExpr pattern match failed on " ++ show expr

sizeof :: String -> String
sizeof t = "ptrtoint ("++t++"* getelementptr ("++t++"* null, i32 1) to i32)"

getMulOp :: Type -> MulOp -> String
getMulOp Int Times  = "mul"
getMulOp Int Div    = "sdiv"
getMulOp Int Mod    = "srem"
getMulOp Doub Times = "fmul"
getMulOp Doub Div   = "fdiv"
getMulOp Doub Mod   = "rem"
getMulOp _ _        = "undefinedMulOp"

getAddOp :: Type -> AddOp -> String
getAddOp Int Plus   = "add"
getAddOp Int Minus  = "sub"
getAddOp Doub Plus  = "fadd"
getAddOp Doub Minus = "fsub"
getAddOp _ _        = "undefinedAddAp"

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
getRelOp _ _      = "undefinedRelOp"

initValue :: Type -> String
initValue Doub = "0.0"
initValue _ = "0"

store :: Type -> String -> String -> GenM ()
store typ src tgt = do
  let llvmT = makeLLVMType typ
  emitInstr $ "store " ++ llvmT ++ " %" ++ src ++ ", " ++ llvmT ++ "* %" ++ tgt

emitInstr :: String -> GenM ()
emitInstr s = emit $ "  " ++ s

emit :: String -> GenM ()
emit s = code %= (s:)

addStringLiteral :: String -> String -> GenM Int
addStringLiteral stringVar string = do
  let escapedString = string ++ "\\00"
  let newLength = length string + 1
  let globalString = "@" ++ stringVar ++ " = internal constant [" ++ show newLength ++ " x i8] c\"" ++ escapedString ++ "\""
  stringLiterals %= (globalString:)
  return newLength

emitStringLiterals :: GenM ()
emitStringLiterals = do
  literals <- use stringLiterals
  mapM_ emit literals

addArrayType :: Type -> GenM ()
addArrayType t = arrayTypes %= (t:)

emitArrayTypes :: GenM ()
emitArrayTypes = do
  types <- use arrayTypes
  forM_ (nub types) $ \t -> do
    let llvmT = makeLLVMType t
    let typeName = makeLLVMType (Array t)
    mapM_ emit [
        typeName ++ " = type {",
        "  i32,",
        "  " ++ llvmT ++ "*",
        "}"
      ]

createRegisterFor :: String -> GenM String
createRegisterFor x = do 
  var <- createRegister
  llvmNames %= \(s:ss) -> M.insert x var s : ss
  return var

createRegister :: GenM String
createRegister = do
  num <- use nextVariable
  nextVariable += 1
  return $ "var" ++ show num

setRegisterForName :: String -> String -> GenM ()
setRegisterForName name reg =
  llvmNames %= \(s:ss) -> M.insert name reg s : ss

registerFor :: String -> GenM String
registerFor name = do
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

setCurrentExpType :: Type -> GenM ()
setCurrentExpType t =
  currentExpType .= Just t

getCurrentExpType :: GenM Type
getCurrentExpType = do
  expType <- use currentExpType
  case expType of
    Nothing -> fail "CRITICAL ERROR: currentExpType not set"
    Just x  -> return x

scoped :: GenM a -> GenM a
scoped fn = do
  llvmNames %= (M.empty:)
  value <- fn
  llvmNames %= tail
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
  return $ head st
