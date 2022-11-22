module DzugaInterpreter where

import AbsGrammar (Arg (..), Block (Block), Expr (EAdd, EAnd, EApp, ELitFalse, ELitInt, ELitTrue, EMul, EOr, ERel, EString, EVar, Neg, Not), Ident (..), Item (Init, NoInit), MulOp (Div, Mod), RelOp (LTH), Stmt (Ass, BStmt, Cond, CondElse, Decl, Decr, Incr, Ret, VRet, While), TopDef (..), Type (Void))
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO), unless)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask, local), unless)
import Data.Maybe (isNothing)
import DzugaInterpreterHelpers (FArg, II, IIEnv, ReturnResult, RuntimeExceptions (DivisionByZeroException, ModulusByZeroException, NoReturnException, OutOfRangeExeption), ValueInMemory (BooleanValue, FunctionValue, IntValue, StringValue), createEmptyList, defaultValueOfType, makeString)
import Memory (addVarToMemory, readFromMemory, readFunFromMemory, readIntFromMemory)
import Operators (makeAdd, makeMul, makeRel)

makeExpression :: Expr -> II ValueInMemory
makeExpression (EVar ident) = readFromMemory ident
makeExpression (EApp identifier args) = do
  (env2, Just ret) <- runFunction identifier args
  return ret
makeExpression (ELitInt x) = return $ IntValue x
makeExpression (EAdd e1 op e2) = do
  IntValue r1 <- makeExpression e1
  IntValue r2 <- makeExpression e2
  return $ IntValue $ makeAdd op r1 r2
makeExpression (EMul e1 op e2) = do
  IntValue r1 <- makeExpression e1
  IntValue r2 <- makeExpression e2
  case op of
    Div -> if r2 == 0 then throwError DivisionByZeroException else return $ IntValue $ makeMul op r1 r2
    Mod -> if r2 == 0 then throwError ModulusByZeroException else return $ IntValue $ makeMul op r1 r2
    _ -> return $ IntValue $ makeMul op r1 r2
makeExpression (Neg e) = do
  IntValue r <- makeExpression e
  return $ IntValue $ - r
makeExpression ELitFalse = return $ BooleanValue False
makeExpression ELitTrue = return $ BooleanValue True
makeExpression (Not e) = do
  BooleanValue r <- makeExpression e
  return $ BooleanValue $ not r
makeExpression (EAnd e1 e2) = do
  BooleanValue r1 <- makeExpression e1
  BooleanValue r2 <- makeExpression e2
  return $ BooleanValue $ r1 && r2
makeExpression (EOr e1 e2) = do
  BooleanValue r1 <- makeExpression e1
  BooleanValue r2 <- makeExpression e2
  return $ BooleanValue $ r1 || r2
makeExpression (ERel e1 op e2) = do
  IntValue r1 <- makeExpression e1
  IntValue r2 <- makeExpression e2
  return $ BooleanValue $ makeRel op r1 r2
makeExpression (EString e) = return $ StringValue e

createMemValueArray :: [Expr] -> II [ValueInMemory]
createMemValueArray [] = return $ []
createMemValueArray [expr] = do
  val <- makeExpression expr
  return $ [val]
createMemValueArray (expr : rest) = do
  val <- makeExpression expr
  tail <- createMemValueArray rest
  return $ val : tail

getMemValArrayFromExpr :: [Expr] -> II [ValueInMemory]
getMemValArrayFromExpr [] = return $ []
getMemValArrayFromExpr [expr] = do
  memVal <- makeExpression expr
  return $ [memVal]
getMemValArrayFromExpr (expr : rest) = do
  memVal <- makeExpression expr
  restVals <- getMemValArrayFromExpr rest
  return $ (memVal : restVals)

addArgsToInterpreterEnv :: [FArg] -> [ValueInMemory] -> IIEnv -> II IIEnv
addArgsToInterpreterEnv [] [] env = do
  return $ env
addArgsToInterpreterEnv (funArg : rest) (memVal : restMemVals) env = do
  let (argName, typ) = funArg
  env <- local (const env) (addVarToMemory (Ident argName) memVal)
  env <- local (const env) (addArgsToInterpreterEnv rest restMemVals env)
  return $ env

runFunction :: Ident -> [Expr] -> II (IIEnv, ReturnResult)
runFunction identifier exprs = do
  funDef <- readFunFromMemory identifier
  let (stmts, env, argList, typ) = funDef
  memVals <- getMemValArrayFromExpr exprs

  let funInnerDef = (stmts, env, argList, typ)
  env <- local (const env) (addVarToMemory identifier (FunctionValue funInnerDef))

  env2 <- local (const env) (addArgsToInterpreterEnv argList memVals env)
  (env2, result) <- local (const env2) (interpretMany stmts)
  if typ == Void
    then return (env2, Nothing)
    else
      if isNothing result
        then throwError NoReturnException
        else return (env2, result)

executeStatement :: Stmt -> II (IIEnv, ReturnResult)
executeStatement (Decl typ [(NoInit identifier)]) = do
  defValue <- defaultValueOfType typ
  env <- addVarToMemory identifier (defValue)
  return (env, Nothing)
executeStatement (Decl typ [(Init identifier expr)]) = do
  val <- makeExpression expr
  env <- addVarToMemory identifier val
  return (env, Nothing)
executeStatement (Decl typ (item : rest)) = do
  (env, ret) <- executeStatement (Decl typ [item])
  local (const env) (executeStatement (Decl typ rest))
executeStatement (BStmt block) = do
  let (Block stmts) = block
  interpretMany stmts
executeStatement (Ass identifier expr) = do
  val <- makeExpression expr
  env <- addVarToMemory identifier val
  return (env, Nothing)
executeStatement (Incr identifier) = do
  intValue <- readIntFromMemory identifier
  let memVal = IntValue ((fromIntegral intValue) + 1)
  env <- addVarToMemory identifier memVal
  return (env, Nothing)
executeStatement (Decr identifier) = do
  intValue <- readIntFromMemory identifier
  let memVal = IntValue ((fromIntegral intValue) - 1)
  env <- addVarToMemory identifier memVal
  return (env, Nothing)
executeStatement (Cond expr stm) = do
  val <- makeExpression expr
  let (BooleanValue booleanValue) = val
  if booleanValue
    then executeStatement stm
    else do
      env <- ask
      return (env, Nothing)
executeStatement (CondElse expr stm1 stm2) = do
  val <- makeExpression expr
  let (BooleanValue booleanValue) = val
  if booleanValue
    then executeStatement stm1
    else executeStatement stm2
executeStatement (While expr stm) = do
  val <- makeExpression expr
  let (BooleanValue booleanValue) = val
  if booleanValue
    then do
      (env, ret) <- executeStatement stm
      if isNothing ret
        then local (const env) (executeStatement (While expr stm))
        else return (env, ret)
    else do
      env <- ask
      return (env, Nothing)
executeStatement (Ret expr) = do
  res <- makeExpression expr
  env <- ask
  return (env, Just res)
executeStatement (VRet) = do
  env <- ask
  return (env, Nothing)
executeStatement _ = do
  env <- ask
  return (env, Nothing)

interpretMany :: [Stmt] -> II (IIEnv, ReturnResult)
interpretMany (s : xs) = do
  (env, ret) <- executeStatement s
  if isNothing ret
    then local (const env) (interpretMany xs)
    else return (env, ret)
interpretMany [] = do
  env <- ask
  return (env, Nothing)

createFunArgList :: [Arg] -> [FArg]
createFunArgList [] = []
createFunArgList (arg : rest) = do
  let (Arg typ (Ident identifier)) = arg
  let singleArg = (identifier, typ)
  let result = singleArg : (createFunArgList rest)
  result

interpretProgram :: [TopDef] -> II (IIEnv, ReturnResult)
interpretProgram (fun : rest) = do
  let (FnDef typ identifier args block) = fun
  let (Ident functionName) = identifier
  let (Block stmts) = block
  env <- ask
  let funInnerDef = (stmts, env, (createFunArgList args), typ)
  env <- local (const env) (addVarToMemory identifier (FunctionValue funInnerDef))
  if (functionName == "dzuga")
    then do
      interpretMany stmts
    else do
      (env, result) <- local (const env) (interpretProgram rest)
      return (env, Nothing)
