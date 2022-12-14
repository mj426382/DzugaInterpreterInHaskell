module FrontendChecker where

import AbsGrammar (Arg (..), Block (Block), ClsDef (FunDef, VarDef), Expr (ELitFalse, ELitTrue), Ident (Ident), Stmt (..), TopDef (..), Type (Bool, Fun, Int, Str, Void))
import AddStaticFunctionDeclarations (addErrorDeclaration, addFunctionDeclarations, addPrintBoolDeclaration, addPrintIntDeclaration, addPrintStringDeclaration, addReadIntDeclaration, addReadStringDeclaration)
import CheckingPreparation (prepareExprType)
import CheckingStatements (checkStatementType, checkStatementTypeForMany)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (MonadReader (ask, local))
import Data.Map (insert, lookup)
import Data.Maybe (isNothing)
import TypeCheckHelpers (TC, TCEnv, TCRes, TypeCheckExceptions (DoubleIdentifierInFunctionDeclarationException, FunctionNotReturnException, IvalidMainArgumentsException, MismatchReturnFunctionType, NoMainException), getTypesFromArgs)

addArgsToEnv :: [Arg] -> TC (TCEnv, TCRes)
addArgsToEnv [] = do
  env <- ask
  return (env, Nothing)
addArgsToEnv (arg : rest) = do
  let (Arg typ identifier) = arg
  let (Ident ident) = identifier
  (env, ret) <- addArgsToEnv rest
  case Data.Map.lookup (ident ++ "_0") env of
    Nothing -> return (Data.Map.insert (ident ++ "_0") typ env, Nothing)
    Just typ -> return (env, Nothing)

addTypeArgsToEnv :: [Ident] -> Type -> TC (TCEnv, TCRes)
addTypeArgsToEnv [] _ = do
  env <- ask
  return (env, Nothing)
addTypeArgsToEnv (identifier : rest) typ = do
  let (Ident ident) = identifier
  (env, ret) <- addTypeArgsToEnv rest typ
  case Data.Map.lookup (ident ++ "_0") env of
    Nothing -> return (Data.Map.insert (ident ++ "_0") typ env, Nothing)
    Just typ -> return (env, Nothing)

addFunctionDeclaration :: TopDef -> TC (TCEnv, TCRes)
addFunctionDeclaration (FnDef typ (Ident identifier) args block) = do
  argTypes <- getTypesFromArgs args
  let fnType = (Fun typ argTypes)
  (env, ret) <- addArgsToEnv args
  return (Data.Map.insert (identifier ++ "_0") fnType env, Nothing)

createStringFromArgs :: [Arg] -> String
createStringFromArgs [] = ""
createStringFromArgs (head : tail) = show head ++ createStringFromArgs tail

argExistsInArgs :: Arg -> [Arg] -> Bool
argExistsInArgs (Arg t1 (Ident i1)) ((Arg t2 (Ident i2)) : tail) = if i1 == i2 then True else argExistsInArgs (Arg t1 (Ident i1)) tail
argExistsInArgs t [] = False

checkIfFunctionHaveDifferentIdentifiers :: [Arg] -> Bool
checkIfFunctionHaveDifferentIdentifiers [] = True
checkIfFunctionHaveDifferentIdentifiers (head : tail) = if argExistsInArgs head tail then False else checkIfFunctionHaveDifferentIdentifiers tail

checkIfStmtReturnsIncorrectType :: Stmt -> Type -> Int -> TC (TCEnv)
checkIfStmtReturnsIncorrectType VRet Void depth = do
  env <- ask
  return (env)
checkIfStmtReturnsIncorrectType VRet t depth = throwError $ MismatchReturnFunctionType t Void
checkIfStmtReturnsIncorrectType (Ret stm) typ depth = do
  t <- prepareExprType stm depth
  if typ == t
    then do
      env <- ask
      return (env)
    else throwError $ MismatchReturnFunctionType typ t
checkIfStmtReturnsIncorrectType (Cond e s) typ depth = checkIfStmtReturnsIncorrectType s typ depth
checkIfStmtReturnsIncorrectType (CondElse e s1 s2) typ depth = do
  checkIfStmtReturnsIncorrectType s1 typ depth
  checkIfStmtReturnsIncorrectType s2 typ depth
  env <- ask
  return (env)
checkIfStmtReturnsIncorrectType (While e s) typ depth = checkIfStmtReturnsIncorrectType s typ depth
checkIfStmtReturnsIncorrectType _ _ depth = do
  env <- ask
  return (env)

checkIfStmtsReturnsIncorrectType :: [Stmt] -> Type -> Int -> TC (TCEnv)
checkIfStmtsReturnsIncorrectType (head : tail) t depth = do
  checkIfStmtReturnsIncorrectType head t depth
  checkIfStmtsReturnsIncorrectType tail t depth
  env <- ask
  return (env)
checkIfStmtsReturnsIncorrectType _ _ depth = do
  env <- ask
  return (env)

checkIfFunctionMismatchRetType :: Block -> Type -> Int -> TC (TCEnv)
checkIfFunctionMismatchRetType (Block []) t depth = checkIfStmtsReturnsIncorrectType [] t depth
checkIfFunctionMismatchRetType (Block stmts) t depth = checkIfStmtsReturnsIncorrectType stmts t depth

checkIfStmtIsReturnVal :: Stmt -> Bool
checkIfStmtIsReturnVal (Ret e) = True
checkIfStmtIsReturnVal (BStmt block) = checkIfFunctionReturn block
checkIfStmtIsReturnVal (Cond ELitTrue s) = True
checkIfStmtIsReturnVal (CondElse ELitTrue s1 s2) = checkIfStmtIsReturnVal s1
checkIfStmtIsReturnVal (CondElse ELitFalse s1 s2) = checkIfStmtIsReturnVal s2
checkIfStmtIsReturnVal (CondElse e s1 s2) = checkIfStmtIsReturnVal s1 && checkIfStmtIsReturnVal s2
checkIfStmtIsReturnVal _ = False

checkIfStmtsReturns :: [Stmt] -> Bool
checkIfStmtsReturns [] = False
checkIfStmtsReturns (head : tail) = checkIfStmtIsReturnVal head || checkIfStmtsReturns tail

checkIfFunctionReturn :: Block -> Bool
checkIfFunctionReturn (Block stmts) = checkIfStmtsReturns stmts

checkStatementTypeProgram :: [TopDef] -> TC (TCEnv, TCRes)
checkStatementTypeProgram ((FnDef typ identifier args block) : rest) = do
  let fn = (FnDef typ identifier args block)
  if typ /= Void && checkIfFunctionReturn block == False
    then throwError $ FunctionNotReturnException identifier
    else
      if checkIfFunctionHaveDifferentIdentifiers args == False
        then do
          let (Ident ident) = identifier
          throwError $ DoubleIdentifierInFunctionDeclarationException ident
        else do
          let (Block stmts) = block
          env <- ask
          (envWith, retWith) <- addFunctionDeclaration fn
          if isNothing retWith
            then do
              (env2, ret2) <- local (const envWith) (checkStatementTypeForMany stmts 0)
              local (const env2) (checkIfFunctionMismatchRetType block typ 0)
              local (const env) (checkStatementTypeProgram rest)
            else return (env, retWith)
checkStatementTypeProgram ((TopClsDef ident clsdefs) : rest) = do
  env <- ask
  env2 <- ask
  local (const env2) (checkClassFunction clsdefs)
  return (env, Nothing)
checkStatementTypeProgram ((ExtClsDef ident1 ident2 clsdefs) : rest) = do
  env <- ask
  env2 <- ask
  local (const env2) (checkClassFunction clsdefs)
  return (env, Nothing)
checkStatementTypeProgram [] = do
  env <- ask
  return (env, Nothing)

checkClassFunction :: [ClsDef] -> TC (TCEnv, TCRes)
checkClassFunction ((FunDef typ identifier args block) : rest) = do
  let fn = (FnDef typ identifier args block)
  if typ /= Void && checkIfFunctionReturn block == False
    then throwError $ FunctionNotReturnException identifier
    else
      if checkIfFunctionHaveDifferentIdentifiers args == False
        then do
          let (Ident ident) = identifier
          throwError $ DoubleIdentifierInFunctionDeclarationException ident
        else do
          let (Block stmts) = block
          env <- ask
          (envWith, retWith) <- addFunctionDeclaration fn
          if isNothing retWith
            then do
              (env2, ret2) <- local (const envWith) (checkStatementTypeForMany stmts 0)
              local (const env2) (checkIfFunctionMismatchRetType block typ 0)
              local (const env) (checkClassFunction rest)
            else return (env, retWith)
checkClassFunction ((VarDef typ idents) : rest) = do
  env <- ask
  (env2, ret2) <- local (const env) (addTypeArgsToEnv idents typ)
  return (env2, ret2)
checkClassFunction [] = do
  env <- ask
  return (env, Nothing)

checkOnlyMainFunction :: [Arg] -> TC (TCEnv, TCRes)
checkOnlyMainFunction [] = do
  env <- ask
  return (env, Nothing)
checkOnlyMainFunction args = do
  throwError $ IvalidMainArgumentsException

checkMainFunction :: [TopDef] -> Int -> TC (TCEnv, TCRes)
checkMainFunction ((FnDef typ (Ident identifier) args block) : rest) num =
  if identifier == "main"
    then do
      (env, ret) <- checkOnlyMainFunction args
      return (env, ret)
    else do
      env <- ask
      checkMainFunction rest num
checkMainFunction (cla : rest) num = do
  env <- ask
  checkMainFunction rest num
checkMainFunction [] num =
  if num /= 0
    then throwError $ NoMainException
    else do
      env <- ask
      return (env, Nothing)

countFunctions :: [TopDef] -> Int
countFunctions [] = 0
countFunctions ((FnDef typ (Ident identifier) args block) : rest) = 1 + countFunctions rest
countFunctions (cla : rest) = countFunctions rest

checkProgram :: [TopDef] -> TC (TCEnv, TCRes)
checkProgram functions = do
  checkMainFunction functions (countFunctions functions)
  (env, ret) <- addFunctionDeclarations functions
  (env2, ret2) <- local (const env) (addPrintIntDeclaration functions)
  (env3, ret3) <- local (const env2) (addPrintStringDeclaration functions)
  (env35, ret35) <- local (const env3) (addReadIntDeclaration functions)
  (env375, ret375) <- local (const env35) (addReadStringDeclaration functions)
  (env4, ret4) <- local (const env375) (addPrintBoolDeclaration functions)
  (env45, ret45) <- local (const env4) (addErrorDeclaration functions)
  (env5, ret5) <- local (const env45) (checkStatementTypeProgram functions)
  return (env5, ret5)
