module StaticTypeChecker where

import AbsGrammar (Arg (..), Block (Block), Expr (ELitFalse, ELitTrue), Ident (Ident), Stmt (..), TopDef (..), Type (Bool, Fun, Int, Str, Void))
import AddStaticFunctionDeclarations (addFunctionDeclarations, addPrintBoolDeclaration, addPrintIntDeclaration, addPrintStringDeclaration, addReadIntDeclaration, addReadStringDeclaration)
import CheckingPreparation (prepareExprType)
import CheckingStatements (checkStatementType, checkStatementTypeForMany)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (MonadReader (ask, local))
import Data.Map (insert, lookup)
import Data.Maybe (isNothing)
import TypeCheckHelpers (TC, TCEnv, TCRes, TypeCheckExceptions (DoubleIdentifierInFunctionDeclarationException, FunctionNotReturnException, MismatchReturnFunctionType), getTypesFromArgs)

addArgsToEnv :: [Arg] -> TC (TCEnv, TCRes)
addArgsToEnv [] = do
  env <- ask
  return (env, Nothing)
addArgsToEnv [arg] = do
  let (Arg typ identifier) = arg
  let (Ident ident) = identifier
  env <- ask
  case Data.Map.lookup ident env of
    Nothing -> return (Data.Map.insert ident typ env, Nothing)
    Just typ -> return (env, Nothing)
-- Just typ -> throwError $ DoubleIdentifierDeclarationExeption ident
addArgsToEnv (arg : rest) = do
  let (Arg typ identifier) = arg
  let (Ident ident) = identifier
  (env, ret) <- addArgsToEnv rest
  return (Data.Map.insert ident typ env, Nothing)

addFunctionDeclaration :: TopDef -> TC (TCEnv, TCRes)
addFunctionDeclaration (FnDef typ (Ident identifier) args block) = do
  argTypes <- getTypesFromArgs args
  let fnType = (Fun typ argTypes)
  (env, ret) <- addArgsToEnv args
  return (Data.Map.insert identifier fnType env, Nothing)

createStringFromArgs :: [Arg] -> String
createStringFromArgs [] = ""
createStringFromArgs (head : tail) = show head ++ createStringFromArgs tail

argExistsInArgs :: Arg -> [Arg] -> Bool
argExistsInArgs (Arg t1 (Ident i1)) ((Arg t2 (Ident i2)) : tail) = if i1 == i2 then True else argExistsInArgs (Arg t1 (Ident i1)) tail
argExistsInArgs t [] = False

checkIfFunctionHaveDifferentIdentifiers :: [Arg] -> Bool
checkIfFunctionHaveDifferentIdentifiers [] = True
checkIfFunctionHaveDifferentIdentifiers (head : tail) = if argExistsInArgs head tail then False else checkIfFunctionHaveDifferentIdentifiers tail

checkIfStmtReturnsIncorrectType :: Stmt -> Type -> TC (TCEnv)
checkIfStmtReturnsIncorrectType VRet Void = do
  env <- ask
  return (env)
checkIfStmtReturnsIncorrectType VRet t = throwError $ MismatchReturnFunctionType t Void
checkIfStmtReturnsIncorrectType (Ret stm) typ = do
  t <- prepareExprType stm
  if typ == t
    then do
      env <- ask
      return (env)
    else throwError $ MismatchReturnFunctionType typ t
checkIfStmtReturnsIncorrectType (Cond e s) typ = checkIfStmtReturnsIncorrectType s typ
checkIfStmtReturnsIncorrectType (CondElse e s1 s2) typ = do
  checkIfStmtReturnsIncorrectType s1 typ
  checkIfStmtReturnsIncorrectType s2 typ
  env <- ask
  return (env)
checkIfStmtReturnsIncorrectType (While e s) typ = checkIfStmtReturnsIncorrectType s typ
checkIfStmtReturnsIncorrectType _ _ = do
  env <- ask
  return (env)

checkIfStmtsReturnsIncorrectType :: [Stmt] -> Type -> TC (TCEnv)
checkIfStmtsReturnsIncorrectType (head : tail) t = do
  checkIfStmtReturnsIncorrectType head t
  checkIfStmtsReturnsIncorrectType tail t
  env <- ask
  return (env)
checkIfStmtsReturnsIncorrectType _ _ = do
  env <- ask
  return (env)

checkIfFunctionMismatchRetType :: Block -> Type -> TC (TCEnv)
checkIfFunctionMismatchRetType (Block []) t = checkIfStmtsReturnsIncorrectType [] t
checkIfFunctionMismatchRetType (Block stmts) t = checkIfStmtsReturnsIncorrectType stmts t

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

-- checkStatementTypeProgram :: [TopDef] -> TC (TCEnv, TCRes)
-- checkStatementTypeProgram (fn : rest) = do
--   let (FnDef typ identifier args block) = fn
--   if typ /= Void && checkIfFunctionReturn block == False
--     then throwError $ FunctionNotReturnException identifier
--     else
--       if checkIfFunctionHaveDifferentIdentifiers args == False
--         then throwError $ DoubleIdentifierInFunctionDeclarationException (show identifier)
--         else do
--           let (Block stmts) = block
--           (env, ret) <- addFunctionDeclarationWithoutAddingArgs fn
--           (envWith, retWith) <- addFunctionDeclaration fn
--           if isNothing ret
--             then do
--               (env2, ret2) <- local (const envWith) (checkStatementTypeForMany stmts)
--               local (const env2) (checkIfFunctionMismatchRetType block typ)
--               local (const env) (checkStatementTypeProgram rest)
--             else return (env, ret)

checkStatementTypeProgram :: [TopDef] -> [TopDef] -> TC (TCEnv, TCRes)
checkStatementTypeProgram (fn : rest) functions = do
  let (FnDef typ identifier args block) = fn
  if typ /= Void && checkIfFunctionReturn block == False
    then throwError $ FunctionNotReturnException identifier
    else
      if checkIfFunctionHaveDifferentIdentifiers args == False
        then throwError $ DoubleIdentifierInFunctionDeclarationException (show identifier)
        else do
          let (Block stmts) = block
          env <- ask
          (envWith, retWith) <- addFunctionDeclaration fn
          if isNothing retWith
            then do
              (env2, ret2) <- local (const envWith) (checkStatementTypeForMany stmts functions)
              local (const env2) (checkIfFunctionMismatchRetType block typ)
              local (const env) (checkStatementTypeProgram rest functions)
            else return (env, retWith)
checkStatementTypeProgram [] _ = do
  env <- ask
  return (env, Nothing)

checkProgram :: [TopDef] -> TC (TCEnv, TCRes)
checkProgram functions = do
  (env, ret) <- addFunctionDeclarations functions
  (env2, ret2) <- local (const env) (addPrintIntDeclaration functions)
  (env3, ret3) <- local (const env2) (addPrintStringDeclaration functions)
  (env35, ret35) <- local (const env3) (addReadIntDeclaration functions)
  (env375, ret375) <- local (const env35) (addReadStringDeclaration functions)
  (env4, ret4) <- local (const env375) (addPrintBoolDeclaration functions)
  (env5, ret5) <- local (const env4) (checkStatementTypeProgram functions functions)
  return (env5, ret5)
