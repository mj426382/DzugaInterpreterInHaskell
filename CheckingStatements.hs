module CheckingStatements where

import AbsGrammar (Block (Block), Expr (EApp), Ident (Ident), Item (Init, NoInit), Stmt (Ass, BStmt, Cond, CondElse, Decl, Decr, Incr, SExp, While), TopDef, Type (Bool, Fun, Int))
import AddStaticFunctionDeclarations
import CheckingPreparation (prepareCheckArgsTypes, prepareCheckType, prepareCheckTypeExpr, prepareExprType)
import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.Reader (MonadReader (ask, local), unless)
import Data.Map (empty, insert, lookup)
import Data.Maybe (isNothing)
import TypeCheckHelpers (TC, TCEnv, TCRes, TypeCheckExceptions (DoubleIdentifierInFunctionDeclarationException, DoubleInitializationException, FuncApplicationException, InvalidTypeInDeclarationException), evalCorrectFunction, getStableTypeForConst, getTypeFromEnv, isConstType, isCorrectStmtType)

checkStatementType :: Stmt -> [TopDef] -> TC (TCEnv, TCRes)
checkStatementType (Decl typ [(NoInit (Ident identifier))]) functions = do
  isCorrect <- isCorrectStmtType typ
  isConst <- isConstType typ
  unless isCorrect $ throwError $ InvalidTypeInDeclarationException typ
  env <- ask
  case Data.Map.lookup identifier env of
    Nothing -> return (Data.Map.insert identifier typ env, Nothing)
    Just typ -> throwError $ DoubleInitializationException identifier
checkStatementType (Decl typ [(Init (Ident identifier) expr)]) functions = do
  isCorrect <- isCorrectStmtType typ
  unless isCorrect $ throwError $ InvalidTypeInDeclarationException typ
  stableType <- getStableTypeForConst typ
  prepareCheckTypeExpr expr stableType
  env <- ask
  case Data.Map.lookup identifier env of
    Nothing -> return (Data.Map.insert identifier typ env, Nothing)
    Just typ -> throwError $ DoubleInitializationException identifier
checkStatementType (Decl typ (item : rest)) functions = do
  (env, ret) <- checkStatementType (Decl typ [item]) functions
  local (const env) (checkStatementType (Decl typ rest) functions)
checkStatementType (While expr stm) functions = do
  prepareCheckTypeExpr expr Bool
  checkStatementType stm functions
checkStatementType (Cond expr stmt) functions = do
  prepareCheckTypeExpr expr Bool
  checkStatementType stmt functions
checkStatementType (CondElse expr stmt1 stmt2) functions = do
  prepareCheckTypeExpr expr Bool
  checkStatementType stmt1 functions
  checkStatementType stmt2 functions
-- checkStatementType (BStmt block) = do
--   let (Block blocks) = block
--   if (length blocks == 0)
--     then do
--       env <- ask
--       return (env, Nothing)
--     else do
--       let (Block (stm : rest)) = block
--       (env, ret) <- checkStatementType stm
--       local (const env) (checkStatementType (BStmt (Block rest)))
checkStatementType (Ass identifier expr) functions = do
  typ <- getTypeFromEnv identifier
  isConst <- isConstType typ
  exprType <- prepareExprType expr
  prepareCheckTypeExpr expr typ
  env <- ask
  return (env, Nothing)
checkStatementType (Incr identifier) functions = do
  typ <- getTypeFromEnv identifier
  prepareCheckType typ Int
  env <- ask
  return (env, Nothing)
checkStatementType (Decr identifier) functions = do
  typ <- getTypeFromEnv identifier
  prepareCheckType typ Int
  env <- ask
  return (env, Nothing)
checkStatementType (BStmt (Block stmts)) functions = do
  env <- ask
  (env1, ret1) <- local (const Data.Map.empty) (addFunctionDeclarations functions)
  (env2, ret2) <- local (const env1) (addPrintIntDeclaration functions)
  (env3, ret3) <- local (const env2) (addPrintStringDeclaration functions)
  (env35, ret35) <- local (const env3) (addReadIntDeclaration functions)
  (env375, ret375) <- local (const env35) (addReadStringDeclaration functions)
  (env4, ret4) <- local (const env375) (addPrintBoolDeclaration functions)
  local (const env4) (checkStatementTypeForMany stmts functions)
  return (env, Nothing)
checkStatementType (SExp (EApp identifier args)) functions = do
  typ <- getTypeFromEnv identifier
  let functionEvaluation = evalCorrectFunction typ
  if isNothing functionEvaluation
    then throwError $ FuncApplicationException
    else do
      let (Fun returnedType types) = typ
      prepareCheckArgsTypes types args
      env <- ask
      return (env, Nothing)
-- typ <- getTypeFromEnv identifier
-- prepareCheckArgsTypes exprs
-- env <- ask
-- return (env, Nothing)
checkStatementType _ _ = do
  env <- ask
  return (env, Nothing)

checkStatementTypeForMany :: [Stmt] -> [TopDef] -> TC (TCEnv, TCRes)
checkStatementTypeForMany (stm : rest) functions = do
  (env, ret) <- checkStatementType stm functions
  -- throwError $ DoubleIdentifierInFunctionDeclarationException (show env ++ show ret ++ show (stm : rest))
  if isNothing ret
    then local (const env) (checkStatementTypeForMany rest functions)
    else return (env, ret)
checkStatementTypeForMany [] functions = do
  env <- ask
  return (env, Nothing)
