module CheckingStatements where

import AbsGrammar (Block (Block), ClsDef (FunDef), Expr (EApp, EFunCall, ESelect), Ident (Ident), Item (Init, NoInit), Stmt (Ass, BStmt, Cond, CondElse, Decl, Decr, Incr, SExp, While), TopDef, Type (Bool, Class, Fun, Int))
import AddStaticFunctionDeclarations
import CheckingPreparation (checkIfIsClass, findFunctionIdentInClass, prepareCheckArgsTypes, prepareCheckType, prepareCheckTypeExpr, prepareExprType)
import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.Reader (MonadReader (ask, local), unless)
import Data.Map (empty, insert, lookup)
import Data.Maybe (isNothing)
import TypeCheckHelpers (TC, TCEnv, TCRes, TypeCheckExceptions (DoubleIdentifierInFunctionDeclarationException, DoubleInitializationException, FuncApplicationException, InvalidTypeInDeclarationException, NoClassException, NotAClassException, NotExistedIdentInClassException), evalCorrectClass, evalCorrectFunction, getStableTypeForConst, getTypeFromEnv, getTypesFromArgs, isConstType, isCorrectStmtType)

checkStatementType :: Stmt -> Int -> TC (TCEnv, TCRes)
checkStatementType (Decl typ [(NoInit (Ident identifier))]) depth = do
  isCorrect <- isCorrectStmtType typ
  isConst <- isConstType typ
  unless isCorrect $ throwError $ InvalidTypeInDeclarationException typ
  env <- ask
  case Data.Map.lookup (identifier ++ "_" ++ show depth) env of
    Nothing -> return (Data.Map.insert (identifier ++ "_" ++ show depth) typ env, Nothing)
    Just typ -> throwError $ DoubleInitializationException identifier
checkStatementType (Decl typ [(Init (Ident identifier) expr)]) depth = do
  isCorrect <- isCorrectStmtType typ
  unless isCorrect $ throwError $ InvalidTypeInDeclarationException typ
  stableType <- getStableTypeForConst typ
  prepareCheckTypeExpr expr stableType depth
  env <- ask
  case Data.Map.lookup (identifier ++ "_" ++ show depth) env of
    Nothing -> return (Data.Map.insert (identifier ++ "_" ++ show depth) typ env, Nothing)
    Just typ -> throwError $ DoubleInitializationException identifier
checkStatementType (Decl typ (item : rest)) depth = do
  (env, ret) <- checkStatementType (Decl typ [item]) depth
  local (const env) (checkStatementType (Decl typ rest) depth)
checkStatementType (While expr stm) depth = do
  prepareCheckTypeExpr expr Bool depth
  checkStatementType stm (depth + 1)
checkStatementType (Cond expr stmt) depth = do
  prepareCheckTypeExpr expr Bool depth
  checkStatementType stmt (depth + 1)
checkStatementType (CondElse expr stmt1 stmt2) depth = do
  prepareCheckTypeExpr expr Bool depth
  checkStatementType stmt1 (depth + 1)
  checkStatementType stmt2 (depth + 1)
checkStatementType (Ass identifier expr) depth = do
  typ <- getTypeFromEnv identifier depth
  isConst <- isConstType typ
  exprType <- prepareExprType expr depth
  prepareCheckTypeExpr expr typ depth
  env <- ask
  return (env, Nothing)
checkStatementType (Incr identifier) depth = do
  typ <- getTypeFromEnv identifier depth
  prepareCheckType typ Int
  env <- ask
  return (env, Nothing)
checkStatementType (Decr identifier) depth = do
  typ <- getTypeFromEnv identifier depth
  prepareCheckType typ Int
  env <- ask
  return (env, Nothing)
checkStatementType (BStmt (Block stmts)) depth = do
  checkStatementTypeForMany stmts (depth + 1)
  env <- ask
  return (env, Nothing)
checkStatementType (SExp (EApp identifier args)) depth = do
  typ <- getTypeFromEnv identifier depth
  let functionEvaluation = evalCorrectFunction typ
  if isNothing functionEvaluation
    then throwError $ FuncApplicationException
    else do
      let (Fun returnedType types) = typ
      prepareCheckArgsTypes types args depth
      env <- ask
      return (env, Nothing)
checkStatementType (SExp (EFunCall expr ident exprs)) depth = do
  typ <- prepareExprType expr depth
  if isNothing (checkIfIsClass typ)
    then throwError $ NotAClassException
    else do
      let (Class ident2) = typ
      typ2 <- getTypeFromEnv ident2 depth
      let classEvaluation = evalCorrectClass typ2

      if isNothing classEvaluation
        then throwError $ NoClassException ident2
        else do
          let (Just justClassEvaluation) = classEvaluation
          let foundType = findFunctionIdentInClass justClassEvaluation ident
          if isNothing foundType
            then throwError $ NotExistedIdentInClassException ident
            else do
              let (Just (FunDef typfn identfn fnargs block)) = foundType
              argTypes <- getTypesFromArgs fnargs
              prepareCheckArgsTypes argTypes exprs depth
              env <- ask
              return (env, Nothing)
checkStatementType _ _ = do
  env <- ask
  return (env, Nothing)

checkStatementTypeForMany :: [Stmt] -> Int -> TC (TCEnv, TCRes)
checkStatementTypeForMany (stm : rest) depth = do
  (env, ret) <- checkStatementType stm depth
  if isNothing ret
    then local (const env) (checkStatementTypeForMany rest depth)
    else return (env, ret)
checkStatementTypeForMany [] depth = do
  env <- ask
  return (env, Nothing)
