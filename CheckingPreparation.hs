module CheckingPreparation where

import AbsGrammar (Expr (..), Type (Bool, Fun, Int, Str, Void))
import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.Reader (unless)
import Data.Maybe (isNothing)
import TypeCheckHelpers (TC, TypeCheckExceptions (FuncApplicationException, TypeCheckException), evalCorrectFunction, getStableTypeForConst, getTypeFromEnv)

checkExprArray :: [Expr] -> Type -> TC ()
checkExprArray [] typ =
  prepareCheckType typ typ
checkExprArray (expr : rest) typ = do
  prepareCheckTypeExpr expr typ
  checkExprArray rest typ

prepareCheckType :: Type -> Type -> TC ()
prepareCheckType a b = do
  stableA <- getStableTypeForConst a
  stableB <- getStableTypeForConst b
  unless (stableA == stableB) $ throwError $ TypeCheckException a b

prepareCheckArgsTypes :: [Type] -> [Expr] -> TC ()
prepareCheckArgsTypes [] [] = return ()
prepareCheckArgsTypes (typ : restTypes) (expr : vars) = do
  prepareCheckTypeExpr expr typ
  prepareCheckArgsTypes restTypes vars
prepareCheckArgsTypes a b = throwError FuncApplicationException

prepareCheckTypeExpr :: Expr -> Type -> TC ()
prepareCheckTypeExpr e t = do
  typ <- prepareExprType e
  prepareCheckType typ t

prepareExprType :: Expr -> TC Type
prepareExprType (EVar ident) = getTypeFromEnv ident
prepareExprType ELitInt {} = return Int
prepareExprType (EAdd e1 op e2) = do
  e1typ <- prepareExprType e1
  e2typ <- prepareExprType e2
  if e1typ == Str && e2typ == Str
    then do
      prepareCheckTypeExpr e1 Str
      prepareCheckTypeExpr e2 Str
      return Str
    else do
      prepareCheckTypeExpr e1 Int
      prepareCheckTypeExpr e2 Int
      return Int
prepareExprType (EMul e1 op e2) = do
  prepareCheckTypeExpr e1 Int
  prepareCheckTypeExpr e2 Int
  return Int
prepareExprType (EAnd e1 e2) = do
  prepareCheckTypeExpr e1 Bool
  prepareCheckTypeExpr e2 Bool
  return Bool
prepareExprType (EOr e1 e2) = do
  prepareCheckTypeExpr e1 Bool
  prepareCheckTypeExpr e2 Bool
  return Bool
prepareExprType (Neg e) = do
  prepareCheckTypeExpr e Int
  return Int
prepareExprType ELitFalse = return Bool
prepareExprType ELitTrue = return Bool
prepareExprType (ERel e1 op e2) = do
  typ1 <- prepareExprType e1
  typ2 <- prepareExprType e2
  prepareCheckType typ1 typ2
  -- prepareCheckTypeExpr e1 Int
  -- prepareCheckTypeExpr e2 Int
  return Bool
prepareExprType (Not e) = do
  prepareCheckTypeExpr e Bool
  return Bool
prepareExprType (EString e) = return Str
prepareExprType (EApp identifier args) = do
  typ <- getTypeFromEnv identifier

  let functionEvaluation = evalCorrectFunction typ

  if isNothing functionEvaluation
    then throwError $ FuncApplicationException
    else do
      let (Fun returnedType types) = typ
      prepareCheckArgsTypes types args
      return $ returnedType
