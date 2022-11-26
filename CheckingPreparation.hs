module CheckingPreparation where

import AbsGrammar (ClsDef (FunDef, VarDef), Expr (..), Ident (Ident), Type (Bool, Class, ClassIntern, Fun, Int, Str, Void))
import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.Reader (unless)
import Data.Maybe (isNothing)
import TypeCheckHelpers (TC, TypeCheckExceptions (FuncApplicationException, NoClassException, NotAClassException, NotExistedIdentInClassException, TypeCheckException), evalCorrectClass, evalCorrectFunction, getStableTypeForConst, getTypeFromEnv, getTypesFromArgs)

checkExprArray :: [Expr] -> Type -> Int -> TC ()
checkExprArray [] typ depth =
  prepareCheckType typ typ
checkExprArray (expr : rest) typ depth = do
  prepareCheckTypeExpr expr typ depth
  checkExprArray rest typ depth

prepareCheckType :: Type -> Type -> TC ()
prepareCheckType a b = do
  stableA <- getStableTypeForConst a
  stableB <- getStableTypeForConst b
  unless (stableA == stableB) $ throwError $ TypeCheckException a b

prepareCheckArgsTypes :: [Type] -> [Expr] -> Int -> TC ()
prepareCheckArgsTypes [] [] depth = return ()
prepareCheckArgsTypes (typ : restTypes) (expr : vars) depth = do
  prepareCheckTypeExpr expr typ depth
  prepareCheckArgsTypes restTypes vars depth
prepareCheckArgsTypes a b depth = throwError FuncApplicationException

prepareCheckTypeExpr :: Expr -> Type -> Int -> TC ()
prepareCheckTypeExpr e t depth = do
  typ <- prepareExprType e depth
  prepareCheckType typ t

checkIfIsClass :: Type -> Maybe Type
checkIfIsClass (Class ident) = Just (Class ident)
checkIfIsClass _ = Nothing

prepareExprType :: Expr -> Int -> TC Type
prepareExprType (EVar ident) depth = getTypeFromEnv ident depth
prepareExprType (ELitInt {}) depth = return Int
prepareExprType (EAdd e1 op e2) depth = do
  e1typ <- prepareExprType e1 depth
  e2typ <- prepareExprType e2 depth
  if e1typ == Str && e2typ == Str
    then do
      prepareCheckTypeExpr e1 Str depth
      prepareCheckTypeExpr e2 Str depth
      return Str
    else do
      prepareCheckTypeExpr e1 Int depth
      prepareCheckTypeExpr e2 Int depth
      return Int
prepareExprType (EMul e1 op e2) depth = do
  prepareCheckTypeExpr e1 Int depth
  prepareCheckTypeExpr e2 Int depth
  return Int
prepareExprType (EAnd e1 e2) depth = do
  prepareCheckTypeExpr e1 Bool depth
  prepareCheckTypeExpr e2 Bool depth
  return Bool
prepareExprType (EOr e1 e2) depth = do
  prepareCheckTypeExpr e1 Bool depth
  prepareCheckTypeExpr e2 Bool depth
  return Bool
prepareExprType (Neg e) depth = do
  prepareCheckTypeExpr e Int depth
  return Int
prepareExprType ELitFalse depth = return Bool
prepareExprType ELitTrue depth = return Bool
prepareExprType (ERel e1 op e2) depth = do
  typ1 <- prepareExprType e1 depth
  typ2 <- prepareExprType e2 depth
  prepareCheckType typ1 typ2
  return Bool
prepareExprType (Not e) depth = do
  prepareCheckTypeExpr e Bool depth
  return Bool
prepareExprType (EString e) depth = return Str
prepareExprType (EApp identifier args) depth = do
  typ <- getTypeFromEnv identifier depth

  let functionEvaluation = evalCorrectFunction typ

  if isNothing functionEvaluation
    then throwError $ FuncApplicationException
    else do
      let (Fun returnedType types) = typ
      prepareCheckArgsTypes types args depth
      return $ returnedType
prepareExprType (ESelect expr ident) depth = do
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
          let foundType = findVarIdentInClass justClassEvaluation ident
          if (isNothing foundType)
            then throwError $ NotExistedIdentInClassException ident
            else do
              let (Just res) = foundType
              return res
prepareExprType (EFunCall expr ident exprs) depth = do
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
              return typfn
prepareExprType (EClass ident) depth = do
  typ <- getTypeFromEnv ident depth
  let classEvaluation = evalCorrectClass typ

  if isNothing classEvaluation
    then throwError $ NoClassException ident
    else do
      let Just (ClassIntern classIdent extendIdent clsDefs) = classEvaluation
      return (Class classIdent)

findVarIdentInClass :: Type -> Ident -> Maybe Type
findVarIdentInClass (ClassIntern classIdent extendIdent clsdefs) ident = findVarIdentInClsDefs clsdefs ident
findVarIdentInClass _ _ = Nothing

findVarIdentInClsDefs :: [ClsDef] -> Ident -> Maybe Type
findVarIdentInClsDefs ((FunDef typ (Ident identifier) args block) : rest) (Ident funident) =
  if identifier == funident then Nothing else findVarIdentInClsDefs rest (Ident funident)
findVarIdentInClsDefs ((VarDef typ []) : rest) ident = findVarIdentInClsDefs rest ident
findVarIdentInClsDefs ((VarDef typ ((Ident identifier) : restIdents)) : rest) (Ident varident) =
  if varident == identifier then Just typ else findVarIdentInClsDefs (VarDef typ restIdents : rest) (Ident varident)
findVarIdentInClsDefs _ _ = Nothing

findFunctionIdentInClass :: Type -> Ident -> Maybe ClsDef
findFunctionIdentInClass (ClassIntern classIdent extendIdent clsdefs) ident = findFunctionIdentInClsDefs clsdefs ident
findFunctionIdentInClass _ _ = Nothing

findFunctionIdentInClsDefs :: [ClsDef] -> Ident -> Maybe ClsDef
findFunctionIdentInClsDefs ((FunDef typ (Ident identifier) args block) : rest) (Ident funident) =
  if identifier == funident then Just (FunDef typ (Ident identifier) args block) else findFunctionIdentInClsDefs rest (Ident funident)
findFunctionIdentInClsDefs ((VarDef typ []) : rest) ident = findFunctionIdentInClsDefs rest ident
findFunctionIdentInClsDefs ((VarDef typ ((Ident identifier) : restIdents)) : rest) (Ident varident) =
  if varident == identifier then Nothing else findFunctionIdentInClsDefs (VarDef typ restIdents : rest) (Ident varident)
findFunctionIdentInClsDefs _ _ = Nothing
