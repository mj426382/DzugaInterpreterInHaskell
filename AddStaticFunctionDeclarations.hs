module AddStaticFunctionDeclarations where

import AbsGrammar (ClsDef (FunDef, VarDef), Ident (..), TopDef (ExtClsDef, FnDef, TopClsDef), Type (Bool, Class, ClassIntern, Fun, Int, Str, Void))
import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Reader.Class (MonadReader (local))
import qualified Data.Map
import TypeCheckHelpers (Class, TC, TCEnv, TCRes, TypeCheckExceptions (NoClassException), getTypesFromArgs, getTypesFromArgsWithoutMonad)

addFunctionDeclarationWithoutAddingArgs :: TopDef -> TC (TCEnv, TCRes)
addFunctionDeclarationWithoutAddingArgs (FnDef typ (Ident identifier) args block) = do
  argTypes <- getTypesFromArgs args
  let fnType = (Fun typ argTypes)
  env <- ask
  return (Data.Map.insert (identifier ++ "_0") fnType env, Nothing)
addFunctionDeclarationWithoutAddingArgs (TopClsDef (Ident identifier) clsdefs) = do
  env <- ask
  return (Data.Map.insert (identifier ++ "_0") (ClassIntern (Ident identifier) (Ident "---") clsdefs) env, Nothing)
addFunctionDeclarationWithoutAddingArgs (ExtClsDef (Ident identifier1) ident2 clsdefs) = do
  let (Ident identifier2) = ident2
  if identifier1 == identifier2
    then throwError $ NoClassException ident2
    else do
      env <- ask
      return (Data.Map.insert (identifier1 ++ "_0") (ClassIntern (Ident identifier1) ident2 clsdefs) env, Nothing)

prepareClassEntities :: [ClsDef] -> Class -> Class
prepareClassEntities ((FunDef typ (Ident identifier) args block) : rest) cla = do
  let argTypes = getTypesFromArgsWithoutMonad args
  let restClass = prepareClassEntities rest cla
  Data.Map.insert (identifier ++ "_0") (Fun typ argTypes) restClass
prepareClassEntities ((VarDef typ []) : rest) cla = prepareClassEntities rest cla
prepareClassEntities ((VarDef typ ((Ident identifier) : restIdents)) : rest) cla = do
  let restClass = prepareClassEntities ((VarDef typ restIdents) : rest) cla
  Data.Map.insert (identifier ++ "_0") typ restClass
prepareClassEntities [] cla = cla

addFunctionDeclarations :: [TopDef] -> TC (TCEnv, TCRes)
addFunctionDeclarations (fn : rest) = do
  (env, ret) <- addFunctionDeclarationWithoutAddingArgs fn
  (env2, ret2) <- local (const env) (addFunctionDeclarations rest)
  return (env2, ret2)
addFunctionDeclarations [] = do
  env <- ask
  return (env, Nothing)

addPrintIntDeclaration :: [TopDef] -> TC (TCEnv, TCRes)
addPrintIntDeclaration functions = do
  env <- ask
  return (Data.Map.insert "printInt_0" (Fun Void [Int]) env, Nothing)

addPrintStringDeclaration :: [TopDef] -> TC (TCEnv, TCRes)
addPrintStringDeclaration functions = do
  env <- ask
  return (Data.Map.insert "printString_0" (Fun Void [Str]) env, Nothing)

addPrintBoolDeclaration :: [TopDef] -> TC (TCEnv, TCRes)
addPrintBoolDeclaration functions = do
  env <- ask
  return (Data.Map.insert "printBool_0" (Fun Void [Bool]) env, Nothing)

addReadIntDeclaration :: [TopDef] -> TC (TCEnv, TCRes)
addReadIntDeclaration functions = do
  env <- ask
  return (Data.Map.insert "readInt_0" (Fun Int []) env, Nothing)

addReadStringDeclaration :: [TopDef] -> TC (TCEnv, TCRes)
addReadStringDeclaration functions = do
  env <- ask
  return (Data.Map.insert "readString_0" (Fun Str []) env, Nothing)