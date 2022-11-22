module AddStaticFunctionDeclarations where

import AbsGrammar (Ident (..), TopDef (FnDef), Type (Bool, Fun, Int, Str, Void))
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Reader.Class (MonadReader (local))
import qualified Data.Map
import TypeCheckHelpers (TC, TCEnv, TCRes, getTypesFromArgs)

addFunctionDeclarationWithoutAddingArgs :: TopDef -> TC (TCEnv, TCRes)
addFunctionDeclarationWithoutAddingArgs (FnDef typ (Ident identifier) args block) = do
  argTypes <- getTypesFromArgs args
  let fnType = (Fun typ argTypes)
  env <- ask
  return (Data.Map.insert identifier fnType env, Nothing)

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
  return (Data.Map.insert "printInt" (Fun Void [Int]) env, Nothing)

addPrintStringDeclaration :: [TopDef] -> TC (TCEnv, TCRes)
addPrintStringDeclaration functions = do
  env <- ask
  return (Data.Map.insert "printString" (Fun Void [Str]) env, Nothing)

addPrintBoolDeclaration :: [TopDef] -> TC (TCEnv, TCRes)
addPrintBoolDeclaration functions = do
  env <- ask
  return (Data.Map.insert "printBool" (Fun Void [Bool]) env, Nothing)

addReadIntDeclaration :: [TopDef] -> TC (TCEnv, TCRes)
addReadIntDeclaration functions = do
  env <- ask
  return (Data.Map.insert "readInt" (Fun Int []) env, Nothing)

addReadStringDeclaration :: [TopDef] -> TC (TCEnv, TCRes)
addReadStringDeclaration functions = do
  env <- ask
  return (Data.Map.insert "readString" (Fun Str []) env, Nothing)