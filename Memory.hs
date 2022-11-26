module Memory where

import AbsGrammar (Ident (..))
import CompilerHelpers (Function, II, IIEnv, RuntimeExceptions (UnitializedException), ValueInMemory (FunctionValue, IntValue))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask))
import Data.Map (insert, lookup)

addVarToMemory :: Ident -> ValueInMemory -> II IIEnv
addVarToMemory (Ident identifier) memVal = do
  env <- ask
  return (Data.Map.insert identifier memVal env)

readFromMemory :: Ident -> II ValueInMemory
readFromMemory (Ident identifier) = do
  env <- ask
  case Data.Map.lookup identifier env of
    Just memVal -> return memVal
    Nothing -> do
      throwError $ UnitializedException identifier
      return (IntValue 0)

readIntFromMemory :: Ident -> II Integer
readIntFromMemory ident = do
  IntValue integer <- readFromMemory ident
  return integer

readFunFromMemory :: Ident -> II Function
readFunFromMemory ident = do
  FunctionValue fun <- readFromMemory ident
  return fun
