module TypeCheckHelpers where

import AbsGrammar (Arg (..), Ident (..), Type (..))
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Data.Map (Map, lookup)

data TypeCheckExceptions = InvalidTypeInDeclarationException Type | TypeCheckException Type Type | FuncApplicationException | DoubleIdentifierInFunctionDeclarationException String | DoubleInitializationException String | MismatchReturnFunctionType Type Type | FunctionNotReturnException Ident | NonexistingIdentifierException String deriving (Show)

type TCEnv = (Data.Map.Map String Type)

type TC = ReaderT TCEnv (ExceptT TypeCheckExceptions IO)

type TCRes = Maybe Type

evalCorrectFunction :: Type -> Maybe Type
evalCorrectFunction (Fun ret args) = Just (Fun ret args)
evalCorrectFunction typ = Nothing

getTypeFromEnv :: Ident -> TC Type
getTypeFromEnv (Ident ident) = do
  env <- ask
  case Data.Map.lookup ident env of
    Nothing -> throwError $ NonexistingIdentifierException ident
    Just typ -> return typ

isCorrectStmtType :: Type -> TC Bool
isCorrectStmtType Void = return False
isCorrectStmtType (Fun typ types) = return False
isCorrectStmtType _ = return True

isConstType :: Type -> TC Bool
isConstType _ = return False

getStableTypeForConst :: Type -> TC Type
getStableTypeForConst t = return t

getTypesFromArgs :: [Arg] -> TC [Type]
getTypesFromArgs [] = do
  return $ []
getTypesFromArgs [arg] = do
  let (Arg typ ident) = arg
  return $ [typ]
getTypesFromArgs (arg : rest) = do
  let (Arg typ ident) = arg
  restTypes <- getTypesFromArgs rest
  return $ (typ : restTypes)
