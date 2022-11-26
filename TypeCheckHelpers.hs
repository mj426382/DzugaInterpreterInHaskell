module TypeCheckHelpers where

import AbsGrammar (Arg (..), Ident (..), Type (..))
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Data.Map (Map, lookup)

data TypeCheckExceptions = InvalidTypeInDeclarationException Type | TypeCheckException Type Type | FuncApplicationException | NoClassException Ident | NotAClassException | NotExistedIdentInClassException Ident | DoubleIdentifierInFunctionDeclarationException String | DoubleInitializationException String | MismatchReturnFunctionType Type Type | FunctionNotReturnException Ident | NonexistingIdentifierException String deriving (Show)

type TCEnv = (Data.Map.Map String Type)

type TC = ReaderT TCEnv (ExceptT TypeCheckExceptions IO)

type TCRes = Maybe Type

type Class = (Data.Map.Map String Type)

evalCorrectFunction :: Type -> Maybe Type
evalCorrectFunction (Fun ret args) = Just (Fun ret args)
evalCorrectFunction typ = Nothing

evalCorrectClass :: Type -> Maybe Type
evalCorrectClass (ClassIntern ident1 ident2 clsDefs) = Just (ClassIntern ident1 ident2 clsDefs)
evalCorrectClass typ = Nothing

getTypeFromEnv :: Ident -> Int -> TC Type
getTypeFromEnv (Ident ident) 0 = do
  env <- ask
  case Data.Map.lookup (ident ++ "_" ++ show 0) env of
    Nothing -> throwError $ NonexistingIdentifierException ident
    Just typ -> return typ
getTypeFromEnv (Ident ident) depth = do
  env <- ask
  case Data.Map.lookup (ident ++ "_" ++ show depth) env of
    Nothing -> getTypeFromEnv (Ident ident) (depth - 1)
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

getTypesFromArgsWithoutMonad :: [Arg] -> [Type]
getTypesFromArgsWithoutMonad [] = []
getTypesFromArgsWithoutMonad [Arg typ ident] = [typ]
getTypesFromArgsWithoutMonad ((Arg typ ident) : rest) = typ : getTypesFromArgsWithoutMonad rest
