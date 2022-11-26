module CompilerHelpers where

import AbsGrammar (Stmt, Type (Bool, Int, Str))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Map (Map)

type ReturnResult = Maybe ValueInMemory

type FArg = (String, Type)

type Function = ([Stmt], IIEnv, [FArg], Type)

type Class = (Data.Map.Map String ValueInMemory)

data ValueInMemory = IntValue Integer | BooleanValue Bool | StringValue String | FunctionValue Function | ClassValue Class

type IIEnv = (Data.Map.Map String ValueInMemory)

type II = ReaderT IIEnv (ExceptT RuntimeExceptions IO)

data RuntimeExceptions = DivisionByZeroException | ModulusByZeroException | NoReturnException | OutOfRangeExeption Integer | UnitializedException String deriving (Show)

makeArrayString :: [ValueInMemory] -> String
makeArrayString [] = ""
makeArrayString [a] = makeString a
makeArrayString (a : tail) = makeString a ++ ", " ++ makeArrayString tail

makeString :: ValueInMemory -> String
makeString (IntValue i) = show i
makeString (BooleanValue b) = show b
makeString (StringValue s) = s
makeString (FunctionValue (stmt, env, arg, typ)) = "Type: " ++ show typ ++ "Args: " ++ show arg

defaultValueOfType :: Type -> II ValueInMemory
defaultValueOfType Bool = return $ (BooleanValue False)
defaultValueOfType Int = return $ (IntValue 0)
defaultValueOfType Str = return $ (StringValue "")

createEmptyList :: Int -> Type -> II [ValueInMemory]
createEmptyList 0 _ = return $ []
createEmptyList 1 typ = do
  defType <- defaultValueOfType typ
  return $ [defType]
createEmptyList n typ = do
  defType <- defaultValueOfType typ
  tail <- createEmptyList (n - 1) typ
  return $ defType : tail
