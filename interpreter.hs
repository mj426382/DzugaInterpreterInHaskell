module Main where

import AbsGrammar (Program (Program))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Map (empty)
import DzugaInterpreter (interpretProgram)
import DzugaInterpreterHelpers (RuntimeExceptions (DivisionByZeroException, ModulusByZeroException, NoReturnException, OutOfRangeExeption, UnitializedException))
import ErrM
import ParGrammar (myLexer, pProgram)
import StaticTypeChecker (checkProgram, checkStatementTypeProgram)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec.Token (GenTokenParser (identifier))
import TypeCheckHelpers (TypeCheckExceptions (DoubleIdentifierInFunctionDeclarationException, DoubleInitializationException, FuncApplicationException, FunctionNotReturnException, InvalidTypeInDeclarationException, MismatchReturnFunctionType, NonexistingIdentifierException, TypeCheckException))

returnError :: String -> IO ()
returnError msg = do
  putStr msg
  exitFailure

readCode :: String -> IO ()
readCode fileName = readFile fileName >>= parse

parse :: String -> IO ()
parse input =
  case pProgram (myLexer input) of
    (Ok s) -> do
      let Program program = s
      typeCheckResult <- runExceptT $ runReaderT (checkProgram program) Data.Map.empty
      case typeCheckResult of
        Left err -> do
          putStr ("Typecheck err: ")
          case err of
            TypeCheckException given expected -> returnError $ "Expected " ++ show expected ++ " given: " ++ show given
            FuncApplicationException -> returnError "Invalid function argument application"
            NonexistingIdentifierException identifier -> returnError $ "Identifier " ++ identifier ++ " doesn't exist"
            InvalidTypeInDeclarationException typ -> returnError $ "Invalid use of type " ++ show typ ++ " in declaration"
            DoubleIdentifierInFunctionDeclarationException identifier -> returnError $ "Double idenftifier in function " ++ identifier ++ " declaration"
            DoubleInitializationException identifier -> returnError $ "Double intialize " ++ identifier
            MismatchReturnFunctionType t1 t2 -> returnError $ "Mismatch return type exception: expected " ++ show t2 ++ " get " ++ show t1
            FunctionNotReturnException ident -> returnError $ "Non void function " ++ show ident ++ "does not return value"
        Right _ -> do
          putStr "Correct"
          return ()
    -- runTimeResult <- runExceptT $ runReaderT (interpretProgram program) Data.Map.empty
    -- case runTimeResult of
    --   Left err -> do
    --     putStr "Runtime error: "
    --     case err of
    --       NoReturnException -> returnError "Function has to return some value"
    --       OutOfRangeExeption identifier -> returnError ("Index " ++ show identifier ++ " out of range!")
    --       UnitializedException identifier -> returnError ("You have to initialized identifier" ++ show identifier ++ "!")
    --       DivisionByZeroException -> returnError "You cannot divide by 0!"
    --       ModulusByZeroException -> returnError "You cannot module by 0!"
    --   Right _ -> return ()
    (Bad s) -> returnError "Error while parsing"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> returnError "You need to provide file names to run interpreter"
    files -> mapM_ readCode files
