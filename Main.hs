module Main where
    import System.IO ( stdin, hGetContents, hPutStrLn, stderr, getContents, hPutStr )
    import System.Environment ( getArgs, getProgName )
    import System.Exit ( exitFailure, exitSuccess )
    import Control.Exception (catch, IOException)    
    import Data.Map as Map

    import TypeChecker
    import Types
    import DzugaInterpreter

    import Control.Monad.Reader
    import Control.Monad.Except

    import ParGrammar
    import AbsGrammar
    import ErrM

    exitWithError :: String -> IO ()
    exitWithError msg = do
        hPutStrLn stderr msg
        exitFailure

    parse :: String -> IO ()
    parse input =
        case pProgram (myLexer input) of
            (Ok s) -> do
                let Program program = s
                typeCheckResult <- runExceptT $ runReaderT (checkStatementTypeProgram program) Map.empty
                case typeCheckResult of
                    Left err -> do
                        hPutStr stderr "Typecheck error. "        
                        case err of
                            TypeCheckException given expected -> exitWithError $ "Expected " ++ show expected ++ " given: "++ show given
                            FuncApplicationException -> exitWithError "Invalid function argument application"
                            NonexistingIdentifierException i -> exitWithError $ "Identifier " ++ i ++ " doesn't exist"
                            InvalidTypeInDeclarationException typ -> exitWithError $ "Invalid use of type " ++ show typ ++ " in declaration"
                            OverridingConstException typ -> exitWithError $ "Try to override const " ++ show typ ++ " in declaration"
                            NotInitializedConst typ -> exitWithError $ "Not initialized const " ++ show typ ++ " in declaration"
                            NotAnArrayException -> exitWithError $ "Not an array"
                    Right _ -> do
                        runTimeResult <- runExceptT $ runReaderT (interpretProgram program) Map.empty
                        case runTimeResult of
                            Left err -> do
                                hPutStr stderr "Runtime exception. "
                                case err of
                                    DivisionByZeroException -> exitWithError "Division by 0"
                                    ModulusByZeroException -> exitWithError "Modulo 0"
                                    NoReturnException ->  exitWithError "Function didn't return any value"
                                    OutOfRangeExeption i -> exitWithError ("Index " ++ show i ++ " out of range!")
                                    UnitializedException i -> exitWithError ("Identifier unitialized!" ++ show i)
                            Right _ -> return()
                return ()
            (Bad s) -> exitWithError "Parse error"

    parseFile :: String -> IO ()
    parseFile fileName = readFile fileName >>= parse

    main :: IO ()
    main = do
        args <- getArgs
        case args of
            [] -> exitWithError "You need to provide file names to run interpreter"
            files -> mapM_ parseFile files
