module Main where
    import AbsGrammar ( Program(Program) )
    import DzugaInterpreter ( interpretProgram )
    import ParGrammar ( pProgram, myLexer )
    import ErrM
    import TypeChecker ( checkStatementTypeProgram )
    import Types ( RuntimeExceptions(ModulusByZeroException, NoReturnException, OutOfRangeExeption, UnitializedException, DivisionByZeroException), TypeCheckExceptions(NotAnArrayException, TypeCheckException, FuncApplicationException, NonexistingIdentifierException, InvalidTypeInDeclarationException, OverridingConstException, NotInitializedConst) )

    import Control.Monad.Reader ( ReaderT(runReaderT) )
    import Control.Monad.Except ( runExceptT )
    import Data.Map ( empty )
    import System.Environment ( getArgs )
    import System.Exit ( exitFailure )


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
                typeCheckResult <- runExceptT $ runReaderT (checkStatementTypeProgram program) Data.Map.empty
                case typeCheckResult of
                    Left err -> do
                        putStr "Typecheck err: "
                        case err of
                            TypeCheckException given expected -> returnError $ "Expected " ++ show expected ++ " given: "++ show given
                            FuncApplicationException -> returnError "Invalid function argument application"
                            NonexistingIdentifierException identifier -> returnError $ "Identifier " ++ identifier ++ " doesn't exist"
                            InvalidTypeInDeclarationException typ -> returnError $ "Invalid use of type " ++ show typ ++ " in declaration"
                            OverridingConstException typ -> returnError $ "Try to override const " ++ show typ ++ " in declaration"
                            NotInitializedConst typ -> returnError $ "Not initialized const " ++ show typ ++ " in declaration"
                            NotAnArrayException -> returnError $ "Not an array"
                    Right _ -> do
                        runTimeResult <- runExceptT $ runReaderT (interpretProgram program) Data.Map.empty
                        case runTimeResult of
                            Left err -> do
                                putStr "Runtime error: "
                                case err of
                                    NoReturnException ->  returnError "Function has to return some value"
                                    OutOfRangeExeption identifier -> returnError ("Index " ++ show identifier ++ " out of range!")
                                    UnitializedException identifier -> returnError ("You have to initialized identifier" ++ show identifier ++ "!")
                                    DivisionByZeroException -> returnError "You cannot divide by 0!"
                                    ModulusByZeroException -> returnError "You cannot module by 0!"
                            Right _ -> return()
                return ()
            (Bad s) -> returnError "Error while parsing"

    main :: IO ()
    main = do
        args <- getArgs
        case args of
            [] -> returnError "You need to provide file names to run interpreter"
            files -> mapM_ readCode files
