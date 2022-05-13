module TypeChecker where
    
    import AbsGrammar( Ident(Ident), Type(Fun), Block(Block), Arg(..), TopDef(..) )
    import CheckingStatements ( checkStatementTypeForMany )
    import TypeCheckHelpers ( getTypesFromArgs )
    import Types ( TCRes, TC, TCEnv )

    import Control.Monad.Reader ( MonadReader(ask, local) )
    import Data.Map ( insert )
    import Data.Maybe ( isNothing )

    addArgsToEnv :: [Arg] -> TC (TCEnv, TCRes)
    addArgsToEnv [] = do
        env <- ask
        return (env, Nothing)
    addArgsToEnv [arg] = do
        let (Arg typ identifier) = arg
        let (Ident ident) = identifier
        env <- ask
        return (Data.Map.insert ident typ env, Nothing)
    addArgsToEnv (arg:rest) = do
        let (Arg typ identifier) = arg
        let (Ident ident) = identifier
        (env, ret) <- addArgsToEnv rest
        return (Data.Map.insert ident typ env, Nothing)

    addFunctionDeclaration :: TopDef -> TC (TCEnv, TCRes)
    addFunctionDeclaration (FnDef typ (Ident identifier) args block) = do
        argTypes <- getTypesFromArgs args
        let fnType = (Fun typ argTypes)
        (env, ret) <- addArgsToEnv args
        return (Data.Map.insert identifier fnType env, Nothing)

    checkStatementTypeProgram :: [TopDef] -> TC (TCEnv, TCRes)
    checkStatementTypeProgram (fn:rest) = do
        let (FnDef typ identifier args block) = fn
        let (Block stmts) = block
        (env, ret) <- addFunctionDeclaration fn
        if isNothing ret then do
            local (const env) (checkStatementTypeForMany stmts)
            local (const env) (checkStatementTypeProgram rest)
        else
            return (env, ret)
    checkStatementTypeProgram [] = do
        env <- ask
        return (env, Nothing)
