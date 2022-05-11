module TypeChecker where
    
    import AbsGrammar
    import CheckingStatements
    import TypeCheckHelpers
    import Types

    import Control.Monad.Reader
    import Control.Monad.Except    
    import Data.Map as Map
    import Data.Maybe

    addArgsToEnv :: [Arg] -> TC (TCEnv, TCRes)
    addArgsToEnv [] = do
        env <- ask
        return (env, Nothing)
    addArgsToEnv [arg] = do
        let (Arg typ identifier) = arg
        let (Ident ident) = identifier
        env <- ask
        return (Map.insert ident typ env, Nothing)
    addArgsToEnv (arg:rest) = do
        let (Arg typ identifier) = arg
        let (Ident ident) = identifier
        (env, ret) <- addArgsToEnv rest
        return (Map.insert ident typ env, Nothing)

    addFunctionDeclaration :: TopDef -> TC (TCEnv, TCRes)
    addFunctionDeclaration (FnDef typ (Ident identifier) args block) = do
        argTypes <- getTypesFromArgs args
        let fnType = (Fun typ argTypes)
        (env, ret) <- addArgsToEnv args
        return (Map.insert identifier fnType env, Nothing)

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
