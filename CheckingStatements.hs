module CheckingStatements where
    import AbsGrammar
    import CheckingPreparation
    import TypeCheckHelpers
    import Types

    import Control.Monad.Reader
    import Control.Monad.Except
    import Data.Map as Map
    import Data.Maybe

    checkStatementType :: Stmt -> TC (TCEnv, TCRes)

    checkStatementType (Decl typ [(NoInit (Ident identifier))]) = do
        isCorrect <- isCorrectStmtType typ
        isConst <- isConstType typ
        unless isCorrect $ throwError $ InvalidTypeInDeclarationException typ
        unless (not isConst) $ throwError $ NotInitializedConst typ
        env <- ask
        return (Map.insert identifier typ env, Nothing)

    checkStatementType (Decl typ [(NoInitArr (Ident identifier) expr)]) = do
        isCorrect <- isCorrectStmtType typ
        isConst <- isConstType typ
        unless isCorrect $ throwError $ InvalidTypeInDeclarationException typ
        unless (not isConst) $ throwError $ NotInitializedConst typ
        prepareCheckTypeExpr expr Int
        env <- ask
        return (Map.insert identifier (Array typ) env, Nothing)

    checkStatementType (Decl typ [(Init (Ident identifier) expr)]) = do
        isCorrect <- isCorrectStmtType typ
        unless isCorrect $ throwError $ InvalidTypeInDeclarationException typ
        stableType <- getStableTypeForConst typ
        prepareCheckTypeExpr expr stableType
        env <- ask
        return (Map.insert identifier typ env, Nothing)

    checkStatementType (Decl typ [(InitArr (Ident identifier) expr exprs)]) = do
        isCorrect <- isCorrectStmtType typ
        unless isCorrect $ throwError $ InvalidTypeInDeclarationException typ
        stableType <- getStableTypeForConst typ
        prepareCheckTypeExpr expr stableType
        checkExprArray exprs typ
        env <- ask
        return (Map.insert identifier (Array typ) env, Nothing)

    checkStatementType (Decl typ (item:rest)) = do
        (env, ret) <- checkStatementType (Decl typ [item])
        local (const env) (checkStatementType (Decl typ rest))

    checkStatementType (While expr stm) = do
        prepareCheckTypeExpr expr Bool
        checkStatementType stm

    checkStatementType (Repeat expr stm) = do
        prepareCheckTypeExpr expr Int
        checkStatementType stm

    checkStatementType (For ident expr1 expr2 stm) = do
        prepareCheckTypeExpr expr1 Int
        prepareCheckTypeExpr expr2 Int
        let (Ident identifier) = ident
        (env, ret) <- checkStatementType stm
        return (Map.insert identifier Int env, Nothing)

    checkStatementType (Cond expr stmt) = do
        prepareCheckTypeExpr expr Bool
        checkStatementType stmt

    checkStatementType (CondElse expr stmt1 stmt2) = do
        prepareCheckTypeExpr expr Bool
        checkStatementType stmt1
        checkStatementType stmt2

    checkStatementType (BStmt block) = do
        let (Block blocks) = block
        if (length blocks == 0) then do
            env <- ask
            return (env, Nothing)
        else do
            let (Block (stm:rest)) = block
            (env, ret) <- checkStatementType stm
            local (const env) (checkStatementType (BStmt (Block rest)))

    checkStatementType (Ass identifier expr) = do
        typ <- getTypeFromEnv identifier
        isConst <- isConstType typ
        unless (not isConst) $ throwError $ OverridingConstException typ
        exprType <- prepareExprType expr
        prepareCheckTypeExpr expr typ
        env <- ask
        return (env, Nothing)

    checkStatementType (AddArr identifier expr1 expr2) = do
        typ <- getTypeFromEnv identifier
        let arrayType = evalCorrectArray typ
        if isNothing arrayType then
            throwError $NotAnArrayException
        else do
            prepareCheckTypeExpr expr1 Int
            let (Just (Array innerType)) = arrayType
            prepareCheckTypeExpr expr2 innerType
            env <- ask
            return (env, Nothing)

    checkStatementType (Incr identifier) = do
        typ <- getTypeFromEnv identifier 
        prepareCheckType typ Int
        env <- ask
        return (env, Nothing)

    checkStatementType (Decr identifier) = do
        typ <- getTypeFromEnv identifier 
        prepareCheckType typ Int
        env <- ask
        return (env, Nothing)

    checkStatementType _ = do
        env <- ask
        return (env, Nothing)

    checkStatementTypeForMany :: [Stmt] -> TC (TCEnv, TCRes)
    checkStatementTypeForMany (stm:rest) = do
        (env, ret) <- checkStatementType stm
        if isNothing ret then
            local (const env) (checkStatementTypeForMany rest)
        else
            return (env, ret)
    checkStatementTypeForMany [] = do
        env <- ask
        return (env, Nothing)
