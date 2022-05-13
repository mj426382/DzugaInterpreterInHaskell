module CheckingPreparation where
    import AbsGrammar( Expr(..), Type(Array, Bool, Str, Fun, Void, Int) )
    import TypeCheckHelpers( evalCorrectArray,evalCorrectFunction,getTypeFromEnv,getStableTypeForConst )
    import Types( TC,TypeCheckExceptions(NotAnArrayException, TypeCheckException,FuncApplicationException) )

    import Control.Monad.Reader ( unless )
    import Control.Monad.Except ( unless, MonadError(throwError) )
    import Data.Maybe ( isNothing )


    checkExprArray :: [Expr] -> Type -> TC ()
    checkExprArray [] typ =
        prepareCheckType typ typ
    checkExprArray (expr:rest) typ = do
        prepareCheckTypeExpr expr typ
        checkExprArray rest typ

    prepareCheckType :: Type -> Type -> TC ()
    prepareCheckType a b = do
        stableA <- getStableTypeForConst a
        stableB <- getStableTypeForConst b
        unless (stableA == stableB) $ throwError $ TypeCheckException a b

    prepareCheckArgsTypes :: [Type] -> [Expr] -> TC ()
    prepareCheckArgsTypes [] [] = return ()
    prepareCheckArgsTypes (typ:restTypes) (expr:vars) = do
        prepareCheckTypeExpr expr typ
        prepareCheckArgsTypes restTypes vars
    
    prepareCheckArgsTypes a b = throwError FuncApplicationException

    prepareCheckTypeExpr :: Expr -> Type -> TC ()
    prepareCheckTypeExpr e t = do
        typ <- prepareExprType e
        prepareCheckType typ t

    prepareExprType :: Expr -> TC Type

    prepareExprType (EVar ident) = getTypeFromEnv ident

    prepareExprType ELitInt {} = return Int

    prepareExprType (EAdd e1 op e2) = do
        prepareCheckTypeExpr e1 Int
        prepareCheckTypeExpr e2 Int
        return Int

    prepareExprType (EMul e1 op e2) = do
        prepareCheckTypeExpr e1 Int
        prepareCheckTypeExpr e2 Int
        return Int

    prepareExprType (EAnd e1 e2) = do
        prepareCheckTypeExpr e1 Bool
        prepareCheckTypeExpr e2 Bool
        return Bool

    prepareExprType (EOr e1 e2) = do
        prepareCheckTypeExpr e1 Bool
        prepareCheckTypeExpr e2 Bool
        return Bool

    prepareExprType (Neg e) = do
        prepareCheckTypeExpr e Int
        return Int

    prepareExprType ELitFalse = return Bool
    prepareExprType ELitTrue = return Bool

    prepareExprType (ERel e1 op e2) = do
        prepareCheckTypeExpr e1 Int
        prepareCheckTypeExpr e2 Int
        return Bool

    prepareExprType (Not e) = do
        prepareCheckTypeExpr e Bool
        return Bool

    prepareExprType (EString e) = return Str

    prepareExprType (EApp identifier args) = do
        typ <- getTypeFromEnv identifier

        let functionEvaluation = evalCorrectFunction typ

        if isNothing functionEvaluation then
            throwError $ FuncApplicationException
        else do
            let (Fun returnedType types) = typ
            prepareCheckArgsTypes types args
            return $ returnedType

    prepareExprType (Ekrotka exprs) = do
        if (length exprs) == 0 then
            return $ (Array Void)
        else
            do
                krotkaType <- (prepareExprType (head exprs))
                checkExprArray exprs krotkaType
                return $ (Array krotkaType)

    prepareExprType (EArrayVar identifier expr) = do
        typ <- getTypeFromEnv identifier
        prepareCheckTypeExpr expr Int
        
        let arrayEvaluation = evalCorrectArray typ

        if isNothing arrayEvaluation then
            throwError $ NotAnArrayException
        else do
            let Just (Array arrayType) = arrayEvaluation
            return $ arrayType
