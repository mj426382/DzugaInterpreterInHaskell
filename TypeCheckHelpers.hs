module TypeCheckHelpers where
    import AbsGrammar ( Ident(..), Type(..), Arg(..) )
    import Types( TC, TypeCheckExceptions(NonexistingIdentifierException) )

    import Control.Monad.Reader ( MonadReader(ask) )
    import Control.Monad.Except ( MonadError(throwError) )
    import Data.Map ( lookup )


    evalCorrectArray :: Type -> Maybe Type
    evalCorrectArray (Array typ) = Just (Array typ)
    evalCorrectArray typ = Nothing

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
    isConstType ConstInt = return True
    isConstType ConstBool = return True
    isConstType ConstStr = return True
    isConstType _ = return False

    getStableTypeForConst :: Type -> TC Type
    getStableTypeForConst ConstInt = return Int
    getStableTypeForConst ConstBool = return Bool
    getStableTypeForConst ConstStr = return Str
    getStableTypeForConst t = return t

    getTypesFromArgs :: [Arg] -> TC [Type]
    getTypesFromArgs [] = do
        return $ []
    getTypesFromArgs [arg] = do
        let (Arg typ ident) = arg
        return $ [typ]
    getTypesFromArgs (arg:rest) = do
        let (Arg typ ident) = arg
        restTypes <- getTypesFromArgs rest
        return $ (typ : restTypes)
