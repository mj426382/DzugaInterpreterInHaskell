module Memory where
    import AbsGrammar

    import Data.Map as Map
    import Data.Maybe

    import Control.Monad.Reader
    import Control.Monad.Except

    import Types

    addVarToMemory :: Ident -> MemVal -> II MyEnv
    addVarToMemory (Ident identifier) memVal = do
        env <- ask
        return (Map.insert identifier memVal env)

    readFromMemory :: Ident -> II MemVal
    readFromMemory (Ident identifier) = do
        env <- ask
        case Map.lookup identifier env of
            Just memVal -> return memVal
            Nothing -> do 
                throwError $ UnitializedException identifier
                return (IntVal 0)
        
    readIntFromMemory :: Ident -> II Integer
    readIntFromMemory ident = do
        IntVal integer <- readFromMemory ident
        return integer

    readFunFromMemory :: Ident -> II FunDef
    readFunFromMemory ident = do
        FunVal fun <- readFromMemory ident
        return fun

    readArrayFromMemory :: Ident -> II ArrayDef
    readArrayFromMemory ident = do
        ArrayVal array <- readFromMemory ident
        return array
