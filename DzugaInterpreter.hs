module DzugaInterpreter where
    import AbsGrammar

    import Data.Map as Map
    import Data.Maybe

    import Control.Monad.Reader
    import Control.Monad.Except

    import Types
    import Memory
    import TypeChecker

    makeArrayString :: [MemVal] -> String
    makeArrayString [] = ""
    makeArrayString [a] = makeString a
    makeArrayString (a:tail) = makeString a ++ ", " ++ makeArrayString tail

    makeString :: MemVal -> String
    makeString (IntVal i) = show i
    makeString (BoolVal b) = show b
    makeString (StringVal s) = s
    makeString (FunVal (stmt, env, arg, typ, capture)) = "Function (" ++ show arg ++ ") -> " ++ show typ
    makeString (ArrayVal (typ, array)) = "[" ++ makeArrayString array ++ "]"

    evalRelOp GTH e1 e2 = e1 > e2
    evalRelOp GE e1 e2 = e1 >= e2
    evalRelOp LTH e1 e2 = e1 < e2
    evalRelOp EQU e1 e2 = e1 == e2
    evalRelOp NE e1 e2 = e1 /= e2
    evalRelOp LE e1 e2 = e1 <= e2

    evalMulOp Div e1 e2 = div e1 e2
    evalMulOp Times e1 e2 = e1 * e2
    evalMulOp Mod e1 e2 = e1 `mod` e2

    evalAddOp Minus e1 e2 = e1 - e2
    evalAddOp Plus e1 e2 = e1 + e2

    evalExpression :: Expr -> II MemVal

    evalExpression (EVar ident) = readFromMemory ident

    -- evalExpression (EApp ident vars) = do
    --     (env2, Just ret) <- runFunc ident vars
    --     return ret

    evalExpression (ELitInt x) = return $ IntVal x
    evalExpression (EAdd e1 op e2) = do
        IntVal r1 <- evalExpression e1
        IntVal r2 <- evalExpression e2
        return $ IntVal $ evalAddOp op r1 r2

    evalExpression (EMul e1 op e2) = do
        IntVal r1 <- evalExpression e1
        IntVal r2 <- evalExpression e2
        case op of
            Div -> if r2 == 0 then throwError DivisionByZeroException else return $ IntVal $ evalMulOp op r1 r2
            Mod -> if r2 == 0 then throwError ModulusByZeroException else return $ IntVal $ evalMulOp op r1 r2
            _ -> return $ IntVal $ evalMulOp op r1 r2
    
    evalExpression (Neg e) = do
        IntVal r <- evalExpression e
        return $ IntVal $ -r

    evalExpression ELitFalse = return $ BoolVal False
    evalExpression ELitTrue = return $ BoolVal True
    
    evalExpression (Not e) = do
        BoolVal r <- evalExpression e
        return $ BoolVal $ not r

    evalExpression (EAnd e1 e2) = do
        BoolVal r1 <- evalExpression e1
        BoolVal r2 <- evalExpression e2
        return $ BoolVal $ r1 && r2

    evalExpression (EOr e1 e2) = do
        BoolVal r1 <- evalExpression e1
        BoolVal r2 <- evalExpression e2
        return $ BoolVal $ r1 || r2

    evalExpression (ERel e1 op e2) = do
        IntVal r1 <- evalExpression e1
        IntVal r2 <- evalExpression e2
        return $ BoolVal $ evalRelOp op r1 r2

    evalExpression (EString e) = return $ StringVal e

    --function
    -- evalExpression (ELambda capture args returnType (Block stmts)) = do
    --     argsList <- mapM argToFunArg args
    --     captureGroup <- mapM constructCaptureGroup capture
    --     return $ FunVal (stmts, Map.empty, argsList, returnType, captureGroup)

    --array
    evalExpression (EArrayVar identifier expr) = do
        IntVal i <- evalExpression expr
        (typ, array) <- readArrayFromMemory identifier
        if i < 0 || i >= fromIntegral(length array) then
            throwError $ OutOfRangeExeption i
        else
            return $ (array !! (fromIntegral i))


    defaultValueOfType :: Type -> II MemVal
    defaultValueOfType Bool = return $ (BoolVal False)
    defaultValueOfType Int = return $ (IntVal 0)
    defaultValueOfType Str = return $ (StringVal "")
    defaultValueOfType (Array typ) = return $ (ArrayVal (typ, []))

    createMemValueArray :: [Expr] -> II [MemVal]
    createMemValueArray [] = return $ []
    createMemValueArray [expr] = do
        val <- evalExpression expr
        return $ [val]
    createMemValueArray (expr:rest) = do
        val <- evalExpression expr
        tail <- createMemValueArray rest
        return $ val : tail

    createEmptyList :: Int -> Type -> II [MemVal]
    createEmptyList 0 _ = return $ []
    createEmptyList 1 typ = do
        defType <- defaultValueOfType typ
        return $ [defType]
    createEmptyList n typ = do
        defType <- defaultValueOfType typ
        tail <- createEmptyList (n - 1) typ
        return $ defType : tail


    execStmt :: Stmt -> II (MyEnv, ReturnResult)

    execStmt (Decl typ [(NoInit identifier)]) = do
        defValue <- defaultValueOfType typ
        env <- addVarToMemory identifier (defValue)
        return (env, Nothing)

    execStmt (Decl typ [(Init identifier expr)]) = do
        val <- evalExpression expr
        env <- addVarToMemory identifier val
        return (env, Nothing)

    execStmt (Decl typ [(NoInitArr identifier expr)]) = do
        val <- evalExpression expr
        let (IntVal integer) = val
        array <- createEmptyList (fromIntegral integer) typ
        env <- addVarToMemory identifier (ArrayVal (typ, array))
        return (env, Nothing)

    execStmt (Decl typ [(InitArr identifier expr exprs)]) = do
        val <- evalExpression expr
        let (IntVal integer) = val
        let arrLength = fromIntegral integer
        unless ((length exprs) <= arrLength) $ throwError $ (OutOfRangeExeption (fromIntegral (length exprs)))
        array <- createMemValueArray exprs
        restArray <- createEmptyList (arrLength - (length array)) typ
        env <- addVarToMemory identifier (ArrayVal (typ, array ++ restArray))
        return (env, Nothing)

    execStmt (Decl typ (item:rest)) = do
        (env, ret) <- execStmt (Decl typ [item])
        local (const env) (execStmt (Decl typ rest))

    execStmt (BStmt block) = do
        let (Block stmts) = block
        interpretMany stmts

    execStmt (Ass identifier expr) = do
        val <- evalExpression expr
        env <- addVarToMemory identifier val
        return (env, Nothing)

    execStmt (AddArr identifier expr1 expr2) = do
        val1 <- evalExpression expr1
        val2 <- evalExpression expr2
        arrDef <- readArrayFromMemory identifier
        let (IntVal integer) = val1
        let (typ, arr) = arrDef
        let index = (fromIntegral integer)
        unless (index < (length arr)) $ throwError $ (OutOfRangeExeption integer)
        let (body,_:tail) = Prelude.splitAt index arr
        let updatedArray = body ++ val2 : tail
        env <- addVarToMemory identifier (ArrayVal (typ, updatedArray))
        return (env, Nothing)

    execStmt (Incr identifier) = do
        intVal <- readIntFromMemory identifier
        let memVal = IntVal ((fromIntegral intVal) + 1)
        env <- addVarToMemory identifier memVal
        return (env, Nothing)

    execStmt (Decr identifier) = do
        intVal <- readIntFromMemory identifier
        let memVal = IntVal ((fromIntegral intVal) - 1)
        env <- addVarToMemory identifier memVal
        return (env, Nothing)

    execStmt (Cond expr stm) = do
        val <- evalExpression expr
        let (BoolVal boolVal) = val
        if boolVal then
            execStmt stm
        else do
            env <- ask
            return (env, Nothing)

    execStmt (CondElse expr stm1 stm2) = do
        val <- evalExpression expr
        let (BoolVal boolVal) = val
        if boolVal then
            execStmt stm1
        else 
            execStmt stm2

    execStmt (While expr stm) = do
        val <- evalExpression expr
        let (BoolVal boolVal) = val        
        if boolVal then
            do
                (env, ret) <- execStmt stm
                if isNothing ret then
                    local (const env) (execStmt (While expr stm))
                else
                    return (env, ret)
        else do
            env <- ask
            return (env, Nothing)

    execStmt (For identifier expr1 expr2 stm) = do
        val1 <- evalExpression expr1
        val2 <- evalExpression expr2
        let (IntVal start) = val1
        let (IntVal end) = val2
        env <- addVarToMemory identifier (IntVal start)
        let newStmt = BStmt (Block [stm, (Incr identifier)])
        if start < end then
            do
                (env, ret) <- local (const env) (execStmt newStmt)
                if isNothing ret then do
                    let newExpr = ERel (EVar identifier) LTH expr2
                    local (const env) (execStmt (While newExpr newStmt))
                else
                    return (env, ret)
        else do
            env <- ask
            return (env, Nothing)

    execStmt (Repeat expr stm) = do
        val <- evalExpression expr
        let (IntVal intVal) = val
        let identifier = (Ident "repeater")
        env <- addVarToMemory identifier (IntVal 0)
        let newStmt = BStmt (Block [stm, (Incr identifier)])
        if 0 < intVal then
            do
                (env, ret) <- local (const env) (execStmt newStmt)
                if isNothing ret then do
                    let newExpr = ERel (EVar identifier) LTH expr
                    local (const env) (execStmt (While newExpr newStmt))
                else
                    return (env, ret)
        else do
            env <- ask
            return (env, Nothing)

    execStmt (Print expr) = do
        res <- evalExpression expr
        let str = makeString res
        liftIO $ putStr str
        env <- ask
        return (env, Nothing)

    -- execStmt Break = do
    -- execStmt Continue = do

    execStmt _ = do
        env <- ask
        return (env, Nothing)

    interpretMany :: [Stmt] -> II (MyEnv, ReturnResult)
    interpretMany (s:xs) = do
                (env, ret) <- execStmt s
                if isNothing ret then
                    local (const env) (interpretMany xs)
                else
                    return (env, ret)
    interpretMany [] = do
        env <- ask
        return (env, Nothing)

    interpretProgram :: [TopDef] -> II (MyEnv, ReturnResult)
    interpretProgram (fun:rest) = do
        let (FnDef typ identifier args block) = fun
        let (Ident functionName) = identifier
        if (functionName == "dzuga") then do
            let (Block stmts) = block
            interpretMany stmts
        else do
            env <- ask
            return (env, Nothing)
