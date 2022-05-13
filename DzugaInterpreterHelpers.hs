module DzugaInterpreterHelpers where
    import Types ( ValueInMemory(..), II )
    import AbsGrammar ( Type(Array, Bool, Int, Str) )
    

    makeArrayString :: [ValueInMemory] -> String
    makeArrayString [] = ""
    makeArrayString [a] = makeString a
    makeArrayString (a:tail) = makeString a ++ ", " ++ makeArrayString tail

    makeString :: ValueInMemory -> String
    makeString (IntValue i) = show i
    makeString (BooleanValue b) = show b
    makeString (StringValue s) = s
    makeString (FunctionValue (stmt, env, arg, typ)) = "Type: " ++ show typ ++ "Args: " ++ show arg
    makeString (ArrayValue (typ, array)) = "[" ++ makeArrayString array ++ "]"

    defaultValueOfType :: Type -> II ValueInMemory
    defaultValueOfType Bool = return $ (BooleanValue False)
    defaultValueOfType Int = return $ (IntValue 0)
    defaultValueOfType Str = return $ (StringValue "")
    defaultValueOfType (Array typ) = return $ (ArrayValue (typ, []))

    createEmptyList :: Int -> Type -> II [ValueInMemory]
    createEmptyList 0 _ = return $ []
    createEmptyList 1 typ = do
        defType <- defaultValueOfType typ
        return $ [defType]
    createEmptyList n typ = do
        defType <- defaultValueOfType typ
        tail <- createEmptyList (n - 1) typ
        return $ defType : tail
