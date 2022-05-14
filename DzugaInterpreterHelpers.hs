module DzugaInterpreterHelpers where
    import AbsGrammar ( Type(Array, Bool, Int, Str), Stmt )

    import Control.Monad.Reader ( ReaderT )
    import Control.Monad.Except ( ExceptT )
    import Data.Map ( Map )


    type ReturnResult = Maybe ValueInMemory

    type FArg = (String, Type)
    type Function = ([Stmt], IIEnv, [FArg], Type)

    type Array = (Type, [ValueInMemory])

    data ValueInMemory = IntValue Integer | BooleanValue Bool | StringValue String | ArrayValue Array | FunctionValue Function

    type IIEnv = (Data.Map.Map String ValueInMemory)
    type II = ReaderT IIEnv (ExceptT RuntimeExceptions IO)

    data RuntimeExceptions = DivisionByZeroException | ModulusByZeroException | NoReturnException | OutOfRangeExeption Integer | UnitializedException String deriving Show


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
