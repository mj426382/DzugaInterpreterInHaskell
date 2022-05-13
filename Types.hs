module Types where
    import AbsGrammar ( Type, Stmt )

    import Control.Monad.Reader ( ReaderT )
    import Control.Monad.Except ( ExceptT )
    import Data.Map ( Map )


    type ReturnResult = Maybe ValueInMemory

    type FArg = (String, Type)
    type Function = ([Stmt], IIEnv, [FArg], Type)

    type Array = (Type, [ValueInMemory])

    data ValueInMemory = IntValue Integer | BooleanValue Bool | StringValue String | ArrayValue Array | FunctionValue Function

    data TypeCheckExceptions = InvalidTypeInDeclarationException Type | OverridingConstException Type | NotInitializedConst Type | NotAnArrayException | TypeCheckException Type Type | FuncApplicationException | NonexistingIdentifierException String deriving Show
    data RuntimeExceptions = DivisionByZeroException | ModulusByZeroException | NoReturnException | OutOfRangeExeption Integer | UnitializedException String deriving Show

    type IIEnv = (Data.Map.Map String ValueInMemory)
    type II = ReaderT IIEnv (ExceptT RuntimeExceptions IO)
    type TCEnv = (Data.Map.Map String Type)
    type TC = ReaderT  TCEnv (ExceptT TypeCheckExceptions IO)
    type TCRes = Maybe Type
