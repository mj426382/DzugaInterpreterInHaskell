module Types where
    import AbsGrammar

    import Data.Map as Map
    import Data.Maybe

    import Control.Monad.Reader
    import Control.Monad.Except

    -- Environment, maps variable/function name to location
    type MyEnv = Map.Map String MemVal

    -- return value of block of statements. Nothing when no return, Just MemVal if return occured
    type ReturnResult = Maybe MemVal

    -- way of passing argument to function
    data PassArgType = ByValue | ByRef deriving Show

    -- argument passed to function (name of variable, type of variable, way of passing argument)
    type FunArg = (String, Type, PassArgType)

    -- list of arguments passed to function
    type FunArgList = [FunArg]

    -- list of "capture group" - name of variable and value when created lambda
    type CaptureGroupElement = (Ident, MemVal)
    type CaptureGroup = [CaptureGroupElement]

    -- function/lambda - fun body, environment when declared function, return type, capture group when declared function/lambda (function always empty)
    type FuncDef = ([Stmt], MyEnv, FunArgList, Type, CaptureGroup)

    -- list definition - hold type and list of values
    type ArrayDef = (Type, [MemVal])

    -- general type for value in memory (in store)
    data MemVal = BoolVal Bool | IntVal Integer | StringVal String | FunVal FuncDef | ArrayVal ArrayDef

    data TypeCheckExceptions = InvalidTypeInDeclarationException Type | OverridingConstException Type | NotInitializedConst Type | NotAnArrayException | TypeCheckException Type Type | FuncApplicationException | NonexistingIdentifierException String deriving Show

    data RuntimeExceptions = DivisionByZeroException | ModulusByZeroException | NoReturnException | OutOfRangeExeption Integer | UnitializedException String deriving Show

    type II = ReaderT MyEnv (ExceptT RuntimeExceptions IO)

    type TCEnv = (Map.Map String Type)
    type TC = ReaderT  TCEnv (ExceptT TypeCheckExceptions IO)
    type TCRes = Maybe Type
