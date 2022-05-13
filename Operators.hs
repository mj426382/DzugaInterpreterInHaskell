module Operators where
    import AbsGrammar( RelOp(LE, GTH, GE, LTH, EQU, NE), MulOp(Mod, Div, Times), AddOp(Plus, Minus) )


    makeAdd Minus a b = a - b

    makeAdd Plus a b = a + b

    makeMul Div a b = div a b

    makeMul Times a b = a * b

    makeMul Mod a b = a `mod` b

    makeRel GTH a b = a > b

    makeRel GE a b = a >= b
    
    makeRel LTH a b = a < b
    
    makeRel EQU a b = a == b
    
    makeRel NE a b = a /= b

    makeRel LE a b = a <= b
