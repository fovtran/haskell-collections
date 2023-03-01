data LogicExp  a = P a                              | 
                     True'                      | 
                     False'                                 | 
                     Not' (LogicExp a)                  |  
                     (LogicExp a) :&  (LogicExp a)  | 
                     (LogicExp a) :|  (LogicExp a)  | 
                     (LogicExp a) :=> (LogicExp a)    |
                     (LogicExp a) :=  (LogicExp a)
    deriving Show


type LExp = LogicExp String

data Position = L | R

deMorgan :: LExp -> LExp
deMorgan (e1 :& e2) = Not' ((Not e1) :| (Not e2))
deMorgan (e1 :| e2) = Not' ((Not e1) :& (Not e2))
deMorgan x = x

apply :: [Position] -> (LExp -> LExp) -> LExp -> LExp
apply [] f e = f e
apply (L:xs) f (e1 :& e2) = (apply xs f e1) :& e2
apply (R:xs) f (e1 :& e2) = e1 :& (apply xs f e2)
apply (L:xs) f (e1 :| e2) = (apply xs f e1) :| e2
apply (R:xs) f (e1 :| e2) = e1 :| (apply xs f e2)
apply (L:xs) f (e1 :=> e2) = (apply xs f e1) :=> e2
apply (R:xs) f (e1 :=> e2) = e1 :=> (apply xs f e2)
apply (L:xs) f (e1 := e2) = (apply xs f e1) := e2
apply (R:xs) f (e1 := e2) = e1 := (apply xs f e2)
apply (x:xs) f (Not' e) = apply xs f e
The function works fine. But can I use some data constructor "wildcard" to have a more simple function like this?

apply :: [Position] -> (LExp -> LExp) -> LExp -> LExp
apply [] f e = f e
apply (L:xs) f (e1 ?? e2) = (apply xs f e1) ?? e2
apply (R:xs) f (e1 ?? e2) = e1 ?? (apply xs f e2)
apply (x:xs) f (Not' e) = apply xs f e


data LogicExp a
    = P a
    | True'
    | False'
    | Not' (LogicExp a) 
    | Bin' BinaryOp (LogicExp a) (LogicExp a)
    deriving Show

data BinaryOp = And' | Or' | Impl' | Equiv'
    deriving Show
apply :: [Position] -> (LExp -> LExp) -> LExp -> LExp
apply [] f e = f e
apply (L:xs) f (Bin' op e1 e2) = Bin' op (apply xs f e1) e2
apply (R:xs) f (Bin' op e1 e2) = Bin' op e1 (apply xs f e2)
apply (x:xs) f (Not' e) = apply xs f e
-- ... and the P, True' and False' cases.