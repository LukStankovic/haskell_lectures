-- iterace, zřetězení, ... já 1 - dva týdny
-- http://behalek.cs.vsb.cz/wiki/index.php/FP_Homework_3_extension

-- regulářní výraz - spojení s tečkou a nebo, iterace, znak
-- potom udělat regulární výraz na automat
    --  reprezentace automatu - 0 startovní - dvojice trojice počáteční a stav
-- R(A) | R(B) - nový stav
-- R(A) . R(B) - koncové na ten počáteční
-- iterace - opakování



-- netradiční jazyk - referát - příští týden se ozvat

-- import Control.Monad (join)

import Control.Monad

-- data RegExp = Empty
--             | Epsilon
--             | Lit Char
--             | Cat RegExp RegExp
--             | Alt RegExp RegExp
--             | Star RegExp
--     deriving (Eq, Ord, Read)

-- test :: RegExp
-- test = Cat (Star(Alt (Lit 'a') (Lit 'b'))) (Lit 'c')

    
-- showExpr :: RegExp -> String
-- showExpr expr = showExpr' False expr where
--     add x = "(" ++ x ++ ")"

--     showExpr' :: Bool -> RegExp -> String
--     showExpr' _ (Epsilon) = ""
--     showExpr' _ (Lit x) = show x
--     showExpr' high (Cat l r) = 
--         let result = (showExpr' True l) ++ "" ++ (showExpr' True r)
--         in if high then add result else result
--     showExpr' high (Alt l r) = 
--         let result = (showExpr' True l) ++ "+" ++ (showExpr' True r)
--         in if high then add result else result
--     showExpr' _ (Star x) = (showExpr' True x) ++ "*"

-- instance Show RegExp where 
--     show = showExpr


data Reg = Empty
         | Epsilon 
         | Literal Char 
         | Or Reg Reg 
         | Then Reg Reg 
         | Star Reg
         | Opt Reg           
         | Plus Reg          
           deriving Eq