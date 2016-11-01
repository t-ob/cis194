module Party where

import Data.Tree
import Employee


-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ empFun) (GL es glFun) = GL (e:es) (empFun + glFun)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL es1 fun1) (GL es2 fun2) = GL (es1 ++ es2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2)
    | fun1 >= fun2 = gl1
    | otherwise    = gl2
