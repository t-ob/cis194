module Party where

import Data.Tree
import Data.List
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


-- Exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node label forest) = f label $ map (treeFold f) forest


-- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss guestListPairs = (glCons boss $ mconcat $ map snd guestListPairs
                                , mconcat $ map fst guestListPairs)


-- Exercise 4

maxFun :: Tree Employee -> GuestList
-- maxFun company@(Node {rootLabel = boss}) = uncurry moreFun $ treeFold nextLevel company
maxFun company = uncurry moreFun $ treeFold nextLevel company


-- Exercise 5

instance Ord Employee where
    (<=) (Emp name1 _) (Emp name2 _) = name1 <= name2

getName :: Employee -> String
getName (Emp name _) = name

asString :: GuestList -> String
asString (GL employees totalFun) = "Total fun: " ++ show totalFun ++ "\n" ++
                                   unlines (sort (map getName employees))

main :: IO ()
main = readFile "company.txt"
       >>= putStrLn . asString . maxFun . read
