{-# LANGUAGE FlexibleInstances #-}

data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node l x r) = (flatten l) ++ [x] ++ (flatten r)

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT

eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit i)     = f i
exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)

eval2 :: ExprT -> Integer
eval2 = exprTFold id (+) (*)

numLiterals :: ExprT -> Integer
numLiterals = exprTFold (const 1) (+) (+)

newtype AddBool = AddBool Bool
  deriving (Eq, Show)

instance Monoid AddBool where
    mempty  = AddBool False
    mappend (AddBool True) (AddBool True) = AddBool False
    mappend (AddBool False) (AddBool False) = AddBool False
    mappend _ _ = AddBool True

instance Monoid (a -> a) where
    mempty = id
    mappend = (.)
