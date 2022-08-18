{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

newtype MyNum = MyNum Int -- deriving Show via Int

-- deriving instance Show MyNum
deriving via Int instance Show MyNum

main :: IO ()
main = do
  print $ MyNum 5


--  | Shape Functors, Recursion Schemes, Catamorphism
  
data MyList a = Nil | Cons a (MyList a)

map' :: (t -> a) -> MyList t -> MyList a
map' f Nil = Nil
map' f (Cons x xs) = f x `Cons` map' f xs

filter' :: (a -> Bool) -> MyList a -> MyList a
filter' test Nil = Nil
filter' test (Cons x xs) = if test x then Cons x (filter' test xs) else filter' test xs

data ListF a r
  = NilF
  | ConsF a r

instance Functor (ListF a) where
  fmap :: (x -> y) -> ListF a x -> ListF a y
  fmap f NilF = NilF
  fmap f (ConsF a r) = ConsF a (f r)

newtype Fix f = Fix { unFix :: f (Fix f) }

type List a = Fix (ListF a)

type EmptyList = List ()

emptyList :: List a
emptyList = Fix NilF

listInt :: Fix (ListF Integer)
listInt = Fix (ConsF 0 (Fix (ConsF 1 (Fix NilF))))

foldr' :: (a -> b -> b) -> b -> MyList a -> b
foldr' f b Nil = b
foldr' f b (Cons x xs) = f x $ foldr' f b xs

gfold :: (ListF a b -> b) -> Fix (ListF a) -> b
gfold f (Fix NilF) = f NilF
gfold f (Fix (ConsF x xs)) = f (ConsF x (gfold f xs))

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata = undefined









-- cata f = f . fmap (cata f) . unFix