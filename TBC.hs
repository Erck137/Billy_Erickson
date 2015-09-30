module TBC where

import Data.List

-- 1.a

null' [] = True
null' (x:xs) = False

--pembatas

take' a (x:xs)
  | a == 0 = []
  | otherwise = [x] ++ take' (a-1) (xs)

--pembatas

drop' a (x:xs)
  | a == 0 = (x:xs)
  | otherwise = drop' (a-1) xs

--pembatas

fst' (a,b) = a

--pembatas

snd' (a,b) = b

--pembatas

map' f [] = []
map' f (x:xs) = f x : map' f xs

--pembatas

filter' _ [] = []
filter' a (x:xs)
  | a x == True = [x] ++ filter' a xs
  | otherwise = filter' a xs

--pembatas

delete' _ [] = []
delete' a (x:xs)
  | a == x = xs
  | otherwise = [x] ++ (delete' a xs)

--pembatas

deleteAll' _ [] = []
deleteAll' a (x:xs)
  | a == x = deleteAll' a xs
  | otherwise = [x] ++ deleteAll' a xs

--pembatas

foldl' x = x

--pembatas

foldl1s' f [x] = x
foldl1s' f (x:xs) = f (x) (foldl1s' f xs)

--pembatas

zip' _ [] = []
zip' [] (x:xs) = []
zip' (x:xs) (a:b) = (x,a) : zip' xs b

--pembatas

zipWith' f [] [] = []
zipWith' f (x:xs) [] = []
zipWith' f [] (a:as) = []
zipWith' f (x:xs) (a:as) = (f x a) : zipWith' f xs as

--pembatas

nth' 0 (x:xs) = x
nth' a (x:xs) = nth' (a-1) xs

--pembatas

scanls' f a [] = []
scanls' f a (x:xs) = (f a x) : (scanls' f a xs)

--pembatas

scanl1s' f [] = []

--pembatas

elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs

--pembatas

notElem' _ [] = True
notElem' a (x:xs)
  | a == x = False
  | otherwise = notElem' a xs

--pembatas

head' (x:xs) = x

--pembatas

length' [] = 0
length' (x:xs) = 1 + (length' xs)

--pembatas

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--pembatas

last' [x] = x
last' (x:xs) = last' xs

--pembatas

tail' (x:xs) = xs

--pembatas

inits' x = x

--pembatas

max' a b
  | a > b = a
  | otherwise = b


--pembatas

min' a b
  | a < b = a
  | otherwise = b


--pembatas

concat' [] = []
concat' [(x:xs)] = (x:xs)

--pembatas

intersperse' _ [] = []
intersperse' a (x:xs) = [x,a] ++ intersperse' a xs

--pembatas

intercalate' (x:xs) [(a:b)] = (a:b)

--pembatas

and' [] = True
and' (x:xs)
  | x == False = False
  | otherwise = and' xs

--pembatas

or' [] = True
or' (x:xs)
  | x == True = True
  | otherwise = or' xs

--pembatas

zip3' [] [] [] = []
zip3' (x:xs) [] [] = []
zip3' [] (a:as) [] = []
zip3' [] [] (b:bs) = []
zip3' (x:xs) (a:as) [] = []
zip3' [] (a:as) (b:bs) = []
zip3' (x:xs) [] (b:bs) = []
zip3' (x:xs) (a:as) (b:bs) = (x,a,b) : zip3' xs as bs

--pembatas

sum' [] = 0
sum' (x:xs) = x + sum' xs

--pembatas

product' [] = 1
product' (x:xs) = x * product' xs

--pembatas

words' x = x

--pembatas

lines' x = x

--pembatas

unlines' x = x

--pembatas

unwords' x = x

--pembatas

takeWhile' f (x:xs)
  | f x == False = []
  | otherwise = [x] ++ takeWhile' f xs

--pembatas

dropWhile' _ [] = []
dropWhile' f (x:xs)
  | f x == False = (x:xs)
  | otherwise = dropWhile' f xs

--pembatas

concatMap' x = x

--pembatas

all' _ [] = True
all' a (x:xs)
  | a x == False = False
  | otherwise = all' a (xs)

--pembatas

any' _ [] = False
any' a (x:xs)
  | a x == True = True
  | otherwise = any' a xs

--pembatas

insert' a [] = [a]
insert' a (x:xs)
  | a == x = a : (x:xs)
  | otherwise = [x] ++ insert' a xs

--pembatas

zipWith3' f [] [] [] = []
zipWith3' f (x:xs) [] [] = []
zipWith3' f [] (y:ys) [] = []
zipWith3' f [] [] (z:zs) = []
zipWith3' f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3' f xs ys zs

--pembatas

-- 1.b

nub' [] = []
nub' (x:xs) = [x] ++ nub' (deleteAll' x (x:xs))

--pembatas

sort' x = x

--pembatas

minimum' [x] = x
minimum' (x:xs) = min' x (minimum' xs)

--pembatas

maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

--pembatas

init' (a:b) = take' (length' (a:b)-1) (a:b)

--pembatas

tails' x = x

--pembatas

union' x = x

--pembatas

intersect' x = x

--pembatas

group' x = x
--pembatas

splitAt' _ [] = ([],[])
splitAt' a (x:xs)
  | a == 0 = ([], (x:xs))
splitAt' a (x:xs) = ((take a (x:xs)), (drop a (x:xs)))

--pembatas

partition' x = x

--pembatas


replicate' a (x:xs)
  | a == 0 = []
  | otherwise = (x:xs) : (replicate' (a-1) (x:xs) )


--pembatas
-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
