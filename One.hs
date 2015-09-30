module One where

import Data.List

length' [] = 0
length' (x:xs) = 1 + (length' xs)

take' a (x:xs)
  | a == 0 = []
  | a >= length' (x:xs) = (x:xs)
  | otherwise = [x] ++ (take' (a-1) (xs))

null' [] = True
null' (x:xs) = False

drop' a (x:xs)
  | a == 0 = (x:xs)
  | otherwise = drop' (a-1) xs

fst' (a,b) = a
snd' (a,b) = b

product' [] = 1
product' (x:xs) = x * product' xs

replicate' a (x:xs)
  | a == 0 = []
  | otherwise = (x:xs) : (replicate' (a-1) (x:xs) )

zip' _ [] = []
zip' [] (x:xs) = []
zip' (x:xs) (a:b) = (x,a) : zip' xs b

head' (x:xs) = x

tail' (x:xs) = xs




intersperse' _ [] = []
intersperse' a (x:xs) = [x,a] ++ intersperse' a xs

intercalate' (x:xs) [(a:b)] = (a:b)
