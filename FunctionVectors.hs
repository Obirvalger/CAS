module FunctionVectors where

--import Math.NumberTheory.Moduli(invertMod)
import Data.Maybe(fromJust)
import Data.List(nub, permutations)
--import Control.Monad(ap)

--k = 5

f = [1,1,4,4]

g = [1,4,4,1]

--functionNames = ["f"]++(map (\c->['s',c]) ['1'..'4'])++["g"]

--functionVectors = (map (snd . normalize' 5 . modSum 5 f . modMul 5 g) [0..4])++[[1,4,4,1]]

--invert :: Integer -> Integer
--invert k = fromJust . flip invertMod k

{-euclid :: Integral a => a -> a -> a
euclid x y = f 1 0 x 0 1 y where
    f a b g u v w | w == 0    = mod a y
                  | otherwise = f u v w (a-q*u) (b-q*v) (g-q*w)
                                where q = div g w
 
invert :: Integral a => a -> a -> a
invert m x | gcd x m == 1 = euclid x m
           | otherwise    = error "divisor must be coprime to modulus"-}

{-invert k x = fromJust $ lookup x inverts
        where inverts = map (\x->(x, invert' k x)) [1..k]-}

--invert = (fromJust .) . flip lookup . ap (map . ap (,) . invert') (enumFromTo 1)

invert' k 1 = 1
invert' k p = (n * k + 1) `div` p
    where n = p - invert' p (k `mod` p)

--modSum :: Integral a => [a] -> [a] -> [a]
modSum k xs ys = zipWith (\x y -> mod (x + y) k) xs ys

modMul k xs m = map (\x -> mod (x*m) k) xs
--modMul xs m = map ((`mod` k) . (m*)) xs

--normalize l@(0:x:xs) = (x, map (\y->mod (y * (invert k x)) k) l)
--normalize l@(x:xs)   = (x, map (\y->mod (y * (invert k x)) k) l)
--normalize l          = error "Could not normalize this vector"

normalize' invs k l@(x:xs)   = (x, map (\y->mod (y * (invert k x)) k) l)
    where invert k x = fromJust $ lookup x invs
--normalize' invs k l          = error "Could not normalize this vector"

rol :: Integral b => [a] -> b -> [a]
rol xs n = bs ++ es where (es, bs) = splitAt (mod (fromIntegral n) (length xs)) xs

tr xs ys = flip lookup (zip xs ys)

--trVS = fromJust . tr functionVectors functionNames . snd . normalize' 5

--trSV = snd . normalize' 5 . fromJust . tr functionNames functionVectors

decompozition' invs k h = map (snd . normalize' invs k . rol h) [0..k-1]

allVectors k n = sequence $ take (fromIntegral n) $ repeat [1..k-1]

allNormalVectors k n = map (1:) $ allVectors k (n-1)

nonConstVectors k n = filter ((/=) 1 . length . nub) $ allVectors k n

nonConstNormalVectors k n = filter ((/=) 1 . length . nub ) $ allNormalVectors k n

findSelfExpr k = filter p $ nonConstNormalVectors k (k-1)
                    where p = (\x->(==) 1 $  length $ nub $ decompozition' invs k x)
                          invs = map (\x->(x, invert' k x)) [1..k]
                

normPerms k = map (1:) $ permutations [2..k-1]

deg k n | gcd k n /= 1 = 0
        | otherwise    = deg' k n n 1
        where
            deg' k n 1 d = d
            deg' k n m d = deg' k n (mod (n*m) k) (d+1)
            
