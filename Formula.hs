module Formula where

import Data.List(sort, sortBy, partition, groupBy)
import Data.Ord(comparing)
--import Data.Function(id)

data Formula = C Int
             | V String 
             | Add Formula Formula 
             | Mul Formula Formula 
            -- | Exp Formula Int 
             deriving (Eq, Ord)--, Show)

instance Num Formula where
    (+) a b        = Add a b
    (*) a b        = Mul a b
    negate a       = Mul a (C (-1))
    abs _          = error "No method abs on Formulas"
    signum _       = error "No method signum on Formulas"
    fromInteger a  = C (fromIntegral a)

{-instance Eq Formula where
    (==) (V a) (V b)         = a == b
    (==) (C a) (C b)         = a == b
    (==) (Add a b) (Add c d) = a == c && b == d || a == d && b == c
    (==) (Mul a b) (Mul c d) = a == c && b == d || a == d && b == c
    (==) _ _                 = False-}

instance Show Formula where
    show (C a)     = show a
    show (V x)     = x
    show (Mul f@(Add _ _) g@(Add _ _)) = "(" ++ show f ++ ")" ++ "(" ++ show g ++ ")"
    show (Mul f@(Add _ _) g) = "(" ++ show f ++ ")" ++ show g
    show (Mul f g@(Add _ _)) = show f ++ "(" ++ show g ++ ")"
    show (Mul f g) = show f ++ show g
    show (Add f g) = show f ++ " + " ++ show g
    {-show (Add a b) = "(" ++ show a ++ ")" ++ " + " ++ "(" ++ show b  ++ ")"
    show (Mul a b) = "(" ++ show a ++ ")" ++ " * " ++ "(" ++ show b  ++ ")"-}

toPolynomial :: Int -> Formula -> Formula
toPolynomial k = applyToFormula (subst01 . evalConstants . constantsMod k) . collect . evalConstants . lassoc . dist

polarize :: Formula -> [(String, Formula)] -> Formula
polarize f ds = mapFormula (polarize' ds) f where
    polarize' :: [(String, Formula)] -> Formula -> Formula
    polarize' ds vx@(V x) = case lookup x ds of
                              Just g  -> Add (V $ "(" ++ (show $ subst01 $ vx + g) ++ ")") (-g)
                              Nothing -> vx
    polarize' ds a        = a

collect :: Formula -> Formula
collect = fromList Add . map pairs . groupBy snds . sortBy (comparing snd) . map (norm . partition isConst) . toSumOfProduct where
    norm (a,b) = (product a, sort b)
    snds a b   = snd a == snd b
    pairs l    = subst01 $ evalConstants $ fromList Mul ((sum $ map fst l):(snd $ head l))

isConst (C a)   = True
isConst _       = False
isVar (V a)     = True
isVar _         = False
isMul (Mul a b) = True
isMul _         = False
isAdd (Add a b) = True
isAdd _         = False

fromConst (C a) = a
fromVar   (V a) = a

commutativeAdd :: Formula -> Formula
commutativeAdd (Add a b) = Add b a

mapFormula :: (Formula -> Formula) -> Formula -> Formula
mapFormula f (Add a b) = f (Add (mapFormula f a) (mapFormula f b))
mapFormula f (Mul a b) = f (Mul (mapFormula f a) (mapFormula f b))
mapFormula f a         = f a

applyToFormula :: (Formula -> Formula) -> Formula -> Formula
applyToFormula f x | x == f x  = x
                   | otherwise = applyToFormula f (f x)

mapAdd :: (Formula -> Formula) -> Formula -> Formula
mapAdd f (Add a b) = f $ Add (mapAdd f a) (mapAdd f b)
mapAdd f (Mul a b) = Mul (mapAdd f a) (mapAdd f b)
mapAdd f a         = a

subst01 :: Formula -> Formula
subst01 = applyToFormula $ mapFormula subst01' where
    subst01' g@(Mul (C a) b) | a == 0    = C 0
                             | a == 1    = b
                             | otherwise = g
    subst01' g@(Mul b (C a)) | a == 0    = C 0
                             | a == 1    = b
                             | otherwise = g
    subst01' g@(Add (C a) b) | a == 0    = b
                             | otherwise = g
    subst01' g@(Add b (C a)) | a == 0    = b
                             | otherwise = g
    subst01' a               = a

dist :: Formula -> Formula
dist = applyToFormula distributive

distributive :: Formula -> Formula
distributive = mapFormula distributive' where
    distributive' :: Formula -> Formula
    distributive' (Mul (Add a b) c) = Add (Mul a c) (Mul b c)
    distributive' (Mul a (Add b c)) = Add (Mul a b) (Mul a c)
    distributive' a                 = a

lassoc :: Formula -> Formula
lassoc = applyToFormula lassociative

lassociative :: Formula -> Formula
lassociative = mapFormula lassociative' where
    lassociative' :: Formula -> Formula
    lassociative' (Mul a (Mul b c)) = Mul (Mul a b) c
    lassociative' (Add a (Add b c)) = Add (Add a b) c
    lassociative' a = a

constantsMod :: Int -> Formula -> Formula
constantsMod k = mapFormula constantsMod' where
    constantsMod' (C a) = C (a `mod` k)
    constantsMod' x     = x

evalConstants :: Formula -> Formula
evalConstants f@(Add a b) = evalAdd $ toList f where
     evalAdd fs = let (cs, ls) = partition isConst (map evalConstants fs)
                    in fromList Add ([C (sum $ map fromConst cs)] ++ ls)
evalConstants f@(Mul a b) = evalMul $ toList f where
    evalMul fs = let (cs, ls) = partition isConst (map evalConstants fs)
                    in fromList Mul ([C (product $ map fromConst cs)] ++ ls)
evalConstants a           = a

fromList :: (Formula -> Formula -> Formula) -> [Formula] -> Formula
fromList f (x:[])   = x
fromList f (x:y:[]) = f x y
fromList f (x:y:xs) = f (f x y) (fromList f xs)

toAddList (Add a1 b1) = toAddList a1 ++ toAddList b1
toAddList a1          = [a1]
toMulList (Mul a1 b1) = toMulList a1 ++ toMulList b1
toMulList a1          = [a1]

toList :: Formula -> [Formula]
toList f = case f of
             (Add a b) -> toAddList a ++ toAddList b
             (Mul a b) -> toMulList a ++ toMulList b
             a         -> [a]

toSumOfProduct :: Formula -> [[Formula]]
toSumOfProduct = map toMulList . toAddList

mapMul :: (Formula -> Formula) -> Formula -> Formula
mapMul f (Mul a b) = f $ Mul (mapMul f a) (mapMul f b)
mapMul f (Add a b) = Add (mapMul f a) (mapMul f b)
mapMul f a         = a

toPNF :: Formula -> Formula
toPNF (Mul a (Add b c)) = Add (toPNF $ Mul a b) (toPNF $ Mul a c)
toPNF (Mul (V a) b)     | b == 1  = V a
                        | otherwise = Mul (toPNF b) (V a)
toPNF (Mul (C a) (C b)) = C (a*b)
toPNF (Mul (C a) b)     | a == 1    = toPNF b
                        | a == 0    = C 0
                        | otherwise = Mul (C a) (toPNF b)

(x,y,z,w,t) = (V "x", V "y", V "z", V "w", V "t")