module Main where

import FunctionVectors

{-allVectors :: Int -> Int -> [[Int]]
allVectors k n = sequence q
                    where q = take n $! repeat [1..k-1]-}
            
main =  do
        putStrLn "Enter prime k: "
        k <- getLine
        print $ findSelfExpr ((read k) :: Integer)
        --main

--main = print $ length $ nonConstNormalVectors 5 11
