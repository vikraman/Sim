module Sim.AI (play) where

import Data.List
import Sim.Types

play playerA playerB status = case curMoves playerA of [] -> case (validMoves playerA) of (x:xs) -> x
                                                       (z:zs) -> case cas of [y] -> y
                                                                             _ -> if (length a2) > (length b2) then computefirstme cas b2
                                                                                                                  else comp cas a1 a2
                               where a1 = curMoves playerA
                                     a2 = validMoves playerA
                                     b2 = validMoves playerB
                                     cas = computeAS (curMoves playerA) (validMoves playerA) (validMoves playerB)

comp cas@(x:xs) a1 a2 = if (computene (x:a1) (delete x a2))==m then x
                                                else comp xs a1 a2
                     where m = minimum (computefirstne cas a1 a2)

computefirstne :: [Move] -> [Move] -> [Move] -> [Int]
computefirstne [] _ _ = []
computefirstne (x:xs) a1 a2 = (computene (x:a1) (delete x a2)):(computefirstne xs a1 a2)


computefirstme :: [Move] -> [Move] -> Move
computefirstme (x:xs) b2 = if (computeme x b2)==1 then x
                                                  else computefirstme xs b2


computeAS :: [Move] -> [Move] -> [Move] -> [Move]
computeAS a1 a2@(x:xs) b2 = computeminfe a2 (map (computefe a1 a2 b2) a2) (computefe a1 a2 b2 x) []

computeminfe :: [Move] -> [Int] -> Int -> [(Int,Move)] -> [Move]
computeminfe _ [] _ t = map snd t
computeminfe (y:ys) (x:xs) r t | x<r = computeminfe ys xs x [(x,y)]
                               | x==r = computeminfe ys xs x ((x,y):t)
                               | otherwise = computeminfe ys xs r t

computefe :: [Move] -> [Move] -> [Move] -> Move -> Int
computefe a1 a2 b2 e = (computene (e:a1) (delete e a2)) - (computeme e b2)

computeme :: Move -> [Move] -> Int
computeme e b2 | elem e b2 = 1
               | otherwise = 0

computene :: [Move] -> [Move] -> Int
computene a1 [] = 0
computene a1 (x:xs) = do if (triangle x a1) then 1+(computene a1 xs)
                                            else (computene a1 xs)

triangle :: Move -> [Move] -> Bool
triangle curmove [] = False
triangle curmove (x:xs) | formtri curmove x = if (formtri2 curmove x xs) then True
                                                                         else triangle curmove xs
                        | otherwise = triangle curmove xs

formtri2 :: Move -> Move -> [Move] -> Bool
formtri2 curmove _ [] = False
formtri2 curmove@(Line a b) (Line p q) ((Line r s):xs) = if (((b==p) && (a==s)  && (q==r)) || ((b==p) && (a==r)  && (q==s)) || ((b==q) && (a==s)  && (p==r)) || ((b==q) && (a==r) && (p==s)) || ((b==r) && (a==p)  && (q==s)) || ((b==r) && (a==q)  && (p==s)) || ((b==s) && (a==p)  && (q==r)) || ((b==s) && (a==q)  && (p==r)))then  True
                                                                                                                                                                                                                                                                else formtri2 curmove (Line p q) xs

formtri :: Move -> Move -> Bool
formtri (Line p q) (Line r s) | (p==r) || (p==s) || (q==r) || (q==s) = True
                              | otherwise = False

curMove :: Move -> [Move] -> [Move]
curMove mov [] = []
curMove curmove (x:xs) | formtri curmove x = moveaux curmove x xs
                       | otherwise = curMove curmove xs

moveaux :: Move -> Move -> [Move] -> [Move]
moveaux _ _ [] = []
moveaux l1@(Line a b) l2@(Line p q) (l3@(Line r s):xs) = if (((b==p) && (a==s)  && (q==r)) || ((b==p) && (a==r)  && (q==s)) || ((b==q) && (a==s)  && (p==r)) || ((b==q) && (a==r) && (p==s)) ||  ((b==r) && (a==p)  && (q==s)) || ((b==r) && (a==q)  && (p==s)) || ((b==s) && (a==p)  && (q==r)) || ((b==s) && (a==q)  && (p==r))) then [l2,l3]
                                                                                                                                                                                                                                                                else moveaux l1 l2 xs

