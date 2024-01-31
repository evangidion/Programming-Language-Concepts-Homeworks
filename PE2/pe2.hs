module PE2 where

import Text.Printf

type Point = (Int, Int)
type Dimensions = (Int, Int)
type Vector = (Int, Int)

getRounded :: Double -> Double
getRounded x = read s :: Double
               where s = printf "%.2f" x

castIntToDouble x = (fromIntegral x) :: Double

-------------------------------------------------------------------------------------------
----------------------- DO NOT CHANGE ABOVE OR FUNCTION SIGNATURES-------------------------
------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------------------------
getVector :: String -> Vector
getVector d | d == "North" = (0, 1)
            | d == "South" = (0, -1)
            | d == "East" = (1, 0)
            | d == "West" = (-1, 0)
            | d == "Stay" = (0 ,0)
            | d == "NorthEast" = (1, 1)
            | d == "NorthWest" = (-1, 1)
            | d == "SouthEast" = (1, -1)
            | d == "SouthWest" = (-1, -1)

-------------------------------------------------------------------------------------------------------------------------------
getAllVectors :: [String] -> [Vector]
getAllVectors [] = []
getAllVectors (l:ls) = getVector l:getAllVectors ls

-------------------------------------------------------------------------------------------------------------------------------

producePath :: Point -> [String] -> [Point]
producePath (x, y) (l:ls) = (x, y):producePath2 (x, y) (l:ls)


producePath2 :: Point -> [String] -> [Point]
producePath2 _ [] = []
producePath2 (a, b) (l:ls) = (a + fst (getVector l), b + snd (getVector l)):producePath2 (a + fst (getVector l), b + snd (getVector l)) ls

-------------------------------------------------------------------------------------------------------------------------------
takePathInArea :: [Point] -> Dimensions -> [Point]
takePathInArea [] _ = []
takePathInArea (l:ls) (bx, by) | fst (l) >= 0 && snd (l) >= 0 && fst (l) < bx && snd (l) < by = l:takePathInArea ls (bx, by)
                               | otherwise = []

-------------------------------------------------------------------------------------------------------------------------------

remainingObjects :: [Point] -> Dimensions -> [Point] -> [Point]
remainingObjects _ _ [] = []
remainingObjects (l:ls) (bx, by) (s:sl) | elem s (takePathInArea (l:ls) (bx, by)) = remainingObjects (l:ls) (bx, by) sl
                                        | otherwise = s:remainingObjects (l:ls) (bx, by) sl

-------------------------------------------------------------------------------------------------------------------------------
averageStepsInSuccess :: [[Point]] -> Dimensions -> [Point] -> Double
averageStepsInSuccess paths border objects = getRounded (castIntToDouble (helper2 (helper paths border objects)) / castIntToDouble (length (helper paths border objects)))

helper :: [[Point]] -> Dimensions -> [Point] -> [[Point]]
helper [] _ _ = []
helper (l:ls) (x, y) (s:sl) | (length l == length (takePathInArea l (x, y))) && (remainingObjects l (x, y) (s:sl) == []) = l:helper ls (x,y) (s:sl)
                            | otherwise = helper ls (x,y) (s:sl)
                            
helper2 :: [[Point]] -> Int
helper2 [] = 0
helper2 (l:ls) = length l + helper2 ls