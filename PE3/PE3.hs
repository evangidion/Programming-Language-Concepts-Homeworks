module PE3 where

-- can use these if you want to...
import Data.List
import Data.Maybe

data Private = Private { idNumber :: Int, height :: Int, timeToSwap :: Int } deriving Show

data Cell = Empty | Full Private deriving Show

type Area = [[Cell]] 

---------------------------------------------------------------------------------------------
------------------------- DO NOT CHANGE ABOVE OR FUNCTION SIGNATURES-------------------------
--------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
--------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
---------------------------------------------------------------------------------------------


-- Note: undefined is a value that causes an error when evaluated. Replace it with
-- a viable definition! Name your arguments as you like by changing the holes: _


--------------------------
-- Part I: Warming up with Abstractions

-- bubbleHumans: Applying a parallel bubble step on integers.
bubbleHumans :: [Int] -> [Int]
bubbleHumans [] = []
bubbleHumans (sl:[]) = [sl]
--bubbleHumans sl | length(sl) == 1 = sl
bubbleHumans (l:ls:lz) | ls > l = ls:l:bubbleHumans lz
                       | otherwise = l:bubbleHumans (ls:lz)

-- bubblePrivates: The same thing, but on privates with a key function and an asc/desc option.
bubblePrivates :: (Private -> Int) -> Bool -> [Private] -> [Private]
bubblePrivates _ _ [] = []
bubblePrivates _ _ (sl:[]) = [sl]
--bubblePrivates _ _ sl | length(sl) == 1 = sl
bubblePrivates key bool (l:ls:lz) | bool == True && key ls > key l = ls:l:bubblePrivates key bool lz
                                  | bool == True && key ls <= key l = l:bubblePrivates key bool (ls:lz)
                                  | bool == False && key ls < key l = ls:l:bubblePrivates key bool lz
                                  | otherwise = l:bubblePrivates key bool (ls:lz)

-- sortPrivatesByHeight: Full sort via multiple bubble steps, calculate the sorting time too!
sortPrivatesByHeight :: [Private] -> ([Private], Int)
sortPrivatesByHeight [] = ([], 0)
sortPrivatesByHeight (sl:[]) = ([sl], 0)
sortPrivatesByHeight ls = (bubbleSortPrivates ls (2*(length ls)), totalSortingTime ls (2*(length ls)))

bubblePrivatesByHeight :: [Private] -> [Private]
bubblePrivatesByHeight [] = []
bubblePrivatesByHeight (sl:[]) = [sl]
bubblePrivatesByHeight (l:ls:lz) | height ls > height l = ls:l:bubblePrivatesByHeight lz
                                 | otherwise = l:bubblePrivatesByHeight (ls:lz)

bubblePrivatesTime :: [Private] -> Int
bubblePrivatesTime [] = 0
bubblePrivatesTime (sl:[]) = 0
bubblePrivatesTime (l:ls:lz) | height ls > height l = max (max (timeToSwap l) (timeToSwap ls)) (bubblePrivatesTime lz) 
                             | otherwise = bubblePrivatesTime (ls:lz)
                                 
                                 
sortingTime :: [Private] -> Int
sortingTime [] = 0
sortingTime (sl:[]) = 0
sortingTime (l:ls:lz) | height ls > height l && timeToSwap ls >= timeToSwap l = max (timeToSwap ls) (sortingTime lz) 
                      | height ls > height l && timeToSwap ls < timeToSwap l = max (timeToSwap l) (sortingTime lz)
                      | otherwise = sortingTime (ls:lz)
                      
totalSortingTime :: [Private] -> Int -> Int
totalSortingTime [] _ = 0
totalSortingTime (sl:[]) _ = 0
totalSortingTime ls a | a >= 0 = sortingTime ls + totalSortingTime (bubblePrivatesByHeight ls) (a-1) 
                      | otherwise = 0
                       
bubbleSortPrivates :: [Private] -> Int -> [Private]
bubbleSortPrivates [] _ = []
bubbleSortPrivates (sl:[]) _ = [sl]
bubbleSortPrivates ls a | a >= 0 = bubbleSortPrivates (bubblePrivatesByHeight ls) (a-1)
                        | otherwise = ls
                        
                        
--------------------------
-- Part II: Squeezing your Brain Muscles

-- ceremonialFormation: Sorting again, but with multiple files. Don't forget the time!
ceremonialFormation :: Area -> (Area, Int)
ceremonialFormation a = (bubbleSortAllLines a, totalTimeAllLines a)

patternMatchingTime :: Cell -> Int
patternMatchingTime Empty = 0
patternMatchingTime (Full x) = timeToSwap x

patternMatchingHeight ::  Cell -> Int
patternMatchingHeight Empty = 0
patternMatchingHeight (Full x) = height x

bubbleCellsByHeight :: [Cell] -> [Cell]
bubbleCellsByHeight [] = []
bubbleCellsByHeight (sl:[]) = [sl]
bubbleCellsByHeight (l:ls:lz) | patternMatchingHeight ls > patternMatchingHeight l = ls:l:bubbleCellsByHeight lz
                              | otherwise = l:bubbleCellsByHeight (ls:lz)
                              
bubbleSortCells :: [Cell] -> Int -> [Cell]
bubbleSortCells [] _ = []
bubbleSortCells (sl:[]) _ = [sl]
bubbleSortCells ls a | a >= 0 = bubbleSortCells (bubbleCellsByHeight ls) (a-1)
                     | otherwise = ls

bubbleSortAllLines :: Area -> Area
bubbleSortAllLines [] = []
bubbleSortAllLines (l:ls) = (bubbleSortCells l (2*length l)):bubbleSortAllLines ls

sortingTimeCells :: [Cell] -> Int
sortingTimeCells [] = 0
sortingTimeCells (sl:[]) = 0
sortingTimeCells (l:ls:lz) | patternMatchingHeight ls > patternMatchingHeight l && patternMatchingTime ls >= patternMatchingTime l = max (patternMatchingTime ls) (sortingTimeCells lz)
                           | patternMatchingHeight ls > patternMatchingHeight l && patternMatchingTime ls < patternMatchingTime l = max (patternMatchingTime l) (sortingTimeCells lz)
                           | otherwise = sortingTimeCells (ls:lz)
                           
totalSortingTimeCells :: [Cell] -> Int -> Int
totalSortingTimeCells [] _ = 0
totalSortingTimeCells (sl:[]) _ = 0
totalSortingTimeCells ls a | a >= 0 = sortingTimeCells ls + totalSortingTimeCells (bubbleCellsByHeight ls) (a-1)
                           | otherwise = 0

totalTimeAllLines :: Area -> Int
totalTimeAllLines [] = 0
totalTimeAllLines (l:ls) = max (totalSortingTimeCells l (2*length l)) (totalTimeAllLines ls)

-- swapPrivates: Swap two arbitrary privates by ID if they are in the area. Big ouch!
swapPrivates :: Int -> Int -> Area -> Area
swapPrivates _ _ _ = undefined 

-- Best of luck to you, friend and colleague!

