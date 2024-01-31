module PE4 where

import Data.List
import Data.Maybe

data Room = SeedStorage Int
          | Nursery Int Int
          | QueensChambers
          | Tunnel
          | Empty
          deriving Show

data Nestree = Nestree Room [Nestree] deriving Show

---------------------------------------------------------------------------------------------
------------------------- DO NOT CHANGE ABOVE OR FUNCTION SIGNATURES-------------------------
--------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
--------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
---------------------------------------------------------------------------------------------

-- Note: undefined is a value that causes an error when evaluated. Replace it with
-- a viable definition! Name your arguments as you like by changing the holes: _

---------------------------------------------------------------------------------------------

-- Q1: Calculate the nutrition value of a given nest.
nestNutritionValue :: Nestree -> Int
--nestNutritionValue (Nestree Empty []) = 0
--nestNutritionValue (Nestree QueensChambers []) = 0
--nestNutritionValue (Nestree Tunnel []) = 0
--nestNutritionValue (Nestree (SeedStorage a) _) = 3*a
--nestNutritionValue (Nestree (Nursery x y) _) = 7*x + 10*y
nestNutritionValue nestle = nestNutritionHelper nestle

nestNutritionHelper :: Nestree -> Int
--nestNutritionHelper (Nestree Empty []) = 0
--nestNutritionHelper (Nestree QueensChambers []) = 0
--nestNutritionHelper (Nestree Tunnel []) = 0
nestNutritionHelper (Nestree Empty sl) = nestNutritionHelper2 sl
nestNutritionHelper (Nestree QueensChambers sl) = nestNutritionHelper2 sl
nestNutritionHelper (Nestree Tunnel sl) = nestNutritionHelper2 sl
nestNutritionHelper (Nestree (SeedStorage a) sl) = 3*a + nestNutritionHelper2 sl
nestNutritionHelper (Nestree (Nursery x y) sl) = 7*x + 10*y + nestNutritionHelper2 sl

nestNutritionHelper2 :: [Nestree] -> Int
nestNutritionHelper2 [] = 0
nestNutritionHelper2 (l:ls) = nestNutritionHelper l + nestNutritionHelper2 ls


-- Q2: Calculate the nutrition value of each root-to-leaf path.
pathNutritionValues :: Nestree -> [Int]
pathNutritionValues (Nestree r []) = [roomHelper r]
--pathNutritionValues (Nestree r []) = ultimateHelper (Nestree r [])
pathNutritionValues (Nestree r (sl:[])) = (pathNutritionHelper sl + roomHelper r):[]
--pathNutritionValues (Nestree r (sl:[])) = (ultimateHelper sl + roomHelper r):[]
--pathNutritionValues (Nestree r (l:ls)) = (pathNutritionHelper l + roomHelper r):(roomHelper r + pathNutritionHelper2 ls):[] 
pathNutritionValues (Nestree r (l:ls)) = (pathNutritionHelper l + roomHelper r):pathNutritionValues (Nestree r ls)
--pathNutritionValues (Nestree r (l:ls)) = (pathNutritionHelper l + roomHelper r):pathNutritionValues l++pathNutritionValues (Nestree r ls)
--pathNutritionValues (Nestree r (l:ls)) = ultimateHelper (Nestree r (l:ls)):pathNutritionValues (Nestree r ls)
--pathNutritionValues (Nestree r (l:ls)) = ultimateHelper (Nestree r (l:ls)):pathNutritionValues l ++pathNutritionValues  (Nestree r ls)



pathNutritionHelper :: Nestree -> Int
pathNutritionHelper (Nestree Empty []) = 0
pathNutritionHelper (Nestree QueensChambers []) = 0
pathNutritionHelper (Nestree Tunnel []) = 0
pathNutritionHelper (Nestree (SeedStorage b) []) = 3*b
pathNutritionHelper (Nestree (Nursery t z) []) = 7*t + 10*z
pathNutritionHelper (Nestree Empty sl) = pathNutritionHelper2 sl
pathNutritionHelper (Nestree QueensChambers sl) = pathNutritionHelper2 sl
pathNutritionHelper (Nestree Tunnel sl) = pathNutritionHelper2 sl
pathNutritionHelper (Nestree (SeedStorage a) sl) = 3*a + pathNutritionHelper2 sl
pathNutritionHelper (Nestree (Nursery x y) sl) = 7*x + 10*y + pathNutritionHelper2 sl

pathNutritionHelper2 :: [Nestree] -> Int
pathNutritionHelper2 [] = 0
pathNutritionHelper2 ((Nestree Empty []):ls) = 0
pathNutritionHelper2 ((Nestree Tunnel []):ls) = 0
pathNutritionHelper2 ((Nestree QueensChambers []):ls) = 0
pathNutritionHelper2 ((Nestree (SeedStorage a) []):ls) = 3*a
pathNutritionHelper2 ((Nestree (Nursery x y) []):ls) = 7*x + 10*y
pathNutritionHelper2 (l:ls) = pathNutritionHelper l + pathNutritionHelper2 ls

roomHelper :: Room -> Int
roomHelper (SeedStorage a) = 3*a
roomHelper (Nursery x y) = 7*x + 10*y
roomHelper c = 0

ultimateHelper :: Nestree -> Int
ultimateHelper (Nestree r []) = roomHelper r
--ultimateHelper (Nestree r (sl:[])) = pathNutritionHelper sl + roomHelper r
ultimateHelper (Nestree r (l:ls)) = roomHelper r + ultimateHelper l



-- Q3: Find the depth of the shallowest tunnel, if you can find one.
shallowestTunnel :: Nestree -> Maybe Int
shallowestTunnel _ = undefined 

-- Q4: Find the path to the Queen's Chambers, if such a room exists.
pathToQueen :: Nestree -> Maybe [Room]
pathToQueen _ = undefined 

-- Q5: Find the quickest depth to the Queen's Chambers, including tunnel-portation :)
quickQueenDepth :: Nestree -> Maybe Int
quickQueenDepth _ = undefined 

-- Example nest given in the PDF.
exampleNest :: Nestree
exampleNest = 
  Nestree Tunnel [
    Nestree (SeedStorage 15) [
      Nestree (SeedStorage 81) []
    ],
    Nestree (Nursery 8 16) [
      Nestree Tunnel [
        Nestree QueensChambers [
          Nestree (Nursery 25 2) []
        ]
      ]
    ],
    Nestree Tunnel [
      Nestree Empty [],
      Nestree (SeedStorage 6) [
        Nestree Empty [],
        Nestree Empty []
      ]
    ]
  ]

-- Same example tree, with tunnels replaced by Empty
exampleNestNoTunnel :: Nestree
exampleNestNoTunnel = 
  Nestree Empty [
    Nestree (SeedStorage 15) [
      Nestree (SeedStorage 81) []
    ],
    Nestree (Nursery 8 16) [
      Nestree Empty [
        Nestree QueensChambers [
          Nestree (Nursery 25 2) []
        ]
      ]
    ],
    Nestree Empty [
      Nestree Empty [],
      Nestree (SeedStorage 6) [
        Nestree Empty [],
        Nestree Empty []
      ]
    ]
  ]
