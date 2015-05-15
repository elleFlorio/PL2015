module Eshs2 where

-- Range (old version)
myRange :: Int -> Int -> [Int]
myRange a b = if a > b
                then error "Low > High"
                else if a < b
                        then a : myRange (a + 1) b
                        else [a]

-- boolean guards
-- Write the myRange function using boolean guards
myRange2 :: Int -> Int -> [Int]

-- Define the myRange function using only one input parameter
myRange3 :: Int -> [Int]

-- case expression
-- Define a function that describes a list (it says if the list is empty, 
-- it's a singleton or a longer list)
describeList :: [a] -> String

-- Define myTakeWhile function that receive as input a condition and a list
-- and copies in a new list the elements of the first one until the 
-- condition became false
myTakeWhile :: (a -> Bool) -> [a] -> [a]

-- Define myFilter function that receives as input a condition and a list
-- and return a new list containing only the elements of the input list
-- that satisfy the condition

-- Folds
-- Define myReverse function that reverse the elements of a list
myReverse :: [a] -> [a]

-- Define mySum function that sums the numbers in a list
mySum :: (Num a)=>[a] -> a

-- Define myFilter2 function using "foldr"
myFilter2 :: (a -> Bool) -> [a] -> [a]

-- Where
-- We want to provide a function that computes the performance of a footbal player.
-- Starting from the number of matches he played and the number of goals 
-- he made (goals / matches),
-- we decide if it's a good player or not
playerEvaluator :: (RealFloat a) => a -> a -> String

-- We are repeating ourselves too m¿any times... let's use a where statement!
playerEvaluator2 :: (RealFloat a) => a -> a -> String

-- list comprehension / let
-- Define a function that creates righth triangles
rightTriangles :: [(Integer, Integer, Integer)]

-- Define a function that orders a list using the quicksort algorithm
quicksort :: (Ord a) => [a] -> [a]

-- typeclass
-- implementation example (from the Prelude:)
-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)
--
-- This means that a) we have to implement both "equals" and "not equals"
-- and b) since "x is equal to y if x is not equal to y" and viceversa,
-- we can just define "equals" or "not equals" and Haskell will infer the
-- other one.

-- Define a TrafficLight that implements Eq and Show typeclasses
data TrafficLight = Red | Yellow | Green

-- Eq

-- Show

-- Binary tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Define a function that creates a singleton tree (one node with 2 empty leaf)
singleton :: a -> Tree a

-- Define a function to insert an element into a tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a

-- Define a function to check if a tree contains an element
treeElem :: (Ord a) => a -> Tree a -> Bool

-- Define a function that sum the elements of a tree composed by numbers
treeSum :: Num a => Tree a -> a

-- Define a function that returns a list containing all the values in a tree
treeValues :: Tree a -> [a]

-- Define the map function on tree
treeMap :: (a -> b) -> Tree a -> Tree b

-- Define the foldl function on tree
treeFoldl :: (a -> b -> a) -> a -> Tree b -> a

-- Define the foldr function on tree
treeFoldr :: (b -> a -> a) -> a -> Tree b -> a

-- Define a filter function on tree
treeFilter :: (a -> Bool) -> Tree a -> [a]




















