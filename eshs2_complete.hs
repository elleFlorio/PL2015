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
myRange2 a b
    | a > b = error "Low > High"
    | a == b = [a]
    | a < b = a : myRange2 (a + 1) b

-- Define the myRange function using only one input parameter
myRange3 :: Int -> [Int]
myRange3 a = a : myRange3 (a + 1) -- WARNING: infinite list, use "take"! (e.g. take 3 $ myRange3 3)

-- case expression
-- Define a function that describes a list (it says if the list is empty, it's a singleton or a longer list)
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of	[] -> "empty."  
                                              	[x] -> "a singleton list."   
                                              	xs -> "a longer list."  

-- Define myTakeWhile function that receive as input a condition and a list
-- and copies in a new list the elements of the first one until the 
-- condition became false
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile f (x : xs)
    | f x = x : myTakeWhile f xs
    | otherwise = []

-- Define myFilter function that receives as input a condition and a list
-- and return a new list containing only the elements of the input list
-- that satisfy the condition
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
    | f x = x : myFilter f xs
    | otherwise = myFilter f xs

-- Folds
-- Define myReverse function that reverse the elements of a list
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []

-- Define mySum function that sums the numbers in a list
mySum :: (Num a)=>[a] -> a
mySum = foldl (\acc x -> acc + x) 0

-- Define myFilter2 function using "foldr"
myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 f = foldr (\x acc -> if f x then x:acc else acc) []

-- Where
-- We want to provide a function that computes the performance of a footbal player.
-- Starting from the number of matches he played and the number of goals he made (goals / matches),
-- we decide if it's a good player or not
playerEvaluator :: (RealFloat a) => a -> a -> String
playerEvaluator 0 _ = "Your moment will come, kid..."
playerEvaluator matches goals
	| goals / matches <= 0.5 = "You should have your feet checked by a good doctor..."
	| goals / matches <= 1 = "Not bad!"
	| goals / matches > 1 = "Wow man, you are a goleador! :)"

-- We are repeating ourselves too many times... let's use a where statement!
playerEvaluator2 :: (RealFloat a) => a -> a -> String
playerEvaluator2 0 _ = "Your moment will come, kid..."
playerEvaluator2 matches goals
	| performance <= 0.5 = "You should have your feet checked by a good doctor..."
	| performance <= 1 = "Not bad!"
	| performance > 1 = "Wow man, you are a goleador! :)"
	where performance = goals / matches

-- list comprehension / let
-- Define a function that creates righth triangles
rightTriangles :: [(Integer, Integer, Integer)]
rightTriangles = [(a,b,c) | c <- [1,2..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2] -- WARNING: infinite list, use "take"!

-- Define a function that orders a list using the quicksort algorithm
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x] 
        biggerSorted = quicksort [a | a <- xs, a > x] 
    in  smallerSorted ++ [x] ++ biggerSorted

-- from "Let vs. Where" (https://wiki.haskell.org/Let_vs._Where)
-- It is important to know that let ... in ... is an expression, that is, it can be written wherever expressions are allowed. 
-- In contrast, where is bound to a surrounding syntactic construct, like the pattern matching line of a function definition.
-- e.g. ghci> 4 * (let a = 9 in a + 1) + 2 --> cannot do it with "where"

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

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- Binary tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Define a function that creates a singleton tree (one node with 2 empty leaf)
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- Define a function to insert an element into a tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

-- Define a function to check if a tree contains an element
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

-- Define a function that sum the elements of a tree composed by numbers
treeSum :: Num a => Tree a -> a
treeSum EmptyTree = 0
treeSum (Node a left right) = a + (treeSum left) + (treeSum right)

-- Define a function that returns a list containing all the values in a tree
treeValues :: Tree a -> [a]
treeValues EmptyTree = []
treeValues (Node a left right) = a : ((treeValues left) ++ (treeValues right))

-- Define the map function on tree
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f EmptyTree = EmptyTree
treeMap f (Node a left right) = Node (f a ) (treeMap f left) (treeMap f right)

-- Define the foldl function on tree
treeFoldl :: (a -> b -> a) -> a -> Tree b -> a
treeFoldl f acc EmptyTree = acc
treeFoldl f acc (Node b left right) = treeFoldl f (f (treeFoldl f acc left) b) right

-- Define the foldr function on tree
treeFoldr :: (b -> a -> a) -> a -> Tree b -> a
treeFoldr f acc EmptyTree = acc
treeFoldr f acc (Node b left right) = treeFoldr f (f b (treeFoldr f acc right)) left

-- Define a filter function on tree
treeFilter :: (a -> Bool) -> Tree a -> [a]
treeFilter f = treeFoldr (\x acc -> if f x then x:acc else acc) []