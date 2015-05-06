module Eshs1 where

-- Length 
-- function definition + pattern matching
myLength :: [Int] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- parametric polimorphism
myLength2 :: [a] -> Int
myLength2 [] = 0
myLength2 (_:xs) = 1 + myLength2 xs

-- Range
myRange :: Int -> Int -> [Int]
myRange a b = if a > b
                then error "Low > High"
                else if a < b
                        then a : myRange (a + 1) b
                        else [a]

-- Constructors (type vs data)

-- Data type (sum)
data TrafficLight = Red | Yellow | Green

data Answer = Yes | No

-- Data type (product)
data Shape = Circle Float Float Float | Rectangle Float Float Float Float

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs (x2 - x1)) * (abs (y2 - y1))

-- type constructor / data constructor
data StringTree = EmptyStringTree | StringNode String StringTree StringTree

data BoolTree = EmptyBoolTree | BoolNode Bool BoolTree BoolTree

-- We can use a type constructor with type variables
data Tree a = EmptyTree | Node a (Tree a) (Tree a)

-- It is important here to consider the difference between a concrete type 
-- (examples include Int, [Char] and Maybe Bool) which is a type that can be 
-- assigned to a value in your program, and a type constructor function which 
-- you need to feed a type to be able to be assigned to a value. 
-- A value can never be of type "list", because it needs to be a "list of something". 
-- In the same spirit, a value can never be of type "binary tree", because it needs 
-- to be a "binary tree storing something".

-- Record syntax
-- person: first name, last name, age, height, phone number 
data BadPerson = BadPerson String String Int Float String

badFirstName :: BadPerson -> String
badFirstName (BadPerson badFirstName _ _ _ _) = badFirstName

badLastName :: BadPerson -> String  
badLastName (BadPerson _ badLastName _ _ _) = badLastName  
  
badAge :: BadPerson -> Int  
badAge (BadPerson _ _ badAge _ _) = badAge  
  
badHeight :: BadPerson -> Float  
badHeight (BadPerson _ _ _ badHeight _) = badHeight  
  
badPhoneNumber :: BadPerson -> String  
badPhoneNumber (BadPerson _ _ _ _ badPhoneNumber) = badPhoneNumber

-- Using record syntax we can do the same things in a quicker
-- and more readable way
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String 
                     }

-- type synonyms
-- We need to organize our collection of MP3 songs. We want to store it as (title, duration) pairs
type Title = String
type Duration = Float
type SongsCollection = [(Title, Duration)]

inSongsCollection :: Title -> Duration -> SongsCollection -> Bool
inSongsCollection title duration collection = (title, duration) `elem` collection

-- newtype vs data
-- newtype "wrap" an existing type
-- newtype: one data constructor with one field
-- data: several data constructor with 0 or more fields
-- newtype -> strict; data -> lazy
-- newtype faster than data
data Profession = Fighter | Archer | Accountant
data Race = Human | Elf | Orc | Goblin
data PlayerCharacter = PlayerCharacter Race Profession

newtype CharList = CharList [Char]
newtype CorrectAnswer = CorrectAnswer Answer
newtype WrongAnswer = WrongAnswer Answer

-- functions currying and composition
-- Map
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

addOne :: [Int] -> [Int]
addOne xs = myMap (+1) xs

-- function composition (GHCI)
-- f (g (z x)) == (f . g . z) x
-- negate . sqrt $ 4
-- myLength . myRange 3 $ 5

-- infinite computations
-- WARNING: infinite list, use "take"!
myRange2 :: Int -> [Int]
myRange2 a = a : myRange2 (a + 1)