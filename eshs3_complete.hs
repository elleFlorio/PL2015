module EsHs3 where

import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.State

-- Monads
-- class Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b
--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y
--     fail :: String -> m a
--     fail msg = error msg

-- Maybe monad
-- instance Monad Maybe where
--     return x = Just x
--     Nothing >>= f = Nothing
--     Just x >>= f  = f x
--     fail _ = Nothing

-- do notation example
-- using bind
foo :: Maybe String  
foo = Just 3   >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
-- using do notation
foo2 :: Maybe String  
foo2 = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y) 

-- k-v map monad
-- lookup :: Ord k => k -> Map k a -> Maybe a
-- O(log n). Lookup the value at a key in the map.
-- The function will return the corresponding value as (Just value), or Nothing if the key isn't in the map.
type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = TIM
                   | Vodafone
                   | Wind
                     deriving (Eq, Ord)

-- Define a function which takes as input:
-- the name of a person
-- the map(personName, PhoneNumber)
-- the map(PhoneNumber, MobileCarrier)
-- the map(MobileCarrier, BillingAddress)
-- and returns the corresponding billing address
-- or Nothing if the search does not produce results
  
-- maps e.g.
-- let persPhone = M.fromList [("pippo","12345678"),("topolino","87654321")]
-- let phoneMobile = M.fromList [("12345678", TIM),("87654321", Vodafone)]
-- let mobileBilling = M.fromList [(TIM,"Via tal dei tali"),(Vodafone, "via tizio caio")]
findCarrierBillingAddress :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress
findCarrierBillingAddress person phoneMap carrierMap addressMap =
    case M.lookup person phoneMap of
      Nothing -> Nothing
      Just number ->
          case M.lookup number carrierMap of
            Nothing -> Nothing
            Just carrier -> M.lookup carrier addressMap

-- That's annoying, let's use the power of monads!!!
findCarrierBillingAddress2 :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress
findCarrierBillingAddress2 person phoneMap carrierMap addressMap = do
    number <- M.lookup person phoneMap
    carrier <- M.lookup number carrierMap
    M.lookup carrier addressMap

-- State Monad
-- Stack
type Stack = [Int]  

-- Define the pop function  
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  

-- Define the push function 
push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)

-- Define a function that executes the following operations on the stack:
-- pop an element
-- pop another element
-- push 100
-- pop an element
-- pop another element
stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    (a,newStack1) = pop stack  
    (b ,newStack2) = pop newStack1
    ((),newStack3) = push 100 newStack2
    (c, newStack4) = pop newStack3  
    in pop newStack4

-- newtype State s a = State { runState :: s -> (a,s) }
-- instance Monad (State s) where  
-- return x = State $ \s -> (x,s)  
-- (State h) >>= f = State $ \s -> let (a, newState) = h s  
--                                     (State g) = f a  
--                                 in  g newState

-- get :: m s
-- Return the state from the internals of the monad.

-- put :: s -> m ()
-- Replace the state inside the monad.

-- Define the pop function using State monad
popM :: State Stack Int  
popM = do
    x:xs <- get
    put xs
    return x 
 
-- Define the push function using State monad  
pushM :: Int -> State Stack ()  
pushM a = do
    xs <- get
    put (a:xs)
    return ()

-- Define stakcManipM function (previouly defined) using State Monad
stackManipM :: State Stack Int  
stackManipM = do  
    a <- popM  
    b <- popM
    pushM 100
    c <- popM  
    popM

-- Logger
type Log = [String]
newtype Logger a = Logger { runLogger :: (a, Log) }

------------------------------------------------------------------------------------
-- This is necessary since ghc 7.10 because Applicative now is a superclass of Monad
-- https://wiki.haskell.org/Functor-Applicative-Monad_Proposal

instance Functor Logger where
    fmap = liftM
 
instance Applicative Logger where
    pure  = return
    (<*>) = ap
    
-- Good (and funny) explanation of Functors, Applicative, Monads
-- http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

------------------------------------------------------------------------------------

-- Define the Logger Monad
instance Monad Logger where
    return a = Logger (a, [])
    m >>= f = let (a, w) = runLogger m
                  n      = f a
                  (b, x) = runLogger n
              in Logger (b, w ++ x)

-- Define an instance of Show for Logger
instance Show a => Show (Logger a) where
    show (Logger a) = show a

-- Define a record function to record things in the log
record :: String -> Logger ()
record s = Logger ((), [s])

-- Binary trees
-- let nums = [8,6,4,1,7,3,5]
-- let t = foldr treeInsert EmptyTree nums
-- (foldr because treeInsert takes the current tree as second parameter)
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeSum :: Num a => Tree a -> a
treeSum EmptyTree = 0
treeSum (Node a left right) = a + (treeSum left) + (treeSum right)

treeFold :: (a -> b -> a) -> a -> Tree b -> a
treeFold f acc EmptyTree = acc
treeFold f acc (Node b left right) = treeFold f (f (treeFold f acc left) b) right

-- We want to add logging to our tree, so define singletonM, treeInsertM and treeSumM
-- that logs the operations performed on the tree during execution
singletonM :: (Show a) => a -> Logger (Tree a)
singletonM x = do
    record ("Created singleton " ++ show x)
    return (Node x EmptyTree EmptyTree)

-- let nums = [8,6,4,1,7,3,5]
-- let numsTree = foldM treeInsertM EmptyTree nums
-- (foldM because treeInsertM returns a monad instead of the standard data structure)
treeInsertM :: (Ord a, Show a) => Tree a -> a -> Logger (Tree a)
treeInsertM EmptyTree x = singletonM x
treeInsertM (Node a left right) x
    | x == a = do
        record ("Inserted " ++ show x)
        return (Node x left right)
    | x < a = do
        l <- treeInsertM left x
        return (Node a l right)
    | x > a = do
        r <- treeInsertM right x
        return (Node a left r)

-- Hint: liftM function
-- liftM :: (Monad m) => (a -> b) -> m a -> m b
-- liftM f m = do
--   x <- m
--   return (f x)
treeSumM :: Num a => Logger (Tree a) -> Logger a
treeSumM t = liftM (treeFold (+) 0) t

-- Define a function that logs if the tree is balanced
-- Hint: define an andM function to do "logical and"(&)
-- on your monad
andM :: Logger Bool -> Logger Bool -> Logger Bool
andM l1 l2 = do
    c1 <- l1
    c2 <- l2
    return (c1 && c2)

treeBalancedM :: Tree a -> Logger Bool
treeBalancedM EmptyTree = do
    record "An empty tree is always balanced"
    return True
treeBalancedM (Node _ EmptyTree EmptyTree) = do
    record "A single node tree is always balanced"
    return True
treeBalancedM (Node _ EmptyTree _) = do
    record "Unbalanced!"
    return False
treeBalancedM (Node _ _ EmptyTree) = do
    record "Unbalanced!"
    return False
treeBalancedM (Node _ left right) = andM (treeBalancedM left) (treeBalancedM right)









