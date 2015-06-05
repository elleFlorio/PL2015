module DoubleStack where

import Control.Applicative
import Control.Monad

type Stack = [Int]
type DoubleStack = (Stack, Stack)

-- newtype State s a = State { runState :: s -> (a,s) }
newtype StateMaybe s a = StateMaybe {runStateM :: s -> Maybe (a, s)}

instance Functor (StateMaybe s) where
    fmap = liftM
 
instance Applicative (StateMaybe s) where
    pure  = return
    (<*>) = ap

-- instance Monad (State s) where  
--     return x = State $ \s -> (x,s)  
--     (State h) >>= f = State $ \s -> let (a, newState) = h s  
--                                         (State g) = f a  
--                                     in  g newState
instance Monad (StateMaybe s) where
    return x = StateMaybe $ \s -> Just (x,s)
    (StateMaybe h) >>= f = StateMaybe $ \s -> case h s of 
                                 Just (a, newState) -> let (StateMaybe g) = f a 
                                                    in g newState
                                 Nothing -> Nothing

push :: Int -> StateMaybe DoubleStack ()
push v = do StateMaybe $ \(xs,ys) -> Just ((),(v:xs,ys))

move :: StateMaybe DoubleStack ()
move = do StateMaybe $ \(xs,ys) -> if null xs
                                then Nothing
                                else Just ((),(tail xs, (head xs):ys))

pop :: StateMaybe DoubleStack Int
pop = do StateMaybe $ \(xs,ys) -> if null ys 
                                then Nothing
                                else Just (head ys,(xs, tail ys))

-- ([1,2,3],[4,5,6]) -> Just (7,([2,3],[4,5,6]))
-- ([ ],[4,5,6]) -> Nothing
check :: StateMaybe DoubleStack Int
check = do
    push 7
    move
    move
    pop
    pop

