module Test where

import Data.List
import Data.Ratio 

test :: Int
test = 123

data MyData a = MyData a

instance Monad MyData

-- Взято из "Learn You a Haskell for a Great Goof"
newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs 
 
flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs  
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)
