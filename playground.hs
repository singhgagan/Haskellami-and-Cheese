module Play
( 
  PlayRecord
) where
  
import qualified Data.Set as Set
import Control.Monad
import Data.Monoid

data Position = Position String String deriving (Show)

data PlayRecord = PlayRecord { track :: String,
                               artist :: String,
                               year :: Integer
                             } deriving (Show)

displayRecord x y z = do 
                  PlayRecord {track = x,
                             year = y,
                             artist = z 
                            }
                  
doubleThis :: Int -> Int
doubleThis x = x + x 

echoProduct :: Int -> Int -> Int
(echoProduct x) y = x*y

tooBig' a = if (a * a) > 1000
              then "yes"
					  else
					    "no"

affordable :: Integral a => a -> String
affordable x
 | x < hundred = "Easily Affordable"
 | x < two_hundred = "Let me talk to Kimsi First"
 | otherwise = "HA HA.. RIGHT."
 where hundred = 100
       two_hundred = 200

list2EvenList n = [a | a <- n, a `rem` 2 ==0 ]

odd2evenList n = [doubleThis a | a <- n, even a]


head' [] = error "Empty List !"
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  

swap' (x,y) = case x < 0 of True -> error "sorry no negative numbers!"
                            False -> (y, x)

--prev' :: Int -> Int
--prev' 0     = 0
--prev' (n+1) = n

halve xs = (take fh xs, drop fh xs)
           where fh = length xs `div` 2

print_prime n = [x | x<- [1..n], prime x ]
                where prime a = factors a == [1,a]
								  where factors y = [n | n <- [1..y], y `mod` n == 0]


zip' _ [] = []
zip' [] _ = []
zip' (x:xtail) (y:ytail) = (x,y): zip' xtail ytail

mySum n = myTailSum 0 n

myTailSum sum n
  | n < 0 = sum
myTailSum sum n = myTailSum (sum+n) (n-1)

keyGen x y = Set.insert (x,y) defaultSet
             where defaultSet = Set.fromList [("default", "default")]

go_haskell = do 
                 let x = keyGen "G" "S"
                 putStrLn $ "hello " ++ (show . Set.toList $ x)
 
funFunctors startList = do
                         let sep = fmap (makeJust) startList
                         show sep

makeJust arg = do
                Just arg

location (Position a b) = "The next stop is " ++ a ++ " &" ++ b  

monadTest (Just x) = Just x >>= \x -> let y = show x in Just y
                     
main = do 
        foo <- putStrLn "Hello, what's your name?"  
        name <- getLine
        let x = keyGen name name
        putStrLn ("Hey " ++ name ++ ", you rock!" ++ ".. and here's a Set " ++ (show . Set.toList $ x))

sumofTwo = do
           putStr "Enter first number: "
           str1 <- getLine
           putStr "Enter second number: "
           str2 <- getLine
           let num1 = Prelude.read str1 :: Float 
           let num2 = Prelude.read str2 :: Float
           print (num1 + num2)

