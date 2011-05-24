import qualified Data.Foldable as F 
import qualified Data.Map as M

data Meat = Ham | Pepperoni | Sausage | Chicken | Bacon
            deriving (Eq, Enum, Read)
data Veggies = Mushrooms | Onions | Artichokes | BellPepper
               deriving (Eq, Enum, Show)
                    
data Pizza = Pizza { veggies :: [Veggies],
                     meat :: [Meat]
                   } deriving (Show)

instance Show Meat where
  show Ham = "Ham: $5.00\n"
  show Pepperoni = "Pepperoni: $5.00\n"
  show Sausage = "Sausage: $5.00\n"
  show Chicken = "Chicken: $5.00\n"
  show Bacon = "Bacon: $5.00\n"

main = do
        let m = [Ham .. Bacon]
        let v = [Mushrooms .. BellPepper]
        putStrLn $ "Make your Own Pizza:\n" ++ "== All Meat Toppings -- 5 BUCKS==: "
        putStrLn . foldr (++) [] . fmap (show) $ m
        putStrLn $ "==Veggie Toppings==: "
        putStrLn . foldr (++) [] . fmap (\x -> show x ++ ":  " ++ (show . vegPriceCheck . show $ x) ++ "\n") $ v
        let toppinglist = getInput
        putStrLn "Please Select Meat Toppings. (Case-Sensitive)"
        b <- toppinglist
        let meatsum = checkMeat b m
        putStrLn "Please Select Any Veggies. (**Extra Tax: 50 Cents)"
        c <- toppinglist
        let vegsum = checkVeg c v
        printBill meatsum vegsum

printBill m v
 | m <= 0 && v <= 0 = putStrLn "Sorry We don't have those toppings. Please try another Pizzeria."
 | otherwise = do
                 let sum = v + fromIntegral m
                  in putStrLn $ "Pizza is ready. Total Bill: " ++ (show sum) ++ "dollars"          

getInput = do
            a <- fmap words getLine
            return a

checkMeat i m = do
                let list = fmap (toMaybe) i
                let priceM = map (meatPriceCheck) list
                abs $ F.foldr (+) 0 priceM
         
checkVeg i m = do
                let priceV = map (vegPriceCheck) i
                let taxes = map (taxCalc) priceV
                abs $ sum taxes
                
toMaybe :: String -> Maybe Meat
toMaybe "Ham" = Just Ham
toMaybe "Bacon" = Just Bacon
toMaybe "Pepperoni" = Just Pepperoni
toMaybe "Chicken" = Just Chicken
toMaybe "Sausage" = Just Sausage
toMaybe _ = Nothing

meatPriceCheck :: Maybe a -> Int
meatPriceCheck (Just x) = 5
meatPriceCheck Nothing = 0

vegPriceCheck x = M.lookup x . M.fromList $ [("Mushrooms", "0.80"),
                                             ("Onions", "0.30"),
                                             ("Artichokes", "0.90"),
                                             ("BellPepper", "0.10")]

taxCalc (Just x) = (read x :: Float) + 0.50
taxCalc Nothing = 0

