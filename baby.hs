doubleMe x = x + x

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

-- Pattern matching
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

firstLetter :: String -> String
firstLetter "" = "Emptry string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards
-- Use patterns to check structure of values passed to function
-- Use guards when we want to check whether a property of the values is true or false

bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normaly. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"

-- where bindings aren't shared across function bodies of different patterns
-- but they're shared across guards

bmiTell' :: Double -> Double -> String
bmiTell' 1 3 = "How do you even exist" -- bmi not available here
bmiTell' weight height
  | bmi <= 18.5 = "You're underweight etc"
  | bmi <= 25.0 = "You're normal and ugly"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2

-- You can also use pattern matching in where
bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
  | bmi <= skinny = "You're underweight etc"
  | bmi <= normal = "You're normal and ugly"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- You can also define functions in where blocks

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height  = weight / height ^ 2

-- Let expressions
-- bind variables anywhere. don't span across guards
-- (let (a, b, c) = (1, 2, 3) in a + b + c) * 100

-- let in List Comprehensions

calcBmis' :: [(Double, Double)] -> [Double]
-- binds w, h in the tuple, then binds bmi. then present bmi as output
-- include let much as you would a predicate, but binds instead of filters
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- case expressions are a way to use pattern matching almost anywhere

-- these are equivalent
-- see head' above

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x

-- # 4 Hello Recursion

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- pattern matching makes it easy to break down the maximum-finding problem
-- into the relevant cases and recursive subproblems

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x : replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0     = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
revsere' [] = []
reverse' (x:xs) = reverse xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = a `elem'` xs

-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger