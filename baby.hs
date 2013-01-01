doubleMe x = x * x

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."