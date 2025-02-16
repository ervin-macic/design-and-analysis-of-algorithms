msort :: Ord a => [a] -> [a]
msort [] = []
msort xs = merge (msort l) (msort r) where (l,r) = halve xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs 
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs
