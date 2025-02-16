isort :: (Ord a) => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

insert :: (Ord a) => a -> [a] -> [a]
insert x xs = takeWhile (<x) xs ++ [x] ++ dropWhile (<x) xs