-- Get parent index
parent :: Int -> Int
parent i = (i - 1) `div` 2

-- Get left child index
leftChild :: Int -> Int
leftChild i = 2 * i + 1

-- Get right child index
rightChild :: Int -> Int
rightChild i = 2 * i + 2

-- Swap elements at two indices in a list
swap :: [a] -> Int -> Int -> [a]
swap xs i j = 
    let elementi = xs !! i
        elementj = xs !! j
    in take i xs ++ [elementj] ++ 
       take (j - i - 1) (drop (i + 1) xs) ++ 
       [elementi] ++ 
       drop (j + 1) xs

-- Heapify a subtree rooted at index i
heapify :: Ord a => [a] -> Int -> Int -> [a]
heapify arr size i =
    let largest = i
        left = leftChild i
        right = rightChild i
        
        -- Find largest among root, left child and right child
        largest1 = if left < size && arr !! left > arr !! largest
                  then left
                  else largest
        
        largest2 = if right < size && arr !! right > arr !! largest1
                  then right
                  else largest1
    
    in if largest2 /= i
       then heapify (swap arr i largest2) size largest2
       else arr

-- Build max heap
buildHeap :: Ord a => [a] -> [a]
buildHeap arr =
    let size = length arr
        lastNonLeaf = (size `div` 2) - 1
    in foldl (\acc i -> heapify acc size i) 
             arr 
             [lastNonLeaf, lastNonLeaf-1..0]

-- Main heapsort function
heapSort :: Ord a => [a] -> [a]
heapSort [] = []
heapSort [x] = [x]
heapSort arr =
    let size = length arr
        -- First build max heap
        heapArr = buildHeap arr
        
        -- Extract elements from heap one by one
        sortStep :: Ord a => ([a], Int) -> Int -> ([a], Int)
        sortStep (a, n) i =
            let -- Move current root to end
                newArr = swap a 0 (n-1)
                -- Heapify reduced heap
                heapified = heapify newArr (n-1) 0
            in (heapified, n-1)
            
        (sortedArr, _) = foldl sortStep (heapArr, size) [1..size-1]
    in sortedArr