--quickSort :: [Int] -> Int -> [Int]
--quickSort list pivot = [num | num <- num list, num <= pivot  ] ++ [pivot] ++ [num | num <- num list, num > pivot]

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y < x] ++ [x] ++ quickSort [y | y <- xs, y >= x]