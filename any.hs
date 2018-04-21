--
-- Is crescent
--

-- isCrescent :: (Int -> Int) -> Int -> Bool
-- isCrescent f 0 = true
-- isCrescent 

sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as

foldr1 :: (t -> t -> t) -> [t] -> t
foldr1 f [a] = a
foldr1 f (a:as) = f a (foldr1 f as)