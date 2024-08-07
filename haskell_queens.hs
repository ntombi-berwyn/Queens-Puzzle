-- n :: Int
-- queens n :: Int -> b

-- [1..n] :: [Int]
-- (.) prunediags perms :: ([[Int]] -> [[Int]]) -> ([Int] -> [[Int]]) -> ([Int] -> [[Int]])
-- (.) prunediags perms [1..n] :: ([[Int]] -> [[Int]]) -> ([Int] -> [[Int]]) -> [Int] -> [[Int]]

-- b = [[Int]]

queens :: Int -> [[Int]]
queens n | sols == [[]]   = [[]]
         | otherwise      = init sols
            where sols = prunediags (perms [1..n])


-- List entries represent which row in a column is occupied, i.e. [[2, 1]] is    | x
--                                                                             x |
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [(x:ys) | x <- xs, ys <- perms (delete x xs)]
            where delete z zs | zs == []     = []
                              | head zs == z = tail zs
                              | otherwise    = head zs : delete z (tail zs)


-- All three inputs have same length
cmp :: Eq a => [a] -> [a] -> [a] -> Bool
cmp (a:as) (b:bs) (c:cs) | (a /= b) && (a /= c) = cmp as bs cs
                         | otherwise            = False
cmp _ _ _ = True


--prunediags :: Eq a => [[a]] -> [[a]]
prunediags :: [[Int]] -> [[Int]]
prunediags [] = [[]]
prunediags (x:xs) | checkdiags x  = x : (prunediags xs)
                  | otherwise     = prunediags xs
            where checkdiags [] = True
                  checkdiags (y:ys) = (cmp ys [y+1 .. (y + length ys)] [y-1, y-2 .. (y - length ys)]) && checkdiags ys
            -- Know y is not empty
            -- [y-1, y-2 ..] Enum class so [y-1 ..] doesn't work


main = do
  putStr "hello world\n"
  print(queens 4)
  print(length (queens 10))


-- [1,5,3,4,2,6]

-- x |   |   |   |   |
--   |   |   |   | x |
--   |   | x |   |   |
--   |   |   | x |   |
--   | x |   |   |   |
--   |   |   |   |   | x

