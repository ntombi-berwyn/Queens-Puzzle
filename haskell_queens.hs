{- Type Derivation Example
      n :: Int
      queens n :: Int -> b

      [1..n] :: [Int]
      (.) prunediags perms :: ([[Int]] -> [[Int]]) -> ([Int] -> [[Int]]) -> ([Int] -> [[Int]])
      (.) prunediags perms [1..n] :: ([[Int]] -> [[Int]]) -> ([Int] -> [[Int]]) -> [Int] -> [[Int]]

      b = [[Int]]     -}

-- Returns list of all queen problem solutions
queens :: Int -> [[Int]]
queens n | sols == [[]]   = [[]]
         | otherwise      = init sols
            where sols = prunediags (perms [1..n])


-- Calculates all permuations of a list
-- List entries represent which row in a column is occupied, i.e. [[2, 1]] is    | x
--                                                                             x |
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [(x:ys) | x <- xs, ys <- perms (delete x xs)]
            where delete z zs | zs == []     = []
                              | head zs == z = tail zs
                              | otherwise    = head zs : delete z (tail zs)


-- Checks whether an initial list shares any identical entries with two other lists
-- Used to check chess board diagonals (already know rows and cols don't clash)
-- All three inputs have same length
cmp :: Eq a => [a] -> [a] -> [a] -> Bool
cmp (a:as) (b:bs) (c:cs) | (a /= b) && (a /= c) = cmp as bs cs
                         | otherwise            = False
cmp _ _ _ = True


-- Remove permutations where there is a diagonal clash of queens
--prunediags :: Eq a => [[a]] -> [[a]]
prunediags :: [[Int]] -> [[Int]]
prunediags [] = [[]]
prunediags (x:xs) | checkdiags x  = x : (prunediags xs)
                  | otherwise     = prunediags xs
            where checkdiags [] = True
                  checkdiags (y:ys) = (cmp ys [y+1 .. (y + length ys)] [y-1, y-2 .. (y - length ys)]) && checkdiags ys
            -- [y-1, y-2 ..] Enum class so [y-1 ..] doesn't work


-- Turns a solution to a string representing the board
-- Here, list entries represent position of queen in a row (overall, solution set symmetrical)
grid :: Int -> [Int] -> String
grid _ [] = ""   -- find better (recursion and no solutions end here)
grid 0 _ = ""
grid n (x:xs) = concat(replicate (x-1) "   |") ++ " x " ++ concat(replicate (n-x) "|   ") ++ "\n" ++ grid n xs


-- Outputs all given solutions as boards
display :: [[Int]] -> IO ()
display xss = putStr(unlines (map (grid len) xss))
            where len = length (head xss)


main = do
  putStr "======The Queens Puzzle in Haskell======\n"
  print(queens 4)
  display (queens 4)
  print(length (queens 8))


{- Example permutation that doesn't work
    [1,5,3,4,2,6]

     x |   |   |   |   |
       |   |   |   | x |
       |   | x |   |   |
       |   |   | x |   |
       | x |   |   |   |
       |   |   |   |   | x        -}
