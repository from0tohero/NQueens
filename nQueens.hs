-- N Queens

import System.Environment

type Row = [Int]
type Col = [Int]
type Diag = [Int]
type Solution = [Col]

nQueens :: Int -> Solution
nQueens n = nQueens' 1 [] [] []
            where nQueens' :: Int -> Col -> Diag -> Diag -> Solution
                  nQueens' row colsTaken lDiagTaken rDiagTaken
                    | row == n+1 = [reverse colsTaken]
                    | otherwise = let xs = [x | x <- [1..n],
                                                not (x `elem` colsTaken),
                                                not ((x-row) `elem` lDiagTaken),
                                                not ((x+row) `elem` rDiagTaken) ]
                                      helper :: Int -> Solution
                                      helper c = nQueens' (row+1) (c:colsTaken) ((c-row):lDiagTaken) ((c+row):rDiagTaken)
                                  in concat $ map helper xs
                                  
main :: IO ()
main = do
         commands <- getArgs
         let solution = nQueens $ read $ head commands
         putStrLn $ show $ length solution
