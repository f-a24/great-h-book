import System.Random

-- グローバル乱数ジェネレータを使ってランダムな文字列を生成する
-- getStdGen：何らかの初期データを使ってシステムのグローバル乱数ジェネレータを初期化する
main = do
    gen <- getStdGen
    putStrLn $ take (randomRs ('a','z') gen)
    -- 同じグローバル乱数ジェネレータなので同じ文字列
    -- gen2 <- getStdGen
    -- putStr $ take (randomRs ('a','z') gen2)
    gen' <- newStdGen
    putStr $ take (randomRs ('a','z') gen')
