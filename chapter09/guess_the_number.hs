import System.Random
import Control.Monad(when)

-- ユーザにプログラムが考えた数を当てさせる
main = do
    gen <- getStdGen
    askForNumber gen

-- 数値を端末から入力してその数値が正しいかどうかを表示するI/Oアクションを返す
askForNumber :: StdGen -> IO ()
askForNumber gen = do
    -- randomR：上限と下限のペアを受け取り、その範囲内の値を返す
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStrLn "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You art correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        askForNumber newGen

{- すべてをmainで呼び出してもできる
main = do
    gen <- getStdGen
    let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)
    putStrLn "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You art correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        newStdGen
        main
-}


