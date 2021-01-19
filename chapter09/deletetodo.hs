import System.IO
import System.Directory
import Data.List
import Control.Exception

-- ToDoリストのアイテムを削除する
-- openTempFileはSystem.IOにある関数
-- bracketOnErrorはControl.Exceptionにある関数
main = do
    contents <- readFile "chapter09/todo.txt"
    let todoTasks = lines contents -- 行ごとに分割
        -- "添字 - 文字列"の配列にする
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These ary your TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString -- 文字列から数値に
        -- 指定した要素を削除して改行文字で区切られた1つの文字列にする
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    -- 問題が起こった場合でも一時ファイルが確実に削除されるようにbracketOnErrorを使う
    bracketOnError (openTempFile "./chapter09" "temp")
        -- 問題が起こった場合は一時ファイルを削除
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)

        -- 一時ファイルに書き込む    
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "chapter09/todo.txt" -- 元ファイルを削除
            renameFile tempName "chapter09/todo.txt") -- 一時ファイルをリネーム
