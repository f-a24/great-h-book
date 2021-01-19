-- 入力を受け取り、10文字より短い行だけを出力する

-- main = do
--     contents <- getContents
--     putStr (shortLinesOnly contents)

-- interactはString -> String型を受け取り、入力にその関数を適用して、返ってきた結果を出力するI/Oアクションを返す
main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines
