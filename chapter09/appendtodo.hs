import System.IO

-- 標準入力から1行ずつ読み込んで、ToDoリストに追加
main = do
    todoItem <- getLine
    appendFile "chapter09/todo.txt" (todoItem ++ "\n")
