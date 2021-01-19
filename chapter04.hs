-- 再帰的に定義したmaximum関数
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- 再帰的に定義したreplicate関数
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

-- 再帰的に定義したtake関数
take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- 再帰的に定義したreverse関数
-- JavaScriptだと…　const reverse = a => a.length > 0 ? [...reverse(a.slice(1)), a.shift()] : [];
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- 再帰的に定義したrepeat関数
-- 基底部のない再帰を使って無限リストを作るので途中で切るのを忘れずに
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- 再帰的に定義したzip関数
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- 再帰的に定義したelem関数
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = Flase
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

-- クイックソート
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger
