doubleMe x = x + x

-- old
--doubleUs x y = x * 2 + y * 2
-- new
doubleUs x y = doubleMe x + doubleMe y

-- if式はelse節が必須
doubleSmallNumber x = if x > 100
    then x
    else x*2

-- 'は関数名に使える
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

-- 関数名の先頭は大文字で始められない
-- 関数が引数をひとつも受け取らない場合、定義とか名前と呼ぶ
conanO'Brien = "It's a-me, Conan O'Brien!"

-- 10以上のすべての奇数をBANG!、10より小さいすべての帰趨をBOOM!に置き換える
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- リスト内包表記を使った独自length関数
length' xs = sum [1 | _ <- xs]

-- 文字列から大文字だけを残す関数
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

-- 各要素が10以下のトリプル
triples = [(a, b, c) | c <- [1..10], a <- [1..10], b <- [1..10]]

-- 直角三角形のトリプル
rightTriangles = [(a, b, c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2]

-- 周囲の長さが24の直角三角形
rightTriangles' = [(a, b, c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24]
