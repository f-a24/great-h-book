-- chapter06
-- 幾何学モジュール
module Geometry
( sphereVolme
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

-- 球の体積
sphereVolme :: Float -> Float
sphereVolme radius = (4.0 / 3.0) * pi * (radius ^ 3)

-- 球の表面積
sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

-- 立方体の体積
cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

-- 立方体の表面積
cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

-- 直方体の体積
cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectArea a b * c

-- 直方体の表面積
cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectArea a b * 2 + rectArea a c * 2 + rectArea c b * 2

-- 辺の長さから長方形の面積を求める補助関数
rectArea :: Float -> Float -> Float
rectArea a b = a * b
