import Data.List

type Grid = Matrix Value

type Matrix a = [Row a]

type Row a = [a]

type Value = Char

blank :: Grid
blank = replicate 9 (replicate 9 '.')

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxs :: Matrix a -> [Row a]
boxs m = concat bs
  where
    rs = boxcols m
    bs = map rowb rs

firstb :: Matrix a -> Row a
firstb = concatMap (take 3)

secondb :: Matrix a -> Row a
secondb = concatMap (take 3 . drop 3)

thirdb :: Matrix a -> Row a
thirdb = concatMap (drop 6)

rowb :: Matrix a -> [Row a]
rowb m = [firstb m, secondb m, thirdb m]

boxcols :: Matrix a -> [[Row a]]
boxcols m = [take 3 m, take 3 (drop 3 m), drop 6 m]
