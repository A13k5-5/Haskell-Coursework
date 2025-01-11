-- 2 --
type Horse = [[Char]]

horse :: Horse
horse =
  [ "   ,//)    ",
    "   ;;' \\   ",
    ",;;' ( '\\  ",
    "     /'\\-) "
  ]

mirror :: Horse -> Horse
mirror = map reverse

transposeHelper :: Horse -> Horse
transposeHelper = foldr (zipWith (:)) (repeat [])

transpose :: Horse -> Horse
transpose = map reverse . transposeHelper

turn180 :: Horse -> Horse
turn180 = transpose . transpose

turn270 :: Horse -> Horse
turn270 = mirror . transpose . mirror

-- 3 --
nextTrib :: (Int, Int, Int) -> (Int, Int, Int)
nextTrib (a, b, c) = (b, c, a + b + c)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

tribonacci :: Int -> [Int]
tribonacci k = take k (map fst3 $ iterate nextTrib (0, 0, 1))

lazycatererNth :: Int -> Int
lazycatererNth n = (n ^ 2 + n + 2) `div` 2

lazyCaterer :: Int -> [Int]
lazyCaterer k = map lazycatererNth [0 .. k - 1]

-- 4 --
pretty :: Horse -> IO ()
pretty [] = return ()
pretty (x : xs) = do
  putStrLn x
  pretty xs

concatHorses :: Int -> Horse -> Horse
concatHorses n h = foldr1 (zipWith (++)) (replicate n h)

prettyMore :: Horse -> Int -> IO ()
prettyMore h n = pretty (concatHorses n h)

printHorses :: [Int] -> Horse -> IO ()
printHorses [] _ = return ()
printHorses (x : xs) h = do
  prettyMore h x
  printHorses xs h

horseSeq :: (Int -> [Int]) -> Int -> Horse -> IO ()
horseSeq f n = printHorses (filter (> 0) (f n))

-- 5 --
shead :: [a] -> Maybe a
shead [] = Nothing
shead xs = Just (head xs)

stail :: [a] -> Maybe [a]
stail [] = Nothing
stail xs = Just (tail xs)