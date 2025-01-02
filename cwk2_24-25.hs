-- 2 --
type Horse = [[Char]]

horse :: Horse
horse =
  [ "   ,//)    ",
    "   ;;' \\   ",
    ",;;' ( ' \\ ",
    "     / '\\-)"
  ]

text :: [String]
text = ["abc", "def", "ghi"]

pretty :: Horse -> IO ()
pretty [] = return ()
pretty (x : xs) = do
  putStrLn x
  pretty xs

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
nextTrib :: (Num c) => (c, c, c) -> (c, c, c)
nextTrib (a, b, c) = (b, c, a + b + c)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

tribonacci :: (Num c) => Int -> [c]
tribonacci k = take k (map fst3 $ iterate nextTrib (0, 0, 1))

lazycatererNth :: Int -> Int
lazycatererNth n = (n ^ 2 + n + 2) `div` 2

lazyCaterer :: Int -> [Int]
lazyCaterer k = [lazycatererNth a | a <- [0 .. k - 1]]

-- 4 --
-- horseSeq :: (Int -> [Int]) -> Int -> Horse -> IO ()
-- horseSeq f n h = do