module Main where
  import Analys
  import System.Environment
  import Debug.Trace

  parse :: String -> ([Int], [([Int], [Int])])
  parse = subParse . lines where
    subParse :: [String] -> ([Int], [([Int], [Int])])
    subParse (strMark : strTranses) = let
      mark      = read strMark
      transfers = read $ concat strTranses
      in (mark, transfers)
  subParse xs = error "Error while parse \"" ++ xs ++ "\""

  -- Input string example
  -- [3, 1, 2]
  -- [([0, 0], [1   ]), ([1   ], [1, 2]),([0   ], [0, 2]),([0, 2], [    ])]
  -- Args example:
  --  ./main --from-args "[3, 1, 2]" \
  -- "[([0, 0], [1   ]), ([1   ], [1, 2]),([0   ], [0, 2]),([0, 2], [    ])]"
  main :: IO ()
  main = do
    text <- do
        args <- getArgs
        case trace (show args) args of
          "--from-args" : content -> return $ unlines content
          _                         -> getContents
    let (mark, transfers) = parse text
    putStrLn $ growAndGetInfo transfers mark
    -- let mark = [1, 2]
    -- let transfers = [([1, 0], [1, 1]),
    --                 ([1,1], [1,1])]
    -- print $ findTree transfers mark
