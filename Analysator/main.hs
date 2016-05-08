
module Main where
  import Analys
  -- import Debug.Trace

  main :: IO ()
  main = do
    -- let mark = Marking [Num i | i <- [3, 1, 2]]
    let mark = [3, 1, 2]
    let transfers = [([0, 0], [1   ]),
                     ([1   ], [1, 2]),
                     ([0   ], [0, 2]),
                     ([0, 2], [    ])]
    putStrLn $ growAndGetInfo transfers mark
    -- let transfers = [([1, 0], [1, 1]),
    --                 ([1,1], [1,1])]
    -- print $ findTree transfers mark
