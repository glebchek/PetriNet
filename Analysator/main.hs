
module Main where
  import Tree
  -- import Debug.Trace

  main :: IO ()
  main = do
    let mark = Marking [Num i | i <- [2, 3]]
    let transfers = [([1, 0], [1, 1]),
                    ([1,1], [1,1])]
    print $ findTree transfers mark
