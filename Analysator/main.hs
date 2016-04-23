
module Main where
  import Tree
  -- import Debug.Trace

  -- type Marking = [Chip]
  -- type Node     = Int
  -- type Transfer = ([Node], [Node]) -- Входные узыл и выходные узлы
  -- type Petri    = [Transfer]

  main :: IO ()
  main = do
    let mark = [Num i | i <- [2, 3]]
    let transfer = [([1, 0], [1, 1]),
                    ([1,1], [1,1])]
    print $ findTree transfer mark
