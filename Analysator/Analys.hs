module Analys where
  import Tree

  kBording :: AttainTree -> Chip
  kBording (AttainTree (Marking mark) _ subTr) = let
          curMax   = maximum mark
          subTrMax = case subTr of
                      Tree pairs -> maximum $ map (kBording . snd) pairs
                      Degenerate -> Num 0
       in max curMax subTrMax

  analys :: AttainTree -> [String]
  analys tree = let
    kBoard = case kBording tree of
      Omega -> "сеть не ограничена"
      Num i -> "К = " ++ show i
    in [kBoard]

  growAndGetInfo :: Petri -> [Int] -> String
  growAndGetInfo transfers mark = let
          petriMark = Marking [Num i | i <- mark]
          tree = findTree transfers petriMark
          settings = analys tree
        in "Attain tree:\n" ++ show tree ++
           "\nSettings:\n"  ++ init (unlines settings)
