module Tree where
  import Control.Arrow --second
  import Data.Tuple()
  import Control.Monad
  import Data.Maybe
  import Control.Monad.State.Lazy

  type TransferNum = Int
  -- Look like maybe Monad
  data Chip     = Num Int |
                  Omega     -- Либо число либо Омега
  instance Show Chip where
    show (Num i) = show i
    show Omega   = "w"--ω"

  instance Eq Chip where
    (==) Omega   Omega   = True
    (==) (Num c) (Num o) = c == o
    (==) _       _       = False

  instance Ord Chip where
    (<=) _       Omega   = True
    (<=) Omega   _       = False
    (<=) (Num c) (Num o) = c <= o

  data Marking  = Marking {
                marks :: [Chip]
              } deriving Eq
  instance Show Marking where
    show m = "|" ++ init (concatMap (\c -> show c ++ " ") $ marks m) ++ "|"

  type Node     = Int
  type Transfer = ([Node], [Node]) -- Входные узыл и выходные узлы
  type Petri    = [Transfer]

  data MarkType = JustMark     |
                  AlreadyExist |
                  CoverMark

  instance Show MarkType where
    show JustMark     = ""
    show AlreadyExist = "Existed"
    show CoverMark    = "Cover Mark"

  isCovering :: MarkType -> Bool
  isCovering CoverMark = True
  isCovering _         = False

  data Tree = Tree [(TransferNum, AttainTree)] |
              Degenerate
  instance Show Tree where
    show Degenerate = ""
    show (Tree pairs) = let
      showOne (trans, tre) = " t" ++ show trans ++ " -> " ++ show tre
      in concatMap showOne pairs
  data AttainTree = AttainTree{
                      currentMark :: Marking
                    , markType    :: MarkType
                    , tree        :: Tree
                  }

  instance Show AttainTree where
    show its = let
            typ = markType its
            mark = currentMark its
            tre = tree its
            writeCurr = "(" ++ show mark ++ case typ of
                  JustMark -> " "
                  _        -> " is " ++ show typ
            writeSubTrees = case tre of
                              Tree _ -> "[" ++ show tre ++ "]"
                              _      -> ""
            in writeCurr ++ writeSubTrees ++ ")"

  replace :: [a] -> a -> Int -> [a]
  replace xs e n = take n xs ++ [e] ++ drop (n + 1) xs

  nextMark :: Monad m => Marking -> Transfer -> m (MarkType, Marking)
  nextMark mark (input, output) = liftM (newMark mark output) $
                                  foldM ifEnough mark input where
      ifEnough :: Monad m => Marking -> Node -> m Marking
      ifEnough (Marking cmark) node = case cmark!!node of
                    Omega   -> return $ Marking cmark
                    (Num i) -> if i <= 0 then
                                fail "Not enough"
                               else let
                                 new = replace cmark (Num (i - 1)) node
                               in return $ Marking new

      newMark :: Marking -> [Node] -> Marking -> (MarkType, Marking)
      newMark (Marking oldMark) n (Marking currMark) = let
          addChip cmark node = case cmark!!node of -- Добавить фишки
              Omega   -> cmark
              (Num i) -> replace cmark (Num (i + 1)) node
          nMark = foldl addChip currMark n
          isCover (prev, cur) = prev <= cur -- Покрывает ли в текущей ноде
          cover oldM cmark node = let -- Заомежить всё, куда добавлялось
                  old = oldM!!node
                  cur = cmark!!node
                  -- tr = "cur > old: " ++ show cur ++ " > " ++ show old
                in if cur > old then
                     -- trace tr $
                     replace cmark Omega node
                   else cmark
        in if all isCover (zip oldMark nMark) then let -- Если покрывает,
              omeged = foldl (cover oldMark) nMark n   -- добавим ω
              in if omeged > nMark then
                   (CoverMark, Marking omeged)
                 else (JustMark, Marking omeged)
           else (JustMark, Marking nMark)

  type FoundedNodes = State [Marking]

  fillSubTree :: Petri -> Marking -> FoundedNodes AttainTree
  fillSubTree p mark = let
      nextMarks = map (nextMark mark) p
      pairs = zip [1..length p] nextMarks -- Пара переход-маркировка
      permittedM = filter (isJust . snd) pairs -- Разрешённые
      permitted  = map (second fromJust) permittedM
            -- уже были / ещё не были
      newAndOld st = span (flip notElem st . snd . snd) permitted
      -- Фунция с общим состоянием для каждого поддерева
      findSubTreeList lst = runState (mapM (fillSubTree p) lst)
      in do
        foundMarks <- get
        -- Возьмём состояния, которых ещё не было и которые были
        let (newMarks, finishedMarks) = newAndOld foundMarks
        -- Превращаем их в уже существующие
        let existedMarks = map (second (\(_, m) -> (AlreadyExist, m))) finishedMarks
        -- Создадим конечные поддеревья для existedMarks
        let creaeteFinish (typ, m) = AttainTree m typ Degenerate
        let finishedPairs = map (second creaeteFinish) existedMarks
        -- сохраним текущее состояние
        let savedMarks = map (snd . snd) newMarks
        let currExistMarks = savedMarks ++ foundMarks
        let (newTrees, newSt) = findSubTreeList savedMarks currExistMarks
        put newSt
        let transfers = map fst newMarks
        let truthPairs = zip transfers newTrees
        return (AttainTree mark JustMark (Tree (finishedPairs ++ truthPairs)))

  findTree :: Petri -> Marking -> AttainTree
  findTree p m = evalState (fillSubTree p m) [m]
