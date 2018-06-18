import qualified Data.Sequence as Seq
import Control.Parallel.Strategies
import Control.DeepSeq
import Text.Read
import System.Environment

data Player = O | Neither | X deriving (Show, Eq, Ord)

nextPlayer X = O
nextPlayer O = X

type Board = Seq.Seq Player
emptyBoard = Seq.replicate 9 Neither

data GameState = GameState Board [GameState] Player deriving (Show, Eq)

instance Ord GameState where
  (GameState _ _ p1) < (GameState _ _ p2) = p1 < p2
  (GameState _ _ p1) > (GameState _ _ p2) = p1 > p2
  (GameState _ _ p1) <= (GameState _ _ p2) = p1 <= p2
  (GameState _ _ p1) >= (GameState _ _ p2) = p1 >= p2


main = do
  movesLoop emptyBoard X
  
movesLoop :: Board -> Player -> IO Player
movesLoop board currentPlayer = do
  putStr $ boardString board
  putStrLn "\n"
  board' <- case currentPlayer of
    X -> return (minimaxMove board)
    O -> myMove board
  case boardWinner board' of
    Nothing -> movesLoop board' $ nextPlayer currentPlayer
    Just player -> do putStr $ boardString board
                      return player

myMove :: Board -> IO Board
myMove board = do
  putStrLn "Enter position:"
  numberStr <- getLine
  return $ Seq.update ((read numberStr :: Int) - 1) O board

  
boardString :: Board -> String
boardString b =
  Seq.foldlWithIndex appender [] b
  where appender s i x | i == 2 || i == 5 || i == 8 = s ++ playerStr x i ++ "\n"
        appender s i x = s ++ playerStr x i ++ " "
        playerStr Neither i = show (i + 1)
        playerStr x _ = show x

boardWinner :: Board -> Maybe Player
boardWinner b =
  if gameOver || ownsCombination /= Neither
  then Just ownsCombination
  else Nothing
  where gameOver = Seq.null $ Seq.filter (== Neither) b
        ownsCombination = case filter (/= Neither) $ map getCombinationOwner combinations of
          [] -> Neither
          (player:_) -> player
        getCombinationOwner indexes = if allTheSame xs then head xs else Neither
          where xs = map (Seq.index b) indexes
                allTheSame xs = and $ map (== head xs) (tail xs)
        combinations = [[0, 1, 2],
                        [3, 4, 5],
                        [6, 7, 8],
                        [0, 3, 6],
                        [1, 4, 7],
                        [2, 5, 8],
                        [0, 4, 8],
                        [6, 4, 2]]


minimaxMove :: Board -> Board
minimaxMove b = b'
  where GameState b' _ _ = maximum moves
        GameState _ moves _ = minimaxStrategy X b


minimaxStrategy :: Player -> Board -> GameState
minimaxStrategy p b = GameState b moves winner
  where moves = Seq.foldlWithIndex canInsert [] b
        canInsert ns i Neither = (minimaxStrategy (nextPlayer p) (Seq.update i p b)):ns `using` rseq-- parTraversable rdeepseq
        canInsert ns i _ = ns
        winner = case boardWinner b of
          Nothing -> case p of
            X -> let GameState _ _ winner = maximum moves in winner
            O -> let GameState _ _ winner = minimum moves in winner
          Just winner -> winner

-- pseudocode
-- minimaxStrategy :: Player -> Board -> GameState
-- minimaxStrategy p b = GameState b moves winner
--     nextBoards = generateNextBoards p b
--     scores = map (minmax (nextPlayer p)) nextBoards
--     bestMove = selectBestMove scores
--     return bestMove