import Data.Char

type Board = [[Square]]

type Move = (Int, Int)

type Pos = (Int, Int)

data PieceType = Rook | Knight | Pawn | King | Queen | Bishop deriving (Eq)

data Square = Filled Piece | Empty deriving (Eq)

data Piece = Piece Team PieceType deriving (Eq)

data Team = Black | White deriving (Eq)

data GameTree = Leaf Int | Node [GameTree] deriving (Show)

--Get AI's choice of move
pickMove :: Board -> Team -> Board
pickMove board team = let listOfPossibleBoards = possibleBoards board team in fst $ getOptimalMove listOfPossibleBoards team

--For all moves the AI could make, run minimax on them all and return the best one
getOptimalMove :: [Board] -> Team -> (Board, Int)
getOptimalMove boards team = foldl accumFunction (head boards, minBound) boards
  where
    accumFunction = \acc n ->
      let currentValue = minimizer $ generateGameTree (otherTeam team) team n
       in if (currentValue >= snd acc && not (isChecked (fst acc) team)) then (n, currentValue) else acc

--Return true if a team is checked
isChecked :: Board -> Team -> Bool
isChecked board team = checkListForKing board (teamMoveDestinations board (otherTeam team))

--Return true if list contains a king
checkListForKing :: Board -> [Pos] -> Bool
checkListForKing _ [] = False
checkListForKing board (pos : xs) = checkForKing || checkListForKing board xs
  where
    checkForKing
      | squareAt board pos == Empty = False
      | extractType board pos == King = True
      | otherwise = False

--Return list of positions that follow the rules of chess for an entire team's move
teamMoveDestinations :: Board -> Team -> [Pos]
teamMoveDestinations board team = foldl (\acc pos -> acc ++ validDestinations board team pos) [] allMovesForTeam
  where
    allMovesForTeam = positionsOfTeam board team

--For all moves the AI could make, return most optimal
maximizer :: GameTree -> Int
maximizer (Leaf value) = value
maximizer (Node xs) = maximum $ map minimizer xs

--For all moves the opposing team could make, return least optimal for the AI
minimizer :: GameTree -> Int
minimizer (Leaf value) = value
minimizer (Node xs) = minimum $ map maximizer xs

--Generates a game tree
generateGameTree :: Team -> Team -> Board -> GameTree
generateGameTree = depthLimitedTree 1

--Helper function that generates game tree of a limited depth
depthLimitedTree :: Int -> Team -> Team -> Board -> GameTree
depthLimitedTree depth team originalTeam board
  | depth == 3 = Leaf $ evalBoard board originalTeam
  | otherwise = Node $ map recurse unCheckedBoards
  where
    recurse = depthLimitedTree (depth + 1) (otherTeam team) originalTeam
    unCheckedBoards = possibleBoards board team

--Generate all possible boards that can result from a teams turn
possibleBoards :: Board -> Team -> [Board]
possibleBoards board team = foldl (\acc pos -> acc ++ (possibleMovesFromPosition board team pos)) [] allMovesForTeam
  where
    allMovesForTeam = positionsOfTeam board team

--Generate all possible boards that can come from moving a piece at some position
possibleMovesFromPosition :: Board -> Team -> Pos -> [Board]
possibleMovesFromPosition board team pos = map (movePos board pos) (validDestinations board team pos)

--Return list of destinations that follow the rules of chess for a given positions move
validDestinations :: Board -> Team -> Pos -> [Pos]
validDestinations board team pos = filter (isValidMove board team pos) listOfDestinations
  where
    listOfDestinations = map (applyMoveToPosition pos) (movesForPieceType board team pos (extractType board pos))

isValidMove :: Board -> Team -> Pos -> Pos -> Bool
isValidMove board team start destination = isOnBoard destination && not (takesOwnTeam board start destination) && isClearPath board path
  where
    path = drawPath board start destination

-- Return true if the result of a move is a piece checking its own team
takesOwnTeam :: Board -> Pos -> Pos -> Bool
takesOwnTeam board start destination
  | squareAt board destination == Empty = False
  | extractTeam board start == extractTeam board destination = True
  | otherwise = False

-- Returns true if no pieces lie in the path
isClearPath :: Board -> [Pos] -> Bool
isClearPath _ [] = True
isClearPath board (x : xs)
  | squareAt board x /= Empty = False
  | otherwise = isClearPath board xs

--Checks if piece path is being drawn from is a knight before drawing path
drawPath :: Board -> Pos -> Pos -> [Pos]
drawPath board start destination
  | extractType board start == Knight = []
  | otherwise = tail $ drawPath_helper start destination

-- Given 2 positions, return all the positions that lie in between
drawPath_helper :: Pos -> Pos -> [Pos]
drawPath_helper (a, b) (x, y)
  | a == x && b == y = []
  | otherwise = (a, b) : (drawPath_helper (nextRow, nextCol) (x, y))
  where
    nextRow = a + rowDirection (a, b) (x, y)
    nextCol = b + colDirection (a, b) (x, y)

-- Return the directions that a movement takes, represented as a vector
movementType :: Pos -> Pos -> Move
movementType pos1 pos2 = (rowDirection pos1 pos2, colDirection pos1 pos2)

--Row component of movementType
rowDirection :: Pos -> Pos -> Int
rowDirection (a, _) (x, _)
  | x - a > 0 = 1
  | x - a < 0 = -1
  | x - a == 0 = 0

--Column component of movementType
colDirection :: Pos -> Pos -> Int
colDirection (_, b) (_, y)
  | y - b > 0 = 1
  | y - b < 0 = -1
  | y - b == 0 = 0

--Return true if position lies within dimensions of the board
isOnBoard :: Pos -> Bool
isOnBoard (row, col) = row >= 0 && row < 8 && col >= 0 && col < 8

--Given a position and a move, return the position that results from applying the move
applyMoveToPosition :: Pos -> Move -> Pos
applyMoveToPosition (x, y) (a, b) = (x + a, y + b)

--Given a piece type, return all the moves that it could make represented as vectors
movesForPieceType :: Board -> Team -> Pos -> PieceType -> [Move]
movesForPieceType board team pos Pawn = generatePawnMoves board team pos
movesForPieceType _ _ _ King = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
movesForPieceType _ _ _ Knight = [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
movesForPieceType _ _ _ Queen = forAllRangesInBoard [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)] 1
movesForPieceType _ _ _ Rook = forAllRangesInBoard [(-1, 0), (0, -1), (0, 1), (1, 0)] 1
movesForPieceType _ _ _ Bishop = forAllRangesInBoard [(-1, -1), (-1, 1), (1, -1), (1, 1)] 1

--Return list of moves a pawn can make
generatePawnMoves :: Board -> Team -> Pos -> [Move]
generatePawnMoves board team pos = getFront board team pos 1 ++ getFront board team pos 2 ++ getDiagonal board team pos (-1) ++ getDiagonal board team pos 1

--Returns a move if there's no pieces in front to block it
getFront :: Board -> Team -> Pos -> Int -> [Move]
getFront board team (row, col) jumpSize
  | not $ isOnBoard posInFront = []
  | squareAt board posInFront == Empty = [(getPawnDirection team * jumpSize, 0)]
  | otherwise = []
  where
    posInFront = (row + (getPawnDirection team) * jumpSize, col)

--Return a diagonal move if there's an opposing teams piece to take
getDiagonal :: Board -> Team -> Pos -> Int -> [Move]
getDiagonal board team (row, col) modif
  | not $ isOnBoard diagonalPos = []
  | squareAt board diagonalPos == Empty = []
  | extractTeam board diagonalPos /= team = [(getPawnDirection team, modif)]
  | otherwise = []
  where
    diagonalPos = (row + (getPawnDirection team), col + modif)

--Return the direction a team's pawn will move in
getPawnDirection :: Team -> Int
getPawnDirection White = (-1)
getPawnDirection Black = 1

--Helper function for moves, scales a list of tuples to everything in range of a single move on a standard chess board
forAllRangesInBoard :: [Move] -> Int -> [Move]
forAllRangesInBoard list c
  | c == 8 = []
  | otherwise = movesForCurrentRange ++ restOfMoves
  where
    movesForCurrentRange = map (tupleProduct c) list
    tupleProduct = (\mul tuple -> (fst tuple * mul, snd tuple * mul))
    restOfMoves = forAllRangesInBoard list (c + 1)

--Given a board and a team, return all positions in the board that have a piece of that team
positionsOfTeam :: Board -> Team -> [Pos]
positionsOfTeam board team = positionsOfTeamIter board team 0 0

--Helper function for positionsOfTeam
positionsOfTeamIter :: Board -> Team -> Int -> Int -> [Pos]
positionsOfTeamIter board team row col
  | squareAt board (row, col) == Empty = recursiveCase
  | extractTeam board (row, col) == team = (row, col) : recursiveCase
  | otherwise = recursiveCase
  where
    recursiveCase
      | row == 7 && col == 7 = []
      | col == 7 = positionsOfTeamIter board team (row + 1) 0
      | otherwise = positionsOfTeamIter board team row (col + 1)

--Given 2 positions, move whatever is at the first position to the second position
movePos :: Board -> Pos -> Pos -> Board
movePos board start destination = replaceEndSquare
  where
    replaceEndSquare = replaceSquare removeStartPiece destination (squareAt board start)
    removeStartPiece = replaceSquare board start Empty

--Given a position and a square, replace whatever is at that position with the square that came as an argument
replaceSquare :: Board -> Pos -> Square -> Board
replaceSquare xs (row, col) square = replaceElem row replacedRow xs
  where
    pickRow = xs !! row
    replacedRow = replaceElem col square pickRow

--Given a position, return the square that resides there. Only intended to be called if there's actually a piece
squareAt :: Board -> Pos -> Square
squareAt xs (row, col) = (xs !! row) !! col

-- Gets the piece that resides at a square. Throws an error if you try to get the piece of an empty square
pieceAt :: Square -> Piece
pieceAt Empty = error "Only intended to be called if there's actually a piece there"
pieceAt (Filled piece) = piece

getType :: Piece -> PieceType
getType (Piece _ pieceType) = pieceType

getTeam :: Piece -> Team
getTeam (Piece team _) = team

-- Given a position, return the team of the piece residing there. Throws an error if called for an empty square
extractTeam :: Board -> Pos -> Team
extractTeam board = getTeam . pieceAt . (squareAt board)

-- Given a position, return the type of the piece residing there. Throws an error if called for an empty square
extractType :: Board -> Pos -> PieceType
extractType board = getType . pieceAt . (squareAt board)

otherTeam :: Team -> Team
otherTeam Black = White
otherTeam White = Black

--Heuristic value for all pieces in the board
evalBoard :: Board -> Team -> Int
evalBoard [] _ = 0
evalBoard (x : xs) team = evalRow x team + evalBoard xs team

--Helper function for evalRow, heuristic value of all pieces in a row
evalRow :: [Square] -> Team -> Int
evalRow [] _ = 0
evalRow (x : xs) team = valueSquare x team + evalRow xs team

--Helper function for evalRow, heuristic value of a square with team taken into account
valueSquare :: Square -> Team -> Int
valueSquare Empty _ = 0
valueSquare (Filled piece) team
  | getTeam piece == team = valuePieceType $ getType piece
  | otherwise = (-1) * (valuePieceType $ getType piece)

--Helper function for valueSquare, heuristic value of a piece without team taken into account
valuePieceType :: PieceType -> Int
valuePieceType Pawn = 10
valuePieceType Knight = 30
valuePieceType Bishop = 40
valuePieceType Rook = 60
valuePieceType Queen = 100
valuePieceType King = 1000

-- The starting board for a game of chess
initBoard :: Board
initBoard = ([firstRow] ++ [secondRow] ++ middle ++ [secondLastRow] ++ [lastRow])
  where
    firstRow = map Filled $ map (Piece Black) [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]
    secondRow = replicate 8 (Filled $ Piece Black Pawn)
    middle = replicate 4 (replicate 8 Empty)
    secondLastRow = replicate 8 (Filled $ Piece White Pawn)
    lastRow = map Filled $ map (Piece White) [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]

--Returns string representation of a board. Note: Use putStrLn to render the line breaks properly
boardString :: Board -> String
boardString [] = ""
boardString (x : xs) = rowString x ++ (boardString xs)

--Helper function for boardString, returns string representation of a row
rowString :: [Square] -> String
rowString [] = "\n"
rowString (x : xs) = show x ++ rowString xs

instance Show Square where
  show Empty = "-"
  show (Filled piece) = show piece

instance Show Piece where
  show (Piece Black pieceType) = toLowerString $ show pieceType
  show (Piece White pieceType) = show pieceType

instance Show PieceType where
  show Rook = "R"
  show King = "K"
  show Pawn = "P"
  show Queen = "Q"
  show Bishop = "B"
  show Knight = "N"

--Generic Helper functions -----------------------------------------------------------------------------------------------------------------
toLowerString :: String -> String
toLowerString = map toLower

--Given a list, an index and an element, replace the index of that list with the element. Indexing starts at 0
replaceElem :: Int -> a -> [a] -> [a]
replaceElem _ _ [] = []
replaceElem counter elem (x : xs)
  | counter == 0 = elem : recursiveCase
  | otherwise = x : recursiveCase
  where
    recursiveCase = replaceElem (counter - 1) elem xs