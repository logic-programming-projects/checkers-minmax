{-
Checkers.hs — Логіка гри в шашки (English Draughts) на Haskell

Порівняльна реалізація того самого алгоритму MinMax + Alpha-Beta,
що і Prolog-версія.
-}

module Checkers where

import Data.Array (Array, listArray, (!), (//))
import Data.List (sort, group, intercalate)

-- ============================================================
-- Типи даних
-- ============================================================

-- | Значення клітинки (аналог Prolog атомів: none, empty, black, ...)
data Cell = None | Empty | Black | White | BlackKing | WhiteKing
  deriving (Eq, Ord, Show)

-- | Гравець
data Player = PlayerBlack | PlayerWhite
  deriving (Eq, Ord, Show)

-- | Хід: початок (R,C), кінець (R,C), список взятих позицій
data Move = Move (Int, Int) (Int, Int) [(Int, Int)]
  deriving (Eq, Ord, Show)

-- | Дошка: масив з 64 клітинок (1-based індекс, як у Prolog)
type Board = Array Int Cell

-- ============================================================
-- Константи
-- ============================================================

-- | Нескінченність для альфа-бета (аналог Prolog inf(9999))
inf :: Int
inf = 9999

-- ============================================================
-- Операції з дошкою (1-based індекси, як у Prolog)
-- ============================================================

-- | Індекс клітинки (R,C) у масиві (аналог Prolog cell_idx/3)
cellIdx :: Int -> Int -> Int
cellIdx r c = (r - 1) * 8 + c

-- | Перевірка ігрової (темної) позиції (аналог Prolog valid_pos/2)
validPos :: Int -> Int -> Bool
validPos r c = r >= 1 && r <= 8 && c >= 1 && c <= 8 && (r + c) `mod` 2 == 1

-- | Читає значення клітинки (аналог Prolog get_cell/4)
getCell :: Board -> Int -> Int -> Cell
getCell board r c = board ! cellIdx r c

-- | Встановлює значення клітинки (аналог Prolog set_cell/5)
setCell :: Board -> Int -> Int -> Cell -> Board
setCell board r c v = board // [(cellIdx r c, v)]

-- | Початкова розстановка шашок (аналог Prolog initial_board/1)
-- Чорні шашки: рядки 1-3, білі шашки: рядки 6-8
initialBoard :: Board
initialBoard = listArray (1, 64) [initCell i | i <- [1..64]]
  where
    initCell i =
      let r = (i - 1) `div` 8 + 1
          c = (i - 1) `mod` 8 + 1
      in if (r + c) `mod` 2 == 0 then None
         else if r <= 3 then Black
         else if r >= 6 then White
         else Empty

-- ============================================================
-- Властивості фігур
-- ============================================================

-- | Визначає гравця для фігури (аналог Prolog belongs_to/2)
belongsTo :: Cell -> Maybe Player
belongsTo Black     = Just PlayerBlack
belongsTo BlackKing = Just PlayerBlack
belongsTo White     = Just PlayerWhite
belongsTo WhiteKing = Just PlayerWhite
belongsTo _         = Nothing

-- | Противник гравця (аналог Prolog opponent/2)
opponent :: Player -> Player
opponent PlayerBlack = PlayerWhite
opponent PlayerWhite = PlayerBlack

-- | Перевіряє, чи є фігура дамкою (аналог Prolog is_king/1)
isKing :: Cell -> Bool
isKing BlackKing = True
isKing WhiteKing = True
isKing _         = False

-- | Напрямок руху пішака (аналог Prolog pawn_dr/2)
pawnDr :: Player -> Int
pawnDr PlayerBlack =  1
pawnDr PlayerWhite = -1

-- | Рядок перетворення на дамку (аналог Prolog promo_row/2)
promoRow :: Player -> Int
promoRow PlayerBlack = 8
promoRow PlayerWhite = 1

-- | Напрямки руху фігури (аналог Prolog piece_dirs/2)
-- Дамка: всі 4 діагоналі; пішак: 2 діагоналі вперед
pieceDirs :: Cell -> [(Int, Int)]
pieceDirs p
  | isKing p  = [(1, 1), (1, -1), (-1, 1), (-1, -1)]
  | otherwise = case belongsTo p of
      Just pl -> let dr = pawnDr pl in [(dr, 1), (dr, -1)]
      Nothing -> []

-- | Перетворення пішака на дамку (аналог Prolog maybe_promote/3)
maybePromote :: Cell -> Int -> Cell
maybePromote p r = case belongsTo p of
  Just pl | r == promoRow pl -> if pl == PlayerBlack then BlackKing else WhiteKing
  _                          -> p

-- ============================================================
-- Генерація ходів
-- ============================================================

-- | Сортує та видаляє дублікати (аналог Prolog sort/2)
sortUniq :: Ord a => [a] -> [a]
sortUniq = map head . group . sort

-- | Всі легальні ходи (аналог Prolog all_legal_moves/3)
-- Якщо є удари — лише удари (обов'язкове взяття).
allLegalMoves :: Board -> Player -> [Move]
allLegalMoves board player =
  let caps  = sortUniq $ concatMap (\(r, c) -> captureMovesFrom board player r c) positions
      simps = sortUniq $ concatMap (\(r, c) -> simpleMovesFrom board r c) positions
  in if not (null caps) then caps else simps
  where
    positions = [(r, c) | r <- [1..8], c <- [1..8],
                          validPos r c,
                          belongsTo (getCell board r c) == Just player]

-- | Прості ходи без взяття з (r, c) (аналог Prolog simple_move/3)
simpleMovesFrom :: Board -> Int -> Int -> [Move]
simpleMovesFrom board r c =
  [Move (r, c) (r2, c2) [] |
   (dr, dc) <- pieceDirs (getCell board r c),
   let r2 = r + dr,
   let c2 = c + dc,
   validPos r2 c2,
   getCell board r2 c2 == Empty]

-- | Ходи зі взяттям з (r, c) (аналог Prolog capture_move/3)
captureMovesFrom :: Board -> Player -> Int -> Int -> [Move]
captureMovesFrom board player r c =
  [Move (r, c) (fr, fc) caps |
   (fr, fc, caps) <- jumpChain board player (getCell board r c) r c [],
   not (null caps)]

-- | Ланцюг взять (аналог Prolog jump_chain/9)
-- Повертає список (finalR, finalC, captures).
-- Клауза 1: робимо удар і продовжуємо
-- Клауза 2: базовий випадок — бити більше не можна
jumpChain :: Board -> Player -> Cell -> Int -> Int -> [(Int, Int)]
          -> [(Int, Int, [(Int, Int)])]
jumpChain board player piece r c used =
  let jumps = tryJumps board player piece r c used
  in if null jumps
     then [(r, c, [])]
     else jumps

-- | Спроба ударів у всіх напрямках
tryJumps :: Board -> Player -> Cell -> Int -> Int -> [(Int, Int)]
         -> [(Int, Int, [(Int, Int)])]
tryJumps board player piece r c used =
  concatMap tryDir (pieceDirs piece)
  where
    opp = opponent player
    tryDir (dr, dc) =
      let mr = r + dr
          mc = c + dc
          r2 = r + 2 * dr
          c2 = c + 2 * dc
      in if validPos mr mc && validPos r2 c2
            && belongsTo (getCell board mr mc) == Just opp
            && (mr, mc) `notElem` used
            && getCell board r2 c2 == Empty
         then
           let b1 = setCell board r c Empty
               b2 = setCell b1 mr mc Empty
               np = maybePromote piece r2
               b3 = setCell b2 r2 c2 np
               newUsed = (mr, mc) : used
           in if np /= piece
              -- Промоція: зупиняємо ланцюг (правило англійських шашок)
              then [(r2, c2, [(mr, mc)])]
              else [(fr, fc, (mr, mc) : caps) |
                    (fr, fc, caps) <- jumpChain b3 player np r2 c2 newUsed]
         else []

-- ============================================================
-- Застосування ходу (аналог Prolog apply_move/4)
-- ============================================================

applyMove :: Board -> Move -> Board
applyMove board (Move (r1, c1) (rf, cf) caps) =
  let piece = getCell board r1 c1
      b1    = setCell board r1 c1 Empty
      b2    = removePieces b1 caps
      np    = maybePromote piece rf
  in setCell b2 rf cf np

-- | Видаляє взяті фігури (аналог Prolog remove_pieces/3)
removePieces :: Board -> [(Int, Int)] -> Board
removePieces = foldl (\b (r, c) -> setCell b r c Empty)

-- ============================================================
-- Кінець гри (аналог Prolog game_over/3)
-- ============================================================

-- | Перевіряє виграш (нічія перевіряється окремо через MC > 100)
gameOver :: Board -> Player -> Maybe Player
gameOver board player
  | not (hasPiece board player)       = Just (opponent player)
  | null (allLegalMoves board player) = Just (opponent player)
  | otherwise                         = Nothing

-- | Чи є хоч одна фігура гравця (аналог Prolog has_piece/2)
hasPiece :: Board -> Player -> Bool
hasPiece board player =
  any (\(r, c) -> belongsTo (getCell board r c) == Just player)
      [(r, c) | r <- [1..8], c <- [1..8], validPos r c]

-- ============================================================
-- Статична оцінка позиції (аналог Prolog evaluate/3)
-- ============================================================

-- | Оцінка позиції для гравця. Позитивне = краще для player.
evaluate :: Board -> Player -> Int
evaluate board player =
  materialScore board player - materialScore board (opponent player)

-- | Матеріальна та позиційна оцінка (аналог Prolog material_score/3)
--   Шашка = 100, дамка = 300, центр (стовпці 3-6) = +10, просування = +5/рядок
materialScore :: Board -> Player -> Int
materialScore board player = sum
  [pieceValue r c | r <- [1..8], c <- [1..8],
   validPos r c, belongsTo (getCell board r c) == Just player]
  where
    pieceValue r c =
      let p         = getCell board r c
          matVal    = if isKing p then 300 else 100
          centBonus = if c >= 3 && c <= 6 then 10 else 0
          advBonus
            | isKing p              = 0
            | player == PlayerBlack = (r - 1) * 5
            | otherwise             = (8 - r) * 5
      in matVal + centBonus + advBonus

-- ============================================================
-- Alpha-Beta MinMax (аналог Prolog модуля alphabeta)
-- ============================================================

-- | Адаптивна глибина пошуку (аналог Prolog adaptive_depth/2)
-- Дебют (перші 6 ходів): 4; середня гра та ендшпіль: 7
adaptiveDepth :: Int -> Int
adaptiveDepth mc = if mc < 6 then 4 else 7

-- | Найкращий хід (аналог Prolog best_move/4)
bestMove :: Board -> Player -> Int -> Maybe Move
bestMove board player mc =
  case allLegalMoves board player of
    []    -> Nothing
    moves -> Just $ snd $ abBest moves board player mc depth (-inf) inf
  where
    depth = adaptiveDepth mc

-- | Перебір ходів у кореневому вузлі (аналог Prolog ab_best/9)
-- Клауза 1: єдиний хід
abBest :: [Move] -> Board -> Player -> Int -> Int -> Int -> Int -> (Int, Move)
abBest [m] board player mc depth alpha beta =
  let nb  = applyMove board m
      mc1 = mc + 1
      opp = opponent player
      val = abMin nb opp player mc1 (depth - 1) alpha beta
  in (val, m)
-- Клауза 2: кілька ходів — перебір з відсіканням
abBest (m:rest) board player mc depth alpha beta =
  let nb     = applyMove board m
      mc1    = mc + 1
      opp    = opponent player
      val    = abMin nb opp player mc1 (depth - 1) alpha beta
      alpha1 = if val > alpha then val else alpha
  in if alpha1 >= beta
     then (val, m)                -- бета-відсікання: цей хід спричинив його
     else let (restVal, restMove) = abBest rest board player mc depth alpha1 beta
          in if val > alpha       -- чи цей хід підняв альфу?
             then if restVal > val    -- чи решта знайшла ще краще?
                  then (restVal, restMove)
                  else (val, m)
             else (restVal, restMove) -- цей хід гірший, використовуємо результат решти
abBest [] _ _ _ _ _ _ = error "abBest: порожній список ходів"

-- | Значення перемоги: +inf якщо переможець = root, -inf інакше
winVal :: Player -> Player -> Int
winVal winner rootPlayer = if winner == rootPlayer then inf else -inf

-- | MAX-вузол (аналог Prolog ab_max/8)
abMax :: Board -> Player -> Player -> Int -> Int -> Int -> Int -> Int
abMax board player rootPlayer mc depth alpha beta
  | depth == 0                = evaluate board rootPlayer
  | mc > 100                  = 0
  | not (hasPiece board player) = winVal (opponent player) rootPlayer
  | otherwise =
      let moves = allLegalMoves board player
          opp   = opponent player
      in if null moves
         then winVal opp rootPlayer
         else abMaxList moves board player opp rootPlayer mc depth alpha beta

-- | Перебір ходів у MAX-вузлі (аналог Prolog ab_max_list)
abMaxList :: [Move] -> Board -> Player -> Player -> Player
          -> Int -> Int -> Int -> Int -> Int
abMaxList [] _ _ _ _ _ _ alpha _ = alpha
abMaxList (m:rest) board player opp root mc depth alpha beta =
  let nb     = applyMove board m
      mc1    = mc + 1
      depth1 = depth - 1
      v      = abMin nb opp root mc1 depth1 alpha beta
      alpha1 = max alpha v
  in if alpha1 >= beta
     then alpha1
     else abMaxList rest board player opp root mc depth alpha1 beta

-- | MIN-вузол (аналог Prolog ab_min/8)
abMin :: Board -> Player -> Player -> Int -> Int -> Int -> Int -> Int
abMin board player rootPlayer mc depth alpha beta
  | depth == 0                = evaluate board rootPlayer
  | mc > 100                  = 0
  | not (hasPiece board player) = winVal (opponent player) rootPlayer
  | otherwise =
      let moves = allLegalMoves board player
          opp   = opponent player
      in if null moves
         then winVal opp rootPlayer
         else abMinList moves board player opp rootPlayer mc depth alpha beta

-- | Перебір ходів у MIN-вузлі (аналог Prolog ab_min_list)
abMinList :: [Move] -> Board -> Player -> Player -> Player
          -> Int -> Int -> Int -> Int -> Int
abMinList [] _ _ _ _ _ _ _ beta = beta
abMinList (m:rest) board player opp root mc depth alpha beta =
  let nb     = applyMove board m
      mc1    = mc + 1
      depth1 = depth - 1
      v      = abMax nb opp root mc1 depth1 alpha beta
      beta1  = min beta v
  in if alpha >= beta1
     then beta1
     else abMinList rest board player opp root mc depth alpha beta1

-- ============================================================
-- Відображення
-- ============================================================

-- | Символ для відображення клітинки
cellChar :: Cell -> Char
cellChar None      = ' '
cellChar Empty     = '.'
cellChar Black     = 'b'
cellChar White     = 'w'
cellChar BlackKing = 'B'
cellChar WhiteKing = 'W'

-- | Текстове представлення дошки
showBoard :: Board -> String
showBoard board = intercalate "\n" $
  "  1 2 3 4 5 6 7 8" :
  [show r ++ " " ++ unwords [[cellChar (getCell board r c)] | c <- [1..8]]
  | r <- [1..8]]

-- | Текстове представлення ходу
showMove :: Move -> String
showMove (Move (r1, c1) (r2, c2) caps) =
  "(" ++ show r1 ++ "," ++ show c1 ++ ")" ++
  "\x2192" ++
  "(" ++ show r2 ++ "," ++ show c2 ++ ")" ++
  if null caps then "" else " x" ++ show (length caps)

-- | Парсинг ходу (формат: рядок_від кол_від рядок_до кол_до, 1-based)
parseInput :: String -> Maybe (Int, Int, Int, Int)
parseInput s = case words s of
  [a, b, c, d] -> case mapM readMaybe [a, b, c, d] of
    Just [fr, fc, tr, tc] -> Just (fr, fc, tr, tc)
    _                     -> Nothing
  _ -> Nothing
  where
    readMaybe :: String -> Maybe Int
    readMaybe str = case reads str of
      [(n, "")] -> Just n
      _         -> Nothing
