{-
checkers_haskell.hs — Шашки на Haskell (порівняльна реалізація)

Демонструє той самий алгоритм MinMax + Alpha-Beta, що і Prolog-версія.

Компіляція: ghc -O2 Checkers.hs checkers_haskell.hs -o checkers_haskell
Запуск:     ./checkers_haskell
Гра у консолі: людина (чорні) проти Комп'ютера (білі).
-}

module Main where

import Checkers
import Data.List (intercalate)
import System.IO (hFlush, stdout)

-- ============================================================
-- Консольна гра
-- ============================================================

-- | Основний цикл консольної гри
play :: IO ()
play = do
  putStrLn "=== Шашки: ви (чорні b) проти Комп'ютера (білі w) ==="
  putStrLn "Формат ходу: рядок_від кол_від рядок_до кол_до (1-based)"
  putStrLn "Наприклад: 3 2 4 3"
  putStrLn ""
  gameLoop initialBoard 0 PlayerBlack

-- | Цикл гри
gameLoop :: Board -> Int -> Player -> IO ()
gameLoop board mc turn = do
  putStrLn (showBoard board)
  putStrLn $ "Ходів: " ++ show mc
  case gameOver board turn of
    Just winner ->
      if winner == PlayerBlack
        then putStrLn "Ви виграли!"
        else putStrLn "Комп'ютер виграв!"
    Nothing
      | mc > 100          -> putStrLn "Нічия!"
      | turn == PlayerBlack -> humanTurn board mc
      | otherwise           -> aiTurn board mc

-- | Хід людини
humanTurn :: Board -> Int -> IO ()
humanTurn board mc = do
  let moves = allLegalMoves board PlayerBlack
  if null moves
    then putStrLn "У вас немає ходів. Комп'ютер виграв!"
    else do
      putStrLn $ "Доступні ходи: " ++ intercalate ", " (map showMove moves)
      putStr "Ваш хід: "
      hFlush stdout
      raw <- getLine
      case parseInput raw of
        Nothing -> do
          putStrLn "Невірний формат. Спробуйте ще раз."
          putStrLn ""
          gameLoop board mc PlayerBlack
        Just (fr, fc, tr, tc) ->
          case filter (matchMove fr fc tr tc) moves of
            (chosen:_) -> do
              let nb = applyMove board chosen
              putStrLn ""
              gameLoop nb (mc + 1) PlayerWhite
            [] -> do
              putStrLn "Такого ходу немає. Спробуйте ще раз."
              putStrLn ""
              gameLoop board mc PlayerBlack
  where
    matchMove fr fc tr tc (Move (r1, c1) (r2, c2) _) =
      r1 == fr && c1 == fc && r2 == tr && c2 == tc

-- | Хід комп'ютера
aiTurn :: Board -> Int -> IO ()
aiTurn board mc = do
  putStrLn "Комп'ютер думає..."
  case bestMove board PlayerWhite mc of
    Nothing -> putStrLn "Комп'ютер не має ходів. Ви виграли!"
    Just mv -> do
      putStrLn $ "Комп'ютер: " ++ showMove mv
      let nb = applyMove board mv
      putStrLn ""
      gameLoop nb (mc + 1) PlayerBlack

-- ============================================================
-- Точка входу
-- ============================================================

main :: IO ()
main = play
