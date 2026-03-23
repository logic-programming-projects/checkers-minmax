"""
checkers_python.py — Шашки на Python (порівняльна реалізація)

Демонструє той самий алгоритм MinMax + Alpha-Beta, що і Prolog-версія,
але в імперативному стилі.

Запуск: python checkers_python.py
Гра у консолі: людина (чорні) проти Компʼютер (білі).
"""

from __future__ import annotations
import copy
from typing import Optional

# ---
# Константи
# ---
EMPTY       = "."
BLACK       = "b"
WHITE       = "w"
BLACK_KING  = "B"
WHITE_KING  = "W"

# Адаптивна глибина: 4 у дебюті (перші 6 ходів), 7 у середній грі
# Відповідає Prolog-версії: adaptive_depth(MC, Depth)
SEARCH_DEPTH_OPENING = 4
SEARCH_DEPTH_MAIN    = 7
INF                  = 9999

# ============================================================
# Клас Board — представлення дошки
# ============================================================
class Board:
    """
    Дошка 8x8.
    Клітинки зберігаються у двовимірному списку board[r][c] (0-based).
    Ігрові клітинки: (r + c) % 2 == 1
    """

    def __init__(self) -> None:
        # Ініціалізуємо порожню дошку
        self.grid: list[list[str]] = [
            [EMPTY] * 8 for _ in range(8)
        ]
        self.move_count: int = 0

    # --------------------------------------------------------
    # Фабричний метод: початкова розстановка
    # --------------------------------------------------------
    @staticmethod
    def initial() -> "Board":
        b = Board()
        for r in range(8):
            for c in range(8):
                if (r + c) % 2 == 0:
                    continue            # світла клітинка — не використовується
                if r < 3:
                    b.grid[r][c] = BLACK
                elif r > 4:
                    b.grid[r][c] = WHITE
        return b

    # --------------------------------------------------------
    def copy(self) -> "Board":
        """Повертає глибоку копію дошки."""
        nb = Board()
        nb.grid = [row[:] for row in self.grid]
        nb.move_count = self.move_count
        return nb

    # --------------------------------------------------------
    def get(self, r: int, c: int) -> str:
        """Значення клітинки (r, c)."""
        return self.grid[r][c]

    def set(self, r: int, c: int, v: str) -> None:
        """Встановити значення клітинки."""
        self.grid[r][c] = v

    # --------------------------------------------------------
    @staticmethod
    def is_dark(r: int, c: int) -> bool:
        return (r + c) % 2 == 1

    @staticmethod
    def in_bounds(r: int, c: int) -> bool:
        return 0 <= r < 8 and 0 <= c < 8

    # --------------------------------------------------------
    def belongs_to(self, r: int, c: int) -> Optional[str]:
        """Повертає 'black' або 'white', або None для порожньої клітинки."""
        v = self.get(r, c)
        if v in (BLACK, BLACK_KING):
            return "black"
        if v in (WHITE, WHITE_KING):
            return "white"
        return None

    def is_king(self, r: int, c: int) -> bool:
        return self.get(r, c) in (BLACK_KING, WHITE_KING)

    # --------------------------------------------------------
    # Напрямки руху
    # --------------------------------------------------------
    def _directions(self, r: int, c: int) -> list[tuple[int, int]]:
        """Список напрямків (dr, dc) для фігури на (r, c)."""
        v = self.get(r, c)
        if v in (BLACK_KING, WHITE_KING):
            return [(-1, -1), (-1, 1), (1, -1), (1, 1)]
        if v == BLACK:
            return [(1, -1), (1, 1)]    # чорні йдуть вниз
        if v == WHITE:
            return [(-1, -1), (-1, 1)]  # білі йдуть вгору
        return []

    # --------------------------------------------------------
    # Промоція
    # --------------------------------------------------------
    def maybe_promote(self, r: int, c: int) -> None:
        """Перетворює пішака на дамку, якщо досяг крайнього рядка."""
        v = self.get(r, c)
        if v == BLACK and r == 7:
            self.set(r, c, BLACK_KING)
        elif v == WHITE and r == 0:
            self.set(r, c, WHITE_KING)

    # --------------------------------------------------------
    # Генерація ходів
    # --------------------------------------------------------
    def all_legal_moves(self, player: str) -> list["Move"]:
        """
        Повертає список легальних ходів для player.
        Якщо є взяття — повертає лише їх (обов'язкове взяття).
        """
        captures = []
        simples  = []
        for r in range(8):
            for c in range(8):
                if self.belongs_to(r, c) == player:
                    captures.extend(self._capture_moves(r, c, [], self))
                    simples.extend(self._simple_moves(r, c))
        return captures if captures else simples

    def _simple_moves(self, r: int, c: int) -> list["Move"]:
        """Прості ходи без взяття з (r, c)."""
        moves = []
        for dr, dc in self._directions(r, c):
            nr, nc = r + dr, c + dc
            if self.in_bounds(nr, nc) and self.get(nr, nc) == EMPTY:
                moves.append(Move(r, c, nr, nc, []))
        return moves

    def _capture_moves(
        self, r: int, c: int,
        used: list[tuple[int, int]],
        board: "Board"
    ) -> list["Move"]:
        """
        Рекурсивно будує всі ланцюги взяття, що починаються з (r, c).
        used — список вже взятих позицій у поточному ланцюзі.
        """
        piece   = board.get(r, c)
        player  = board.belongs_to(r, c)
        opp     = "white" if player == "black" else "black"
        found   = []

        for dr, dc in board._directions(r, c):
            mr, mc = r + dr, c + dc       # клітинка з ворожою фігурою
            er, ec = r + 2*dr, c + 2*dc   # клітинка приземлення

            if not (board.in_bounds(mr, mc) and board.in_bounds(er, ec)):
                continue
            if board.belongs_to(mr, mc) != opp:
                continue
            if (mr, mc) in used:
                continue                  # не брати двічі
            if board.get(er, ec) != EMPTY:
                continue

            # Тимчасово застосовуємо удар
            nb = board.copy()
            nb.set(r,  c,  EMPTY)
            nb.set(mr, mc, EMPTY)
            nb.set(er, ec, piece)
            nb.maybe_promote(er, ec)
            new_used = used + [(mr, mc)]

            # Якщо після промоції — зупиняємось
            if nb.get(er, ec) != piece:
                found.append(Move(r, c, er, ec, new_used))
                continue

            # Продовжуємо ланцюг
            further = nb._capture_moves(er, ec, new_used, nb)
            if further:
                # Додаємо початкову позицію до кожного подальшого ходу
                for fm in further:
                    found.append(Move(r, c, fm.to_r, fm.to_c, fm.captures))
            else:
                found.append(Move(r, c, er, ec, new_used))

        return found

    # --------------------------------------------------------
    # Застосування ходу
    # --------------------------------------------------------
    def apply_move(self, move: "Move") -> "Board":
        """Повертає нову дошку після застосування move."""
        nb = self.copy()
        piece = nb.get(move.from_r, move.from_c)
        nb.set(move.from_r, move.from_c, EMPTY)
        for cr, cc in move.captures:
            nb.set(cr, cc, EMPTY)
        nb.set(move.to_r, move.to_c, piece)
        nb.maybe_promote(move.to_r, move.to_c)
        nb.move_count += 1
        return nb

    # --------------------------------------------------------
    # Перевірка кінця гри
    # --------------------------------------------------------
    def game_over(self, player: str) -> Optional[str]:
        """
        Повертає переможця ('black' або 'white') або 'draw',
        якщо гра закінчена. Інакше — None.
        """
        opp = "white" if player == "black" else "black"
        if not self._has_piece(player) or not self.all_legal_moves(player):
            return opp
        if self.move_count > 100:
            return "draw"
        return None

    def _has_piece(self, player: str) -> bool:
        for r in range(8):
            for c in range(8):
                if self.belongs_to(r, c) == player:
                    return True
        return False

    # --------------------------------------------------------
    # Оцінка позиції
    # --------------------------------------------------------
    def evaluate(self, player: str) -> int:
        """
        Статична оцінка позиції для player. Позитивне — краще для player.
        Відповідає Prolog evaluate/3 + piece_value/3:
          Матеріал:   шашка = 100, дамка = 300
          Центр:      +10 за стовпці 2-5 (0-based) = стовпці 3-6 (1-based)
          Просування: +5 за кожен рядок вперед
        """
        opp = "white" if player == "black" else "black"
        return self._score_for(player) - self._score_for(opp)

    def _score_for(self, player: str) -> int:
        """
        Підраховує матеріальну та позиційну оцінку для одного гравця.
        Відповідає Prolog material_score/3.
        """
        score = 0
        for r in range(8):
            for c in range(8):
                if self.belongs_to(r, c) != player:
                    continue
                # Матеріальна вартість (Prolog: is_king -> 300 ; 100)
                score += 300 if self.is_king(r, c) else 100
                # Бонус за центральні стовпці (Prolog: C >= 3, C =< 6 -> +10)
                if 2 <= c <= 5:
                    score += 10
                # Бонус за просування вперед (Prolog: pawn_dr * 5)
                if not self.is_king(r, c):
                    if player == "black":
                        score += r * 5        # чорні йдуть до рядка 7 (0-based)
                    else:
                        score += (7 - r) * 5  # білі йдуть до рядка 0 (0-based)
        return score

    # --------------------------------------------------------
    def __str__(self) -> str:
        """Текстове представлення дошки для консолі."""
        lines = ["  1 2 3 4 5 6 7 8"]
        for r in range(8):
            row_str = f"{r+1} " + " ".join(
                self.grid[r][c] if self.is_dark(r, c) else " "
                for c in range(8)
            )
            lines.append(row_str)
        return "\n".join(lines)


# ---
# Клас Move
# ---
class Move:
    """Представляє один хід: початок, кінець, список взятих позицій."""

    def __init__(
        self,
        from_r: int, from_c: int,
        to_r:   int, to_c:   int,
        captures: list[tuple[int, int]]
    ) -> None:
        self.from_r   = from_r
        self.from_c   = from_c
        self.to_r     = to_r
        self.to_c     = to_c
        self.captures = captures   # список (r, c) взятих фігур

    def __repr__(self) -> str:
        caps = f" x{len(self.captures)}" if self.captures else ""
        return (f"({self.from_r+1},{self.from_c+1})"
                f"→({self.to_r+1},{self.to_c+1}){caps}")


# ============================================================
# Alpha-Beta MinMax
# ============================================================
def best_move(board: Board, player: str) -> Optional[Move]:
    """
    Знаходить найкращий хід для player за допомогою Alpha-Beta.
    Глибина адаптивна: 4 у дебюті (move_count < 6), 7 у решті гри.
    Відповідає Prolog: adaptive_depth(MC, Depth).
    Повертає Move або None, якщо ходів немає.
    """
    moves = board.all_legal_moves(player)
    if not moves:
        return None

    # Адаптивна глибина залежно від фази гри
    depth = SEARCH_DEPTH_OPENING if board.move_count < 6 else SEARCH_DEPTH_MAIN

    best_val    = -INF
    best_mv     = moves[0]
    alpha, beta = -INF, INF
    opp = "white" if player == "black" else "black"

    for m in moves:
        nb  = board.apply_move(m)
        val = ab_min(nb, opp, player, depth - 1, alpha, beta)
        if val > best_val:
            best_val = val
            best_mv  = m
        alpha = max(alpha, best_val)

    return best_mv


def _win_val(winner: str, root: str) -> int:
    """Значення перемоги: +INF якщо переможець = root, -INF інакше."""
    return INF if winner == root else -INF


def ab_max(
    board: Board, current: str, root: str,
    depth: int, alpha: int, beta: int
) -> int:
    """
    MAX-вузол: хід root-гравця, шукаємо максимум.

    Параметри:
        board   — поточна дошка
        current — гравець, що ходить у цьому вузлі
        root    — Компʼютер-гравець (відносно якого обчислюємо оцінку)
        depth   — залишкова глибина
        alpha   — поточна альфа-межа
        beta    — поточна бета-межа
    """
    if depth == 0:
        return board.evaluate(root)
    if board.move_count > 100:
        return 0

    opp = "white" if current == "black" else "black"

    if not board._has_piece(current):
        return _win_val(opp, root)

    moves = board.all_legal_moves(current)
    if not moves:
        return _win_val(opp, root)

    val = -INF
    for m in moves:
        nb   = board.apply_move(m)
        val  = max(val, ab_min(nb, opp, root, depth - 1, alpha, beta))
        alpha = max(alpha, val)
        if alpha >= beta:
            break           # бета-відсікання

    return val


def ab_min(
    board: Board, current: str, root: str,
    depth: int, alpha: int, beta: int
) -> int:
    """
    MIN-вузол: хід опонента, шукаємо мінімум.

    Параметри — аналогічні до ab_max.
    """
    if depth == 0:
        return board.evaluate(root)
    if board.move_count > 100:
        return 0

    opp = "white" if current == "black" else "black"

    if not board._has_piece(current):
        return _win_val(opp, root)

    moves = board.all_legal_moves(current)
    if not moves:
        return _win_val(opp, root)

    val = INF
    for m in moves:
        nb  = board.apply_move(m)
        val = min(val, ab_max(nb, opp, root, depth - 1, alpha, beta))
        beta = min(beta, val)
        if alpha >= beta:
            break           # альфа-відсікання

    return val


# ---
# Консольна гра
# ---
def parse_input(s: str) -> Optional[tuple[int, int, int, int]]:
    """
    Парсить хід вигляду '3 2 4 3' (рядок_від кол_від рядок_до кол_до).
    Повертає кортеж (fr, fc, tr, tc) у 0-based, або None.
    """
    parts = s.strip().split()
    if len(parts) != 4:
        return None
    try:
        fr, fc, tr, tc = [int(x) - 1 for x in parts]
        return fr, fc, tr, tc
    except ValueError:
        return None


def play() -> None:
    """Основний цикл консольної гри."""
    board  = Board.initial()
    human  = "black"
    ai     = "white"
    turn   = "black"

    print("=== Шашки: ви (чорні b) проти Компʼютера (білі w) ===")
    print("Формат ходу: рядок_від кол_від рядок_до кол_до (1-based)")
    print("Наприклад: 3 2 4 3\n")

    while True:
        print(board)
        print(f"Ходів: {board.move_count}")

        # Перевірка кінця гри
        result = board.game_over(turn)
        if result:
            if result == "draw":
                print("Нічия!")
            elif result == human:
                print("Ви виграли!")
            else:
                print("Компʼютер виграв!")
            break

        if turn == human:
            # --- Хід людини ---
            moves = board.all_legal_moves(human)
            if not moves:
                print("У вас немає ходів. Компʼютер виграв!")
                break

            print(f"Доступні ходи: {moves}")
            raw = input("Ваш хід: ").strip()
            coords = parse_input(raw)
            if coords is None:
                print("Невірний формат. Спробуйте ще раз.\n")
                continue

            fr, fc, tr, tc = coords
            chosen = next(
                (m for m in moves
                 if m.from_r == fr and m.from_c == fc
                 and m.to_r  == tr and m.to_c  == tc),
                None
            )
            if chosen is None:
                print("Такого ходу немає. Спробуйте ще раз.\n")
                continue

            board = board.apply_move(chosen)
            turn  = ai

        else:
            # --- Хід Компʼютера ---
            print("Компʼютер думає...")
            mv = best_move(board, ai)
            if mv is None:
                print("Компʼютер не має ходів. Ви виграли!")
                break
            print(f"Компʼютер: {mv}")
            board = board.apply_move(mv)
            turn  = human

        print()


# ---
# Точка входу
# ---
if __name__ == "__main__":
    play()