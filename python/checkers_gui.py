"""
checkers_gui.py — Шашки з графічним інтерфейсом (pygame)
Гравець проти Комп'ютера. Алгоритм: MinMax + Alpha-Beta, глибина 5.
Запуск: python checkers_gui.py
"""

import sys
import threading
import pygame
from checkers_python import Board, best_move

# ============================================================
# Константи
# ============================================================
CELL       = 70
BOARD_SIZE = CELL * 8   # 560
PANEL_W    = 270
WIN_W      = BOARD_SIZE + PANEL_W
WIN_H      = BOARD_SIZE
FPS        = 60

# Кольорова палітра
BG          = (218, 218, 218)
LIGHT_SQ    = (240, 228, 208)
DARK_SQ     = (145,  98,  58)
PANEL_BG    = (232, 232, 232)
CARD_BG     = (255, 255, 255)
CARD_BORDER = (205, 205, 205)
BLACK_PC    = ( 28,  28,  28)
BLACK_RIM   = (  0,   0,   0)
WHITE_PC    = (248, 248, 248)
WHITE_RIM   = (170, 170, 170)
CROWN_CLR   = (210, 160,  10)
SEL_CLR     = ( 70, 180,  70)
HINT_CLR    = (240, 195,  30)
CAP_CLR     = (210,  55,  55)
TEXT_DARK   = ( 40,  40,  40)
TEXT_GREY   = (130, 130, 130)
TEXT_GREEN  = ( 35, 130,  35)
TEXT_RED    = (190,  45,  45)
BTN_BG      = ( 50,  50,  50)
BTN_HOV     = ( 80,  80,  80)
BTN_TEXT    = (255, 255, 255)
BTN2_BG     = (255, 255, 255)
BTN2_HOV    = (235, 235, 235)
BTN2_BORDER = (180, 180, 180)

CHAR_TO_VAL = {
    'b': 'black', 'w': 'white',
    'B': 'black_king', 'W': 'white_king',
}

# ============================================================
# Шрифти з підтримкою кирилиці
# ============================================================
def make_fonts():
    """
    Шукає системний шрифт із підтримкою кирилиці.
    Пробує список кандидатів по черзі.
    """
    candidates = ['dejavusans', 'freesans', 'liberationsans',
                  'notosans', 'ubuntu', 'arial']
    chosen = None
    for name in candidates:
        try:
            f = pygame.font.SysFont(name, 16)
            # Перевірка: якщо 'Ш' рендериться нормально (ширина > 4px)
            if f.render('Ш', True, (0, 0, 0)).get_width() > 4:
                chosen = name
                break
        except Exception:
            pass

    def f(size, bold=False):
        if chosen:
            return pygame.font.SysFont(chosen, size, bold=bold)
        return pygame.font.Font(None, size)   # запасний варіант

    return {
        'title': f(24, bold=True),
        'body':  f(15),
        'small': f(12),
        'log':   f(13),
        'crown': f(18, bold=True),
    }

# ============================================================
# Стан гри
# ============================================================
class GameState:
    def __init__(self, human='black'):
        self.board      = Board.initial()
        self.human      = human
        self.computer   = 'white' if human == 'black' else 'black'
        self.turn       = 'black'
        self.selected   = None
        self.legal      = []
        self.highlights = []
        self.game_over  = False
        self.thinking   = False
        self.status     = 'Ваш хід — оберіть шашку'
        self.status_clr = TEXT_GREEN
        self.log        = []
        self._refresh_legal()
        col = 'чорні ⚫' if human == 'black' else 'білі ⚪'
        self.add_log(f'Нова гра. Ви — {col}', 'sys')

    def _refresh_legal(self):
        self.legal = self.board.all_legal_moves(self.human)

    def select(self, r, c):
        """Обрати шашку на клітинці (r, c) — 0-based."""
        if self.board.belongs_to(r, c) == self.human:
            self.selected   = (r, c)
            self.highlights = [m for m in self.legal
                               if m.from_r == r and m.from_c == c]
        else:
            self.selected   = None
            self.highlights = []

    def try_move(self, r, c):
        """Спробувати зробити хід у (r, c). Повертає True якщо успішно."""
        move = next((m for m in self.highlights
                     if m.to_r == r and m.to_c == c), None)
        if move:
            self.board      = self.board.apply_move(move)
            self.selected   = None
            self.highlights = []
            cap = f' ×{len(move.captures)}' if move.captures else ''
            self.add_log(
                f'Ви: ({move.from_r+1},{move.from_c+1})'
                f'→({move.to_r+1},{move.to_c+1}){cap}', 'user')
            return True
        return False

    def add_log(self, msg, kind='sys'):
        self.log.append((msg, kind))
        if len(self.log) > 30:
            self.log.pop(0)

    def count(self, player):
        return sum(
            1 for r in range(8) for c in range(8)
            if self.board.belongs_to(r, c) == player
        )

# ============================================================
# Малювання дошки
# ============================================================
def draw_board(surf):
    """Малює 8×8 клітинки дошки."""
    for r in range(8):
        for c in range(8):
            clr = DARK_SQ if (r + c) % 2 == 1 else LIGHT_SQ
            pygame.draw.rect(surf, clr, (c*CELL, r*CELL, CELL, CELL))

def draw_coords(surf, fonts):
    """Малює координатні мітки (цифри по вертикалі, літери по горизонталі)."""
    f = fonts['small']
    for i in range(8):
        c1 = LIGHT_SQ if i % 2 == 0 else DARK_SQ
        surf.blit(f.render(str(i+1), True, c1), (3, i*CELL + 3))
        c2 = LIGHT_SQ if i % 2 == 1 else DARK_SQ
        surf.blit(f.render(chr(65+i), True, c2),
                  (i*CELL + CELL - 13, BOARD_SIZE - 15))

def draw_highlights(surf, gs):
    """
    Виділяє обрану шашку (зелена рамка) та можливі ходи
    (жовта крапка — простий хід, червоне коло — взяття).
    """
    if gs.selected is None:
        return
    r, c = gs.selected
    # Зелена рамка навколо обраної шашки
    pygame.draw.rect(surf, SEL_CLR,
                     (c*CELL+2, r*CELL+2, CELL-4, CELL-4), 3, border_radius=4)
    for m in gs.highlights:
        mx = m.to_c * CELL + CELL // 2
        my = m.to_r * CELL + CELL // 2
        if m.captures:
            pygame.draw.circle(surf, CAP_CLR, (mx, my), CELL*34//100, 3)
        else:
            pygame.draw.circle(surf, HINT_CLR, (mx, my), CELL*15//100)

def draw_piece(surf, r, c, val, fonts):
    """Малює одну шашку або дамку."""
    cx  = c * CELL + CELL // 2
    cy  = r * CELL + CELL // 2
    rad = CELL * 33 // 100
    is_black = val.startswith('black')
    is_king  = val.endswith('king')

    # Тінь
    pygame.draw.circle(surf, (0, 0, 0), (cx+2, cy+4), rad)
    # Основне коло
    pygame.draw.circle(surf, BLACK_PC if is_black else WHITE_PC, (cx, cy), rad)
    pygame.draw.circle(surf, BLACK_RIM if is_black else WHITE_RIM, (cx, cy), rad, 2)
    # Внутрішнє декоративне кільце
    inner = (75, 75, 75) if is_black else (195, 195, 195)
    pygame.draw.circle(surf, inner, (cx, cy), rad*63//100, 1)
    # Мітка дамки
    if is_king:
        lbl = fonts['crown'].render('Д', True, CROWN_CLR)
        surf.blit(lbl, lbl.get_rect(center=(cx, cy)))

def draw_pieces(surf, board, fonts):
    """Малює всі шашки з поточної дошки."""
    for r in range(8):
        for c in range(8):
            v = board.grid[r][c]
            if v != '.':
                draw_piece(surf, r, c, CHAR_TO_VAL.get(v, v), fonts)

# ============================================================
# Малювання панелі
# ============================================================
def card_rect(surf, x, y, w, h):
    """Малює білу картку з рамкою."""
    pygame.draw.rect(surf, CARD_BG,    (x, y, w, h), border_radius=8)
    pygame.draw.rect(surf, CARD_BORDER,(x, y, w, h), 1, border_radius=8)

def draw_panel_choose(surf, fonts, px, btn_black, btn_white, bbh, bwh):
    """Екран вибору кольору шашок."""
    pygame.draw.rect(surf, PANEL_BG, (px, 0, PANEL_W, WIN_H))
    f  = fonts['body']
    ft = fonts['title']
    fs = fonts['small']

    surf.blit(ft.render('Шашки', True, TEXT_DARK), (px+16, 20))
    surf.blit(f.render("vs Комп'ютер", True, TEXT_GREY), (px+16, 52))

    surf.blit(f.render('Оберіть колір:', True, TEXT_DARK), (px+16, 106))

    # Кнопка «Чорні»
    pygame.draw.rect(surf, BTN2_HOV if bbh else BTN2_BG,
                     btn_black, border_radius=8)
    pygame.draw.rect(surf, BTN2_BORDER, btn_black, 2, border_radius=8)
    pygame.draw.circle(surf, BLACK_PC,  (btn_black.x+26, btn_black.centery), 11)
    pygame.draw.circle(surf, BLACK_RIM, (btn_black.x+26, btn_black.centery), 11, 1)
    surf.blit(f.render('Чорні (ходять першими)', True, TEXT_DARK),
              (btn_black.x+46, btn_black.centery-10))

    # Кнопка «Білі»
    pygame.draw.rect(surf, BTN2_HOV if bwh else BTN2_BG,
                     btn_white, border_radius=8)
    pygame.draw.rect(surf, BTN2_BORDER, btn_white, 2, border_radius=8)
    pygame.draw.circle(surf, WHITE_PC,  (btn_white.x+26, btn_white.centery), 11)
    pygame.draw.circle(surf, WHITE_RIM, (btn_white.x+26, btn_white.centery), 11, 1)
    surf.blit(f.render('Білі', True, TEXT_DARK),
              (btn_white.x+46, btn_white.centery-10))

def draw_panel_game(surf, fonts, gs, px, btn_new, bnh):
    """Панель під час гри: статус, кнопка, рахунок, журнал."""
    pygame.draw.rect(surf, PANEL_BG, (px, 0, PANEL_W, WIN_H))
    f  = fonts['body']
    ft = fonts['title']
    fs = fonts['small']
    fl = fonts['log']

    surf.blit(ft.render('Шашки', True, TEXT_DARK), (px+16, 20))
    surf.blit(f.render("vs Комп'ютер", True, TEXT_GREY), (px+16, 52))

    y = 84

    # Статус
    card_rect(surf, px+12, y, PANEL_W-24, 38)
    s = f.render(gs.status, True, gs.status_clr)
    surf.blit(s, s.get_rect(center=(px + PANEL_W//2, y+19)))
    y += 50

    # Кнопка «Нова гра»
    pygame.draw.rect(surf, BTN_HOV if bnh else BTN_BG, btn_new, border_radius=8)
    bl = f.render('Нова гра', True, BTN_TEXT)
    surf.blit(bl, bl.get_rect(center=btn_new.center))
    y += 52

    # Картка рахунку
    card_rect(surf, px+12, y, PANEL_W-24, 100)

    def stat_row(label, val, yy, dot=None):
        lx = px + 18
        if dot is not None:
            pygame.draw.circle(surf, dot[0], (px+26, yy+9), 7)
            pygame.draw.circle(surf, dot[1], (px+26, yy+9), 7, 1)
            lx = px + 40
        surf.blit(fs.render(label, True, TEXT_GREY), (lx, yy))
        vl = f.render(str(val), True, TEXT_DARK)
        surf.blit(vl, (px+PANEL_W-26-vl.get_width(), yy))

    stat_row('Чорних шашок', gs.count('black'), y+12, (BLACK_PC, BLACK_RIM))
    pygame.draw.line(surf, CARD_BORDER, (px+18,y+34),(px+PANEL_W-18,y+34))
    stat_row('Білих шашок',  gs.count('white'), y+40, (WHITE_PC, WHITE_RIM))
    pygame.draw.line(surf, CARD_BORDER, (px+18,y+62),(px+PANEL_W-18,y+62))
    stat_row('Ходів зіграно', gs.board.move_count, y+68)
    y += 114

    # Журнал ходів
    log_h = WIN_H - y - 14
    card_rect(surf, px+12, y, PANEL_W-24, log_h)
    surf.blit(fs.render('ЖУРНАЛ', True, TEXT_GREY), (px+18, y+8))

    line_h    = 17
    max_lines = (log_h - 26) // line_h
    for i, (msg, kind) in enumerate(gs.log[-max_lines:]):
        clr = TEXT_GREEN if kind == 'user' else \
              TEXT_RED   if kind == 'comp' else TEXT_GREY
        surf.blit(fl.render(msg, True, clr), (px+18, y+24+i*line_h))

    # Напівпрозорий оверлей «думає»
    if gs.thinking:
        ov = pygame.Surface((PANEL_W-24, log_h), pygame.SRCALPHA)
        ov.fill((255, 255, 255, 200))
        surf.blit(ov, (px+12, y))
        tl = f.render("Комп'ютер думає...", True, TEXT_GREY)
        surf.blit(tl, tl.get_rect(center=(px+PANEL_W//2, y+log_h//2)))

# ============================================================
# Хід комп'ютера у фоновому потоці
# ============================================================
def start_computer_move(gs, result_holder, done_flag):
    """Запускає пошук ходу в окремому потоці, щоб не підвішувати UI."""
    gs.thinking   = True
    gs.status     = "Комп'ютер думає..."
    gs.status_clr = TEXT_GREY

    def run():
        result_holder[0] = best_move(gs.board, gs.computer)
        done_flag[0]     = True

    threading.Thread(target=run, daemon=True).start()

def apply_computer_result(gs, move):
    """Застосовує хід комп'ютера і перевіряє кінець гри."""
    gs.thinking = False
    if move:
        gs.board = gs.board.apply_move(move)
        cap = f' ×{len(move.captures)}' if move.captures else ''
        gs.add_log(
            f"Комп: ({move.from_r+1},{move.from_c+1})"
            f"→({move.to_r+1},{move.to_c+1}){cap}", 'comp')
    else:
        gs.game_over  = True
        gs.status     = 'Ви виграли!'
        gs.status_clr = TEXT_GREEN
        gs.add_log('Ви виграли!', 'sys')
        return

    result = gs.board.game_over(gs.human)
    if result:
        gs.game_over = True
        if result == 'draw':
            gs.status, gs.status_clr = 'Нічия!', TEXT_GREY
        elif result == gs.human:
            gs.status, gs.status_clr = 'Ви виграли!', TEXT_GREEN
        else:
            gs.status, gs.status_clr = "Комп'ютер виграв!", TEXT_RED
        gs.add_log(gs.status, 'sys')
    else:
        gs._refresh_legal()
        gs.turn       = gs.human
        gs.status     = 'Ваш хід — оберіть шашку'
        gs.status_clr = TEXT_GREEN

def apply_human_result(gs, result, comp_result, comp_done,
                       start_comp):
    """Обробляє результат після ходу людини."""
    if result:
        end = gs.board.game_over(gs.computer)
        if end:
            gs.game_over = True
            if end == 'draw':
                gs.status, gs.status_clr = 'Нічия!', TEXT_GREY
            elif end == gs.human:
                gs.status, gs.status_clr = 'Ви виграли!', TEXT_GREEN
            else:
                gs.status, gs.status_clr = "Комп'ютер виграв!", TEXT_RED
            gs.add_log(gs.status, 'sys')
        else:
            gs.turn = gs.computer
            start_comp()

# ============================================================
# Головний цикл
# ============================================================
def main():
    pygame.init()
    pygame.display.set_caption("Шашки — Комп'ютер")
    surf  = pygame.display.set_mode((WIN_W, WIN_H))
    clock = pygame.time.Clock()

    fonts = make_fonts()

    px        = BOARD_SIZE
    btn_new   = pygame.Rect(px+12, 136, PANEL_W-24, 40)
    btn_black = pygame.Rect(px+12, 134, PANEL_W-24, 46)
    btn_white = pygame.Rect(px+12, 190, PANEL_W-24, 46)

    choosing = True
    gs       = None

    comp_result = [None]
    comp_done   = [False]

    def start_comp():
        start_computer_move(gs, comp_result, comp_done)

    running = True
    while running:
        mp  = pygame.mouse.get_pos()
        bnh = btn_new.collidepoint(mp)   if not choosing else False
        bbh = btn_black.collidepoint(mp) if choosing     else False
        bwh = btn_white.collidepoint(mp) if choosing     else False

        # ---- Результат ходу комп'ютера ----
        if comp_done[0] and gs is not None:
            comp_done[0] = False
            apply_computer_result(gs, comp_result[0])

        # ---- Обробка подій ----
        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                running = False

            elif ev.type == pygame.MOUSEBUTTONDOWN and ev.button == 1:
                mx, my = ev.pos

                # Екран вибору кольору
                if choosing:
                    if btn_black.collidepoint(mx, my):
                        choosing = False
                        gs = GameState('black')
                    elif btn_white.collidepoint(mx, my):
                        choosing = False
                        gs = GameState('white')
                        start_comp()
                    continue

                # Кнопка «Нова гра»
                if btn_new.collidepoint(mx, my):
                    choosing     = True
                    gs           = None
                    comp_done[0] = False
                    continue

                # Клік на дошку
                if mx < BOARD_SIZE and gs and not gs.game_over and not gs.thinking:
                    r = my // CELL
                    c = mx // CELL
                    if 0 <= r < 8 and 0 <= c < 8:
                        if gs.selected is not None:
                            moved = gs.try_move(r, c)
                            if moved:
                                apply_human_result(
                                    gs, moved, comp_result, comp_done, start_comp)
                            else:
                                gs.select(r, c)
                        else:
                            gs.select(r, c)

        # ---- Малювання ----
        surf.fill(BG)
        draw_board(surf)
        draw_coords(surf, fonts)

        if choosing or gs is None:
            draw_pieces(surf, Board.initial(), fonts)
            draw_panel_choose(surf, fonts, px, btn_black, btn_white, bbh, bwh)
        else:
            draw_highlights(surf, gs)
            draw_pieces(surf, gs.board, fonts)
            draw_panel_game(surf, fonts, gs, px, btn_new, bnh)

        pygame.draw.line(surf, CARD_BORDER,
                         (BOARD_SIZE, 0), (BOARD_SIZE, WIN_H), 1)
        pygame.display.flip()
        clock.tick(FPS)

    pygame.quit()
    sys.exit()


if __name__ == '__main__':
    main()