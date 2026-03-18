:- encoding(utf8).
% ============================================================
% checkers.pl — Логіка гри в шашки (English Draughts)
%
% Представлення дошки:
%   Список з 64 атомів (індекс 1..64 для nth1)
%   Позиція (R, C) -> індекс = (R-1)*8 + C
%   Темні (ігрові) клітинки: (R+C) mod 2 =:= 1
%
% Значення клітинок: none | empty | black | white | black_king | white_king
%   none       - світла (неігрова) клітинка
%   empty      - порожня темна клітинка
%   black      - чорна шашка
%   white      - біла шашка
%   black_king - чорна дамка
%   white_king - біла дамка
% ============================================================

:- module(checkers, [
    initial_board/1,
    valid_pos/2,
    get_cell/4,
    set_cell/5,
    belongs_to/2,
    opponent/2,
    is_king/1,
    all_legal_moves/3,
    apply_move/4,
    game_over/3,
    evaluate/3,
    strings_to_board/2,
    move_to_dict/2
]).

% ---- Допоміжні предикати для дошки ----

% cell_idx(+R, +C, -I): обчислює 1-базовий індекс для nth1
% ++ ++, --
cell_idx(R, C, I) :- I is (R-1)*8 + C.

% valid_pos(?R, ?C): перевіряє/генерує ігрові (темні) позиції
% ??, ??
valid_pos(R, C) :-
    between(1, 8, R),
    between(1, 8, C),
    (R + C) mod 2 =:= 1.

% get_cell(+Board, +R, +C, -V): читає значення клітинки
% ++ ++ ++, --
get_cell(Board, R, C, V) :-
    cell_idx(R, C, I),
    nth1(I, Board, V).

% set_cell(+Board, +R, +C, +V, -NB): встановлює значення клітинки
% ++ ++ ++ ++, --
set_cell(Board, R, C, V, NB) :-
    cell_idx(R, C, I),
    setnth1(I, Board, V, NB).

% setnth1(+N, +List, +V, -NewList): замінює N-й елемент списку
% ++ ++ ++, --
setnth1(1, [_|T], V, [V|T]) :- !.
setnth1(N, [H|T], V, [H|R]) :-
    N > 1, N1 is N - 1,
    setnth1(N1, T, V, R).

% ---- Початковий стан дошки ----

% initial_board(-Board): початкова розстановка шашок
% --
% Чорні шашки: рядки 1-3, білі шашки: рядки 6-8
initial_board(Board) :-
    numlist(1, 64, Is),
    maplist(init_cell_val, Is, Board).

% init_cell_val(+I, -V): визначає початкове значення клітинки за індексом
% ++, --
init_cell_val(I, V) :-
    R is (I - 1) // 8 + 1,
    C is (I - 1) mod 8 + 1,
    (   (R + C) mod 2 =:= 0 -> V = none    % неігрова клітинка
    ;   R =< 3              -> V = black    % чорна шашка
    ;   R >= 6              -> V = white    % біла шашка
    ;                          V = empty    % порожня ігрова клітинка
    ).

% ---- Властивості фігур ----

% belongs_to(+Piece, -Player): визначає гравця для фігури
% ++, -
belongs_to(black,      black).
belongs_to(black_king, black).
belongs_to(white,      white).
belongs_to(white_king, white).

% opponent(+Player, -Opp): противник гравця
% ++, -
opponent(black, white).
opponent(white, black).

% is_king(+Piece): перевіряє, чи є фігура дамкою
% ++
is_king(black_king).
is_king(white_king).

% pawn_dr(+Player, -DR): напрямок руху пішака (зміна рядка)
% ++, --
pawn_dr(black,  1).   % чорні йдуть вниз (до рядка 8)
pawn_dr(white, -1).   % білі йдуть вгору (до рядка 1)

% promo_row(+Player, -Row): рядок перетворення на дамку
% ++, --
promo_row(black, 8).
promo_row(white, 1).

% piece_dirs(+Piece, -Dirs): список напрямків руху фігури
% ++, --
piece_dirs(P, Dirs) :-
    (   is_king(P)
    ->  Dirs = [1-1, 1-(-1), (-1)-1, (-1)-(-1)]   % дамка: всі 4 діагоналі
    ;   belongs_to(P, Pl),
        pawn_dr(Pl, DR),
        Dirs = [DR-1, DR-(-1)]                     % пішак: 2 діагоналі вперед
    ).

% maybe_promote(+Piece, +R, -NewPiece): перетворення пішака на дамку
% ++ ++, --
maybe_promote(P, R, NP) :-
    belongs_to(P, Pl),
    promo_row(Pl, PR),
    (   R =:= PR
    ->  (Pl = black -> NP = black_king ; NP = white_king)
    ;   NP = P
    ).

% ---- Генерація ходів ----

% all_legal_moves(+Board, +Player, -Moves)
% ++ ++, --
% Якщо є удари — лише удари (обов''язкове взяття).
% Інакше — прості ходи.
%
% Мультипризначеність:
%   (++, ++, --) — генерує всі легальні ходи
all_legal_moves(Board, Player, Moves) :-
    findall(M, capture_move(Board, Player, M), Caps),
    sort(Caps, UCaps),
    (   UCaps \= []
    ->  Moves = UCaps
    ;   findall(M, simple_move(Board, Player, M), Simps),
        sort(Simps, Moves)
    ).

% simple_move(+Board, +Player, -Move)
% ++ ++, --
% Простий хід без взяття: move(R1-C1, R2-C2, [])
simple_move(Board, Player, move(R1-C1, R2-C2, [])) :-
    valid_pos(R1, C1),
    get_cell(Board, R1, C1, P),
    belongs_to(P, Player),
    piece_dirs(P, Dirs),
    member(DR-DC, Dirs),
    R2 is R1 + DR, C2 is C1 + DC,
    valid_pos(R2, C2),
    get_cell(Board, R2, C2, empty).

% capture_move(+Board, +Player, -Move)
% ++ ++, --
% Повна послідовність взяття: move(R1-C1, RF-CF, CapturedList)
capture_move(Board, Player, move(R1-C1, RF-CF, Caps)) :-
    valid_pos(R1, C1),
    get_cell(Board, R1, C1, P),
    belongs_to(P, Player),
    jump_chain(Board, Player, P, R1, C1, [], Caps, RF, CF),
    Caps \= [].

% jump_chain(+Board, +Player, +Piece, +R, +C, +Used, -Caps, -FR, -FC)
% ++ ++ ++ ++ ++ ++, -- -- --
%
% Будує повний ланцюг взять — зупиняється лише коли далі бити неможливо.
%
% Клауза 1: робимо удар і продовжуємо
jump_chain(Board, Player, P, R, C, Used,
           [MR-MC | MoreCaps], RF, CF) :-
    piece_dirs(P, Dirs),
    member(DR-DC, Dirs),
    MR is R + DR, MC is C + DC,
    valid_pos(MR, MC),
    get_cell(Board, MR, MC, MP),
    MP \= empty, MP \= none,
    belongs_to(MP, Opp), opponent(Player, Opp),
    \+ member(MR-MC, Used),                     % не брати двічі одну фігуру
    R2 is R + 2*DR, C2 is C + 2*DC,
    valid_pos(R2, C2),
    get_cell(Board, R2, C2, empty),
    % Тимчасово застосовуємо удар для побудови подальших станів
    set_cell(Board, R,  C,  empty, B1),
    set_cell(B1,   MR, MC, empty, B2),
    maybe_promote(P, R2, NP),
    set_cell(B2,   R2, C2, NP,   B3),
    NewUsed = [MR-MC | Used],
    % Якщо фігура стала дамкою — зупиняємо (правило англійських шашок)
    (   P \= NP
    ->  RF = R2, CF = C2, MoreCaps = []
    ;   jump_chain(B3, Player, NP, R2, C2, NewUsed, MoreCaps, RF, CF)
    ).

% Клауза 2: базовий випадок — зупинитись, якщо бити більше не можна
jump_chain(Board, Player, P, R, C, Used, [], R, C) :-
    \+ can_jump(Board, Player, P, R, C, Used).

% can_jump(+Board, +Player, +Piece, +R, +C, +Used)
% Перевіряє, чи є можливий удар з поточної позиції
can_jump(Board, Player, P, R, C, Used) :-
    piece_dirs(P, Dirs),
    member(DR-DC, Dirs),
    MR is R + DR, MC is C + DC,
    valid_pos(MR, MC),
    get_cell(Board, MR, MC, MP),
    MP \= empty, MP \= none,
    belongs_to(MP, Opp), opponent(Player, Opp),
    \+ member(MR-MC, Used),
    R2 is R + 2*DR, C2 is C + 2*DC,
    valid_pos(R2, C2),
    get_cell(Board, R2, C2, empty).

% ---- Застосування ходу ----

% apply_move(+Board, +_Player, +Move, -NewBoard)
% ++ ++, --
apply_move(Board, _Player, move(R1-C1, RF-CF, Caps), NewBoard) :-
    get_cell(Board, R1, C1, P),
    set_cell(Board, R1, C1, empty, B1),
    remove_pieces(B1, Caps, B2),
    maybe_promote(P, RF, NP),
    set_cell(B2, RF, CF, NP, NewBoard).

% remove_pieces(+Board, +CapList, -NewBoard): видаляє взяті фігури
remove_pieces(B, [], B).
remove_pieces(B, [R-C | Rest], NB) :-
    set_cell(B, R, C, empty, B1),
    remove_pieces(B1, Rest, NB).

% ---- Кінець гри ----

% game_over(+Board, +Player, -Result)
% ++ ++, --
% Гра закінчена, якщо гравець не має фігур або ходів
game_over(Board, Player, win(Winner)) :-
    (   \+ has_piece(Board, Player)
    ;   all_legal_moves(Board, Player, [])
    ), !,
    opponent(Player, Winner).

% has_piece(+Board, +Player): чи є хоч одна фігура гравця
has_piece(Board, Player) :-
    valid_pos(R, C),
    get_cell(Board, R, C, P),
    belongs_to(P, Player), !.

% ---- Статична оцінка позиції ----

% evaluate(+Board, +Player, -Score)
% ++ ++, --
% Позитивне значення = краще для Player
% Шашка = 10 очок, дамка = 15 очок
evaluate(Board, Player, Score) :-
    opponent(Player, Opp),
    count_pieces(Board, Player, MyP, MyK),
    count_pieces(Board, Opp,    OppP, OppK),
    Score is (MyP - OppP) * 10 + (MyK - OppK) * 15.

% count_pieces(+Board, +Player, -Pawns, -Kings)
% ++ ++, -- --
count_pieces(Board, Player, Pawns, Kings) :-
    findall(1, (
        valid_pos(R, C), get_cell(Board, R, C, P),
        belongs_to(P, Player), \+ is_king(P)
    ), PL),
    length(PL, Pawns),
    findall(1, (
        valid_pos(R, C), get_cell(Board, R, C, P),
        belongs_to(P, Player), is_king(P)
    ), KL),
    length(KL, Kings).

% ---- Конвертація для JSON ----

% strings_to_board(+Strings, -Board): JSON рядки -> атоми Prolog
% ++, --
strings_to_board(Strings, Board) :-
    maplist(normalize_cell, Strings, Board).

% normalize_cell(+S, -A): рядок або атом -> атом
normalize_cell(S, A) :-
    (atom(S) -> A = S ; atom_string(A, S)).

% move_to_dict(+Move, -Dict): хід -> словник для JSON
% ++, --
move_to_dict(move(R1-C1, R2-C2, Caps), Dict) :-
    maplist(cap_to_dict, Caps, CapsJson),
    Dict = move{from_row:R1, from_col:C1,
                to_row:R2,   to_col:C2,
                captures:CapsJson}.

% cap_to_dict(+R-C, -Dict): позиція взяття -> словник
cap_to_dict(R-C, cap{row:R, col:C}).
