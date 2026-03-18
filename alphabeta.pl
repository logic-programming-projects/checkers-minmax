:- encoding(utf8).
% ============================================================
% alphabeta.pl — Алгоритм MinMax з Alpha-Beta відсіканням
%
% Реалізує пошук найкращого ходу для Компʼютер-гравця.
% Глибина пошуку: 5 рівнів.
%
% Умовні позначення оцінок:
%   +inf (9999)  — виграш поточного гравця
%   -inf (-9999) — поразка поточного гравця
% ============================================================

:- module(alphabeta, [best_move/4]).

:- use_module(checkers).

% Константи глибини та нескінченності
search_depth(5).
inf(9999).

% ============================================================
% best_move(+Board, +Player, +MoveCount, -BestMove)
% ++ ++ ++, --
%
% Знаходить найкращий хід для Player на поточній дошці.
% MoveCount — лічильник ходів (для визначення нічиєї).
%
% Мультипризначеність:
%   (++, ++, ++, --) — знаходить оптимальний хід Компʼютера
% ============================================================
best_move(Board, Player, MoveCount, BestMove) :-
    all_legal_moves(Board, Player, Moves),
    Moves \= [],
    search_depth(Depth),
    inf(Inf), NegInf is -Inf,
    ab_best(Moves, Board, Player, MoveCount, Depth,
            NegInf, Inf, _, BestMove).

% ============================================================
% ab_best(+Moves, +Board, +Player, +MC, +Depth, +Alpha, +Beta,
%         -BestVal, -BestMove)
% ++ ++ ++ ++ ++ ++ ++, -- --
%
% Перебирає список ходів, відстежуючи найкращий.
% Реалізує відсікання: якщо Alpha >= Beta — припиняємо.
%
% Клауза 1: єдиний хід — немає з чим порівнювати
ab_best([M], Board, Player, MC, Depth, Alpha, Beta, Val, M) :- !,
    apply_move(Board, Player, M, NB),
    MC1 is MC + 1,
    opponent(Player, Opp),
    ab_min(NB, Opp, Player, MC1, Depth, Alpha, Beta, Val).

% Клауза 2: кілька ходів — перебір з відсіканням
ab_best([M|Rest], Board, Player, MC, Depth, Alpha, Beta, BestVal, BestMove) :-
    apply_move(Board, Player, M, NB),
    MC1 is MC + 1,
    opponent(Player, Opp),
    ab_min(NB, Opp, Player, MC1, Depth, Alpha, Beta, Val),
    (   Val > Alpha
    ->  Alpha1 = Val, BM = M         % новий найкращий хід
    ;   Alpha1 = Alpha, BM = M       % залишаємо старий (для першого)
    ),
    (   Alpha1 >= Beta
    ->  BestVal = Alpha1, BestMove = BM    % бета-відсікання
    ;   ab_best(Rest, Board, Player, MC, Depth, Alpha1, Beta, RestVal, RestMove),
        (   RestVal >= Alpha1
        ->  BestVal = RestVal, BestMove = RestMove
        ;   BestVal = Alpha1, BestMove = BM
        )
    ).

% ============================================================
% ab_max(+Board, +MaxPlayer, +RootPlayer, +MC, +Depth,
%        +Alpha, +Beta, -Val)
% ++ ++ ++ ++ ++ ++ ++, --
%
% MAX-вузол: хід RootPlayer, шукаємо максимум.
%
% Мультипризначеність:
%   (++,++,++,++,++,++,++,--) — обчислює оцінку MAX-вузла
% ============================================================
ab_max(Board, Player, RootPlayer, MC, Depth, Alpha, Beta, Val) :-
    % Базовий випадок: кінцевий вузол
    (   Depth =:= 0
    ->  evaluate(Board, RootPlayer, Val)
    ;   MC > 100                                % нічия після 100 ходів
    ->  Val = 0
    ;   game_over(Board, Player, win(W))
    ->  (   W = RootPlayer
        ->  inf(Val)                            % перемога
        ;   inf(Inf), Val is -Inf               % поразка
        )
    ;   % Рекурсивний випадок: отримуємо ходи і вибираємо гілку
        all_legal_moves(Board, Player, Moves),
        opponent(Player, Opp),
        (   Moves = []
        ->  evaluate(Board, RootPlayer, Val)    % немає ходів — оцінка
        ;   ab_max_list(Moves, Board, Player, Opp, RootPlayer,
                        MC, Depth, Alpha, Beta, Val)
        )
    ).

% ab_max_list: перебір ходів у MAX-вузлі
% ++ ++ ++ ++ ++ ++ ++ ++ ++, --
ab_max_list([], _, _, _, _, _, _, Alpha, _, Alpha).
ab_max_list([M|Rest], Board, Player, Opp, Root, MC, Depth, Alpha, Beta, Val) :-
    apply_move(Board, Player, M, NB),
    MC1 is MC + 1,
    Depth1 is Depth - 1,
    ab_min(NB, Opp, Root, MC1, Depth1, Alpha, Beta, V),
    (   V > Alpha -> Alpha1 = V ; Alpha1 = Alpha ),
    (   Alpha1 >= Beta
    ->  Val = Alpha1                            % бета-відсікання
    ;   ab_max_list(Rest, Board, Player, Opp, Root, MC, Depth, Alpha1, Beta, Val)
    ).

% ============================================================
% ab_min(+Board, +MinPlayer, +RootPlayer, +MC, +Depth,
%        +Alpha, +Beta, -Val)
% ++ ++ ++ ++ ++ ++ ++, --
%
% MIN-вузол: хід опонента, шукаємо мінімум.
%
% Мультипризначеність:
%   (++,++,++,++,++,++,++,--) — обчислює оцінку MIN-вузла
% ============================================================
ab_min(Board, Player, RootPlayer, MC, Depth, Alpha, Beta, Val) :-
    (   Depth =:= 0
    ->  evaluate(Board, RootPlayer, Val)
    ;   MC > 100
    ->  Val = 0
    ;   game_over(Board, Player, win(W))
    ->  (   W = RootPlayer
        ->  inf(Val)
        ;   inf(Inf), Val is -Inf
        )
    ;   all_legal_moves(Board, Player, Moves),
        opponent(Player, Opp),
        (   Moves = []
        ->  evaluate(Board, RootPlayer, Val)    % немає ходів — оцінка
        ;   ab_min_list(Moves, Board, Player, Opp, RootPlayer,
                        MC, Depth, Alpha, Beta, Val)
        )
    ).

% ab_min_list: перебір ходів у MIN-вузлі
% ++ ++ ++ ++ ++ ++ ++ ++ ++, --
ab_min_list([], _, _, _, _, _, _, _, Beta, Beta).
ab_min_list([M|Rest], Board, Player, Opp, Root, MC, Depth, Alpha, Beta, Val) :-
    apply_move(Board, Player, M, NB),
    MC1 is MC + 1,
    Depth1 is Depth - 1,
    ab_max(NB, Opp, Root, MC1, Depth1, Alpha, Beta, V),
    (   V < Beta -> Beta1 = V ; Beta1 = Beta ),
    (   Alpha >= Beta1
    ->  Val = Beta1                             % альфа-відсікання
    ;   ab_min_list(Rest, Board, Player, Opp, Root, MC, Depth, Alpha, Beta1, Val)
    ).
