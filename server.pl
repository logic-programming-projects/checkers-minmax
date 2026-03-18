:- encoding(utf8).
% ============================================================
% server.pl — HTTP-сервер для гри в шашки
%
% Запуск: swipl server.pl
% Сервер відкривається на порту 5500.
%
% Маршрути:
%   GET  /        -> статичні файли (index.html, тощо)
%   POST /api/legal  -> список легальних ходів для гравця
%   POST /api/move   -> застосувати хід людини, отримати хід Компʼютера
% ============================================================

:- use_module(library(http/thread_httpd)).   % багатопотоковий HTTP-сервер
:- use_module(library(http/http_dispatch)).  % маршрутизація URL
:- use_module(library(http/http_json)).      % читання/запис JSON
:- use_module(library(http/http_files)).     % видача статичних файлів
:- use_module(library(http/http_cors)).      % CORS-заголовки (для розробки)

:- use_module(checkers).
:- use_module(alphabeta).

% ---- Реєстрація маршрутів ----

% /api/legal — POST: список легальних ходів
:- http_handler('/api/legal', handle_legal, [method(post)]).

% /api/move — POST: хід людини + відповідь Компʼютера
:- http_handler('/api/move',  handle_move,  [method(post)]).

% / — статичні файли з поточної директорії
:- http_handler('/', serve_static,          [prefix]).

% ---- Точка входу ----

:- initialization(main, main).

% main: запускає HTTP-сервер на порту 5500
main :-
    Port = 5500,
    format("Starting checkers server on http://localhost:~w/~n", [Port]),
    http_server(http_dispatch, [port(Port)]),
    thread_get_message(_).          % утримуємо головний потік живим

% ---- Обробник статичних файлів ----

% serve_static(+Request)
% Віддає файли з директорії, де лежить server.pl
serve_static(Request) :-
    cors_enable,
    http_reply_from_files('.', [], Request).

% ---- /api/legal ----

% handle_legal(+Request)
% ++ (request from HTTP)
% Читає JSON з полями: board (масив 64 рядків), player (рядок)
% Повертає JSON: { moves: [...] }
handle_legal(Request) :-
    cors_enable,
    http_read_json_dict(Request, Body, []),
    % Отримуємо дошку та гравця з тіла запиту
    BoardStrings = Body.board,
    PlayerStr    = Body.player,
    atom_string(Player, PlayerStr),
    strings_to_board(BoardStrings, Board),
    % Знаходимо всі легальні ходи
    all_legal_moves(Board, Player, Moves),
    % Конвертуємо у JSON-сумісні словники
    maplist(move_to_dict, Moves, MoveDicts),
    reply_json_dict(_{moves: MoveDicts}).

% ---- /api/move ----

% handle_move(+Request)
% ++ (request from HTTP)
% Читає JSON:
%   board      — масив 64 рядків (поточна дошка)
%   player     — "black" або "white" (гравець-людина)
%   move_count — ціле число (кількість зіграних ходів)
%   from_row, from_col, to_row, to_col — координати ходу людини
%
% Повертає JSON:
%   board      — нова дошка після обох ходів
%   ai_move    — хід AI (або null)
%   game_over  — false або { winner: "..." }
%   move_count — оновлений лічильник
handle_move(Request) :-
    cors_enable,
    http_read_json_dict(Request, Body, []),

    % --- Розпаковуємо запит ---
    BoardStrings = Body.board,
    PlayerStr    = Body.player,
    MoveCount    = Body.move_count,
    FR = Body.from_row, FC = Body.from_col,
    TR = Body.to_row,   TC = Body.to_col,

    atom_string(Human, PlayerStr),
    opponent(Human, AI),
    strings_to_board(BoardStrings, Board),

    % --- Знаходимо і застосовуємо хід людини ---
    % FR = -1 є sentinel: людина не ходить (Компʼютер ходить першим)
    (   FR =:= -1
    ->  Board1 = Board, MC1 = MoveCount
    ;   all_legal_moves(Board, Human, LegalMoves),
        (   find_move(LegalMoves, FR, FC, TR, TC, HumanMove)
        ->  apply_move(Board, Human, HumanMove, Board1),
            MC1 is MoveCount + 1
        ;   Board1 = Board, MC1 = MoveCount
        )
    ),

    % --- Перевіряємо стан після ходу людини ---
    (   game_over(Board1, AI, win(W1))
    ->  GoResult = _{winner: W1},
        FinalBoard = Board1,
        AIMoveDict = null,
        MC2 = MC1
    ;   MC1 > 100
    ->  GoResult = _{winner: draw},
        FinalBoard = Board1,
        AIMoveDict = null,
        MC2 = MC1
    ;   % --- Хід Компʼютера ---
        (   best_move(Board1, AI, MC1, AIMove)
        ->  apply_move(Board1, AI, AIMove, Board2),
            MC2 is MC1 + 1,
            move_to_dict(AIMove, AIMoveDict)
        ;   % Компʼютер не має ходів
            Board2 = Board1, MC2 = MC1, AIMoveDict = null
        ),
        % --- Перевіряємо стан після ходу Компʼютера ---
        (   game_over(Board2, Human, win(W2))
        ->  GoResult = _{winner: W2}
        ;   MC2 > 100
        ->  GoResult = _{winner: draw}
        ;   GoResult = false
        ),
        FinalBoard = Board2
    ),

    % --- Конвертуємо дошку у рядки для JSON ---
    maplist(atom_string, FinalBoard, FinalStrings),

    reply_json_dict(_{
        board:      FinalStrings,
        ai_move:    AIMoveDict,
        game_over:  GoResult,
        move_count: MC2
    }).

% ---- Допоміжні ----

% find_move(+Moves, +FR, +FC, +TR, +TC, -Move)
% ++ ++ ++ ++ ++, --
% Знаходить хід у списку за початковою та кінцевою позицією
find_move([M|_], FR, FC, TR, TC, M) :-
    M = move(FR-FC, TR-TC, _), !.
find_move([_|Rest], FR, FC, TR, TC, M) :-
    find_move(Rest, FR, FC, TR, TC, M).
