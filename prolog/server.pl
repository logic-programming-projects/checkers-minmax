:- encoding(utf8).
% ============================================================
% server.pl — HTTP server for the checkers game
%
% Launch: swipl server.pl
% The server opens on port 5500.
%
% Routes:
%   GET  /           -> static files (index.html, etc.)
%   POST /api/legal  -> list of legal moves for a player
%   POST /api/move   -> apply human move, get AI response
% ============================================================

:- use_module(library(http/thread_httpd)).   % multi-threaded HTTP server
:- use_module(library(http/http_dispatch)).  % URL routing
:- use_module(library(http/http_json)).      % JSON read/write
:- use_module(library(http/http_files)).     % static file serving
:- use_module(library(http/http_cors)).      % CORS headers (for development)

:- use_module(checkers).
:- use_module(alphabeta).

% ---- Route registration ----

% /api/legal — POST: list of legal moves
:- http_handler('/api/legal', handle_legal, [method(post)]).

% /api/move — POST: human move + AI response
:- http_handler('/api/move',  handle_move,  [method(post)]).

% / — static files from the current directory
:- http_handler('/', serve_static,          [prefix]).

% ---- Entry point ----

:- initialization(main, main).

% main/0
% Starts the HTTP server on port 5500 and blocks the main thread.
main :-
    Port = 5500,
    format("Starting checkers server on http://localhost:~w/~n", [Port]),
    http_server(http_dispatch, [port(Port)]),
    thread_get_message(_).          % keep the main thread alive

% ---- Static file handler ----

% serve_static(++Request)
% Serves static files from the directory where server.pl resides.
%
% Meaningful modes:
%   serve_static(++) — handle an incoming HTTP request
% Non-meaningful: HTTP request must be ground.
serve_static(Request) :-
    cors_enable,
    http_reply_from_files('.', [], Request).

% ---- /api/legal ----

% handle_legal(++Request)
% Reads JSON with fields: board (array of 64 strings), player (string).
% Returns JSON: { moves: [...] }.
%
% Meaningful modes:
%   handle_legal(++) — handle an incoming HTTP request
% Non-meaningful: HTTP request must be ground.
handle_legal(Request) :-
    cors_enable,
    http_read_json_dict(Request, Body, []),
    % Extract board and player from the request body
    BoardStrings = Body.board,
    PlayerStr    = Body.player,
    atom_string(Player, PlayerStr),
    strings_to_board(BoardStrings, Board),
    % Find all legal moves
    all_legal_moves(Board, Player, Moves),
    % Convert to JSON-compatible dicts
    maplist(move_to_dict, Moves, MoveDicts),
    reply_json_dict(_{moves: MoveDicts}).

% ---- /api/move ----

% handle_move(++Request)
% Reads JSON:
%   board      — array of 64 strings (current board)
%   player     — "black" or "white" (human player)
%   move_count — integer (number of moves played)
%   from_row, from_col, to_row, to_col — move coordinates
%
% Returns JSON:
%   board      — new board after both moves
%   ai_move    — AI move (or null)
%   game_over  — false or { winner: "..." }
%   move_count — updated counter
%
% Meaningful modes:
%   handle_move(++) — handle an incoming HTTP request
% Non-meaningful: HTTP request must be ground.
handle_move(Request) :-
    cors_enable,
    http_read_json_dict(Request, Body, []),

    % --- Unpack common request fields ---
    BoardStrings = Body.board,
    PlayerStr    = Body.player,
    MoveCount    = Body.move_count,
    FR = Body.from_row, FC = Body.from_col,
    TR = Body.to_row,   TC = Body.to_col,

    atom_string(Player, PlayerStr),
    opponent(Player, OtherPlayer),
    strings_to_board(BoardStrings, Board),

    % Extract optional fields
    (get_dict(difficulty, Body, DiffStr) -> atom_string(Difficulty, DiffStr) ; Difficulty = normal),
    (get_dict(game_mode, Body, ModeStr) -> atom_string(GameMode, ModeStr)   ; GameMode = 'human-vs-ai'),

    % --- Dispatch by game mode ---
    (   handle_mode(GameMode, Body, Board, Player, OtherPlayer, MoveCount,
                    FR, FC, TR, TC, Difficulty,
                    FinalBoard, AIMoveDict, GoResult, MC2)
    ->  board_to_list(FinalBoard, FinalList),
        maplist(atom_string, FinalList, FinalStrings),
        reply_json_dict(_{
            board:      FinalStrings,
            ai_move:    AIMoveDict,
            game_over:  GoResult,
            move_count: MC2
        })
    ;   reply_json_dict(_{error: "Illegal move"}, [status(400)])
    ).

% handle_mode(++GameMode, ++Body, ++Board, ++Player, ++OtherPlayer, ++MC,
%             ++FR, ++FC, ++TR, ++TC, ++Difficulty,
%             --FinalBoard, --AIMoveDict, --GoResult, --MC2)
% Dispatches move handling based on the game mode (ai-vs-ai, human-vs-human,
% or human-vs-ai). Applies the appropriate moves and checks for game-over.
%
% Meaningful modes:
%   handle_mode(++,++,++,++,++,++,++,++,++,++,++,--,--,--,--) — compute
% Non-meaningful: game state must be fully known.

% --- Mode: ai-vs-ai ---
% Player = whose turn it is. FR=-1 sentinel means compute AI move.
handle_mode('ai-vs-ai', _Body, Board, Player, OtherPlayer, MC,
            _FR, _FC, _TR, _TC, Difficulty,
            FinalBoard, AIMoveDict, GoResult, MC2) :-
    difficulty_depth(Difficulty, Board, MC, SearchDepth),
    (   best_move(Board, Player, MC, SearchDepth, AIMove)
    ->  apply_move(Board, AIMove, Board1),
        MC1 is MC + 1,
        move_to_dict(AIMove, AIMoveDict),
        FinalBoard = Board1,
        MC2 = MC1,
        check_game_over(Board1, OtherPlayer, MC1, GoResult)
    ;   % Player has no legal moves — they lose
        FinalBoard = Board, MC2 = MC, AIMoveDict = null,
        opponent(Player, Winner),
        GoResult = _{winner: Winner}
    ).

% --- Mode: human-vs-human ---
% Player = whose turn it is. Apply their move, no AI.
handle_mode('human-vs-human', Body, Board, Player, OtherPlayer, MC,
            FR, FC, TR, TC, _Difficulty,
            FinalBoard, AIMoveDict, GoResult, MC2) :-
    AIMoveDict = null,
    (   FR =:= -1
    ->  FinalBoard = Board, MC2 = MC, GoResult = false
    ;   all_legal_moves(Board, Player, LegalMoves),
        extract_caps(Body, ReqCaps),
        find_move(LegalMoves, FR, FC, TR, TC, ReqCaps, HumanMove),
        apply_move(Board, HumanMove, Board1),
        MC1 is MC + 1,
        FinalBoard = Board1,
        MC2 = MC1,
        check_game_over(Board1, OtherPlayer, MC1, GoResult)
    ).

% --- Mode: human-vs-ai (default) ---
% Player = human. Apply human move, then compute AI response.
handle_mode(_, Body, Board, Human, AI, MC,
            FR, FC, TR, TC, Difficulty,
            FinalBoard, AIMoveDict, GoResult, MC2) :-
    % Apply human move (FR=-1 = sentinel: AI plays first)
    (   FR =:= -1
    ->  Board1 = Board, MC1 = MC
    ;   all_legal_moves(Board, Human, LegalMoves),
        extract_caps(Body, ReqCaps),
        find_move(LegalMoves, FR, FC, TR, TC, ReqCaps, HumanMove),
        apply_move(Board, HumanMove, Board1),
        MC1 is MC + 1
    ),
    % Check game-over after human move
    (   game_over(Board1, AI, win(W1))
    ->  GoResult = _{winner: W1},
        FinalBoard = Board1, AIMoveDict = null, MC2 = MC1
    ;   MC1 >= 100
    ->  GoResult = _{winner: draw},
        FinalBoard = Board1, AIMoveDict = null, MC2 = MC1
    ;   % AI move
        difficulty_depth(Difficulty, Board1, MC1, SearchDepth),
        (   best_move(Board1, AI, MC1, SearchDepth, AIMove)
        ->  apply_move(Board1, AIMove, Board2),
            MC2 is MC1 + 1,
            move_to_dict(AIMove, AIMoveDict)
        ;   Board2 = Board1, MC2 = MC1, AIMoveDict = null
        ),
        FinalBoard = Board2,
        check_game_over(Board2, Human, MC2, GoResult)
    ).

% ---- Helpers ----

% extract_caps(++Body, -Caps)
% Extracts the capture list from the request body, or leaves Caps unbound.
%
% Meaningful modes:
%   extract_caps(++, -) — extract captures from dict
% Non-meaningful: dict must be ground. Always succeeds (trivial existence).
extract_caps(Body, Caps) :-
    (   get_dict(captures, Body, CapsJson)
    ->  maplist(cap_from_dict, CapsJson, Caps)
    ;   true  % Caps remains unbound
    ).

% check_game_over(++Board, ++NextPlayer, ++MC, --Result)
% Checks game-over after a move. NextPlayer is the one who moves next.
% Returns a winner dict, draw, or false.
%
% Meaningful modes:
%   check_game_over(++, ++, ++, --) — compute game-over result
% Non-meaningful: requires ground board. Always succeeds (trivial existence).
check_game_over(Board, NextPlayer, MC, Result) :-
    (   game_over(Board, NextPlayer, win(W))
    ->  Result = _{winner: W}
    ;   MC >= 100
    ->  Result = _{winner: draw}
    ;   Result = false
    ).

% difficulty_depth(++Difficulty, ++Board, ++MoveCount, --Depth)
% Maps a difficulty setting to an alpha-beta search depth.
% For normal difficulty, uses board-aware adaptive depth that reduces
% search depth in late endgame positions.
%
% Meaningful modes:
%   difficulty_depth(++, ++, ++, --) — compute search depth
% Non-meaningful: requires ground difficulty atom.
difficulty_depth(easy, _, _, 3).
difficulty_depth(hard, _, _, 8).
difficulty_depth(normal, Board, MC, D) :- alphabeta:adaptive_depth(Board, MC, D).

% find_move(++Moves, ++FR, ++FC, ++TR, ++TC, ?Caps, --Move)
% Finds a move in the list matching the given from/to coordinates and
% optional capture list. When Caps is unbound, matches any captures
% (backward-compatible).
%
% Meaningful modes:
%   find_move(++, ++, ++, ++, ++, ?, --) — find matching move
% Existence check:
%   find_move(++, ++, ++, ++, ++, ?, _)  — non-trivial, fails if no
%     matching move exists in the list.
find_move([M|_], FR, FC, TR, TC, Caps, M) :-
    M = move(FR-FC, TR-TC, Caps), !.
find_move([_|Rest], FR, FC, TR, TC, Caps, M) :-
    find_move(Rest, FR, FC, TR, TC, Caps, M).

% cap_from_dict(++Dict, --RC)
% Converts a JSON capture dict {row: R, col: C} to an R-C pair.
%
% Meaningful modes:
%   cap_from_dict(++, --) — extract row-col pair from dict
% Non-meaningful: dict must be ground.
cap_from_dict(Dict, R-C) :-
    R = Dict.row, C = Dict.col.
