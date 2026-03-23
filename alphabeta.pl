:- encoding(utf8).
% ============================================================
% alphabeta.pl — MinMax algorithm with Alpha-Beta pruning
%
% Implements best-move search for the computer player.
% Search depth is adaptive: 4 in the opening (MC < 6),
% 7 in the midgame and endgame.
%
% Evaluation scale:
%   +inf (9999)  — win for the root player
%   -inf (-9999) — loss for the root player
% ============================================================

:- module(alphabeta, [best_move/4, best_move/5]).

:- use_module(checkers).

% inf(-Val)
% Returns the infinity constant used for alpha-beta bounds.
%
% Meaningful modes:
%   inf(-) — get the infinity constant
% Non-meaningful: trivial existence; always succeeds.
inf(9999).

% adaptive_depth(++MoveCount, --Depth)
% Computes search depth based on the current game phase.
%
% Opening (first 6 moves): depth 4 for fast response.
% Midgame and endgame: depth 7 for stronger play.
% Mirrors Python constants SEARCH_DEPTH_OPENING=4, SEARCH_DEPTH_MAIN=7.
%
% Meaningful modes:
%   adaptive_depth(++, --) — compute depth from move count
%   adaptive_depth(++, +)  — verify expected depth
% Non-meaningful: arithmetic requires ground MoveCount.
adaptive_depth(MC, Depth) :-
    ( MC < 6 -> Depth = 4 ; Depth = 7 ).

/** <examples>
?- adaptive_depth(0, D).
D = 4.

?- adaptive_depth(10, D).
D = 7.
*/

% ============================================================
% best_move(++Board, ++Player, ++MoveCount, --BestMove)
% Finds the best move for Player on the given board using adaptive depth.
%
% MoveCount is the move counter (used for draw detection and depth selection).
%
% Meaningful modes:
%   best_move(++, ++, ++, --) — find the optimal computer move
%   best_move(++, ++, ++, _)  — existence check: fails when no legal moves (non-trivial)
% Non-meaningful: search requires a fully ground board.
% ============================================================
best_move(Board, Player, MoveCount, BestMove) :-
    adaptive_depth(MoveCount, Depth),
    best_move(Board, Player, MoveCount, Depth, BestMove).

% best_move(++Board, ++Player, ++MoveCount, ++Depth, --BestMove)
% Finds the best move for Player at a given explicit search depth.
%
% Generates all legal moves, then selects the best via alpha-beta search.
% Fails if no legal moves exist.
%
% Meaningful modes:
%   best_move(++, ++, ++, ++, --) — find the optimal move at explicit depth
%   best_move(++, ++, ++, ++, _)  — existence check: fails when no legal moves (non-trivial)
% Non-meaningful: search requires a fully ground board.
best_move(Board, Player, MoveCount, Depth, BestMove) :-
    all_legal_moves(Board, Player, Moves),
    Moves \= [],
    inf(Inf), NegInf is -Inf,
    ab_best(Moves, Board, Player, MoveCount, Depth,
            NegInf, Inf, _, BestMove).

/** <examples>
?- initial_board(B), best_move(B, black, 0, BM).
BM = move(3-2,4-3,[]).
*/

% ============================================================
% ab_best(++Moves, ++Board, ++Player, ++MC, ++Depth,
%         ++Alpha, ++Beta, --BestVal, --BestMove)
% Iterates over a list of moves, tracking the best one via alpha-beta pruning.
%
% Clause 1: single remaining move — evaluate without comparison.
% Clause 2: multiple moves — recurse with pruning; stop on beta cutoff.
%
% Meaningful modes:
%   ab_best(++, ++, ++, ++, ++, ++, ++, --, --) — select best move from list
% Non-meaningful: all inputs must be ground for alpha-beta search.
% ============================================================

% Clause 1: single move — no comparison needed
ab_best([M], Board, Player, MC, Depth, Alpha, Beta, Val, M) :- !,
    apply_move(Board, M, NB),
    MC1 is MC + 1,
    opponent(Player, Opp),
    ab_min(NB, Opp, Player, MC1, Depth, Alpha, Beta, Val).

% Clause 2: multiple moves — iterate with pruning
ab_best([M|Rest], Board, Player, MC, Depth, Alpha, Beta, BestVal, BestMove) :-
    apply_move(Board, M, NB),
    MC1 is MC + 1,
    opponent(Player, Opp),
    ab_min(NB, Opp, Player, MC1, Depth, Alpha, Beta, Val),
    (   Val > Alpha -> Alpha1 = Val ; Alpha1 = Alpha ),
    (   Alpha1 >= Beta
    ->  BestVal = Val, BestMove = M          % beta cutoff: this move caused it
    ;   ab_best(Rest, Board, Player, MC, Depth, Alpha1, Beta, RestVal, RestMove),
        (   Val > Alpha                       % did THIS move raise alpha?
        ->  (   RestVal > Val                 % did Rest find something even better?
            ->  BestVal = RestVal, BestMove = RestMove
            ;   BestVal = Val, BestMove = M
            )
        ;   BestVal = RestVal, BestMove = RestMove  % M was inferior, use Rest's result
        )
    ).

% ============================================================
% ab_max(++Board, ++Player, ++RootPlayer, ++MC, ++Depth,
%        ++Alpha, ++Beta, --Val)
% MAX node: the root player's turn; seeks the maximum evaluation.
%
% Terminal conditions (in order):
%   - Depth 0: return static evaluation.
%   - MC > 100: draw, return 0.
%   - Player has no pieces: loss/win depending on RootPlayer.
%   - No legal moves: same as no pieces.
% Otherwise: expand all moves and delegate to ab_max_list.
%
% Meaningful modes:
%   ab_max(++, ++, ++, ++, ++, ++, ++, --) — compute MAX-node value
% Non-meaningful: all inputs must be ground for alpha-beta search.
% ============================================================
ab_max(Board, Player, RootPlayer, MC, Depth, Alpha, Beta, Val) :-
    (   Depth =:= 0
    ->  evaluate(Board, RootPlayer, Val)
    ;   MC > 100
    ->  Val = 0
    ;   \+ has_piece(Board, Player)
    ->  opponent(Player, W),
        (W = RootPlayer -> inf(Val) ; inf(Inf), Val is -Inf)
    ;   all_legal_moves(Board, Player, Moves),
        (   Moves = []
        ->  opponent(Player, W),
            (W = RootPlayer -> inf(Val) ; inf(Inf), Val is -Inf)
        ;   opponent(Player, Opp),
            ab_max_list(Moves, Board, Player, Opp, RootPlayer,
                        MC, Depth, Alpha, Beta, Val)
        )
    ).

% ab_max_list(++Moves, ++Board, ++Player, ++Opp, ++Root,
%             ++MC, ++Depth, ++Alpha, ++Beta, --Val)
% Iterates moves at a MAX node, applying alpha-beta pruning.
%
% Base case: no moves left — return current Alpha as the node value.
% Recursive case: apply each move, call ab_min on the result, update Alpha,
% and prune (beta cutoff) when Alpha >= Beta.
%
% Meaningful modes:
%   ab_max_list(++, ++, ++, ++, ++, ++, ++, ++, ++, --) — iterate MAX-node moves
% Non-meaningful: all inputs must be ground for alpha-beta search.
ab_max_list([], _, _, _, _, _, _, Alpha, _, Alpha).
ab_max_list([M|Rest], Board, Player, Opp, Root, MC, Depth, Alpha, Beta, Val) :-
    apply_move(Board, M, NB),
    MC1 is MC + 1,
    Depth1 is Depth - 1,
    ab_min(NB, Opp, Root, MC1, Depth1, Alpha, Beta, V),
    (   V > Alpha -> Alpha1 = V ; Alpha1 = Alpha ),
    (   Alpha1 >= Beta
    ->  Val = Alpha1                            % beta cutoff
    ;   ab_max_list(Rest, Board, Player, Opp, Root, MC, Depth, Alpha1, Beta, Val)
    ).

% ============================================================
% ab_min(++Board, ++Player, ++RootPlayer, ++MC, ++Depth,
%        ++Alpha, ++Beta, --Val)
% MIN node: the opponent's turn; seeks the minimum evaluation.
%
% Terminal conditions (in order):
%   - Depth 0: return static evaluation.
%   - MC > 100: draw, return 0.
%   - Player has no pieces: loss/win depending on RootPlayer.
%   - No legal moves: same as no pieces.
% Otherwise: expand all moves and delegate to ab_min_list.
%
% Meaningful modes:
%   ab_min(++, ++, ++, ++, ++, ++, ++, --) — compute MIN-node value
% Non-meaningful: all inputs must be ground for alpha-beta search.
% ============================================================
ab_min(Board, Player, RootPlayer, MC, Depth, Alpha, Beta, Val) :-
    (   Depth =:= 0
    ->  evaluate(Board, RootPlayer, Val)
    ;   MC > 100
    ->  Val = 0
    ;   \+ has_piece(Board, Player)
    ->  opponent(Player, W),
        (W = RootPlayer -> inf(Val) ; inf(Inf), Val is -Inf)
    ;   all_legal_moves(Board, Player, Moves),
        (   Moves = []
        ->  opponent(Player, W),
            (W = RootPlayer -> inf(Val) ; inf(Inf), Val is -Inf)
        ;   opponent(Player, Opp),
            ab_min_list(Moves, Board, Player, Opp, RootPlayer,
                        MC, Depth, Alpha, Beta, Val)
        )
    ).

% ab_min_list(++Moves, ++Board, ++Player, ++Opp, ++Root,
%             ++MC, ++Depth, ++Alpha, ++Beta, --Val)
% Iterates moves at a MIN node, applying alpha-beta pruning.
%
% Base case: no moves left — return current Beta as the node value.
% Recursive case: apply each move, call ab_max on the result, update Beta,
% and prune (alpha cutoff) when Alpha >= Beta.
%
% Meaningful modes:
%   ab_min_list(++, ++, ++, ++, ++, ++, ++, ++, ++, --) — iterate MIN-node moves
% Non-meaningful: all inputs must be ground for alpha-beta search.
ab_min_list([], _, _, _, _, _, _, _, Beta, Beta).
ab_min_list([M|Rest], Board, Player, Opp, Root, MC, Depth, Alpha, Beta, Val) :-
    apply_move(Board, M, NB),
    MC1 is MC + 1,
    Depth1 is Depth - 1,
    ab_max(NB, Opp, Root, MC1, Depth1, Alpha, Beta, V),
    (   V < Beta -> Beta1 = V ; Beta1 = Beta ),
    (   Alpha >= Beta1
    ->  Val = Beta1                             % alpha cutoff
    ;   ab_min_list(Rest, Board, Player, Opp, Root, MC, Depth, Alpha, Beta1, Val)
    ).
