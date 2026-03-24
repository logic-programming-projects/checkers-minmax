:- encoding(utf8).
% ============================================================
% alphabeta.pl — MinMax algorithm with Alpha-Beta pruning
%
% Implements best-move search for the computer player.
% Search depth is adaptive: 5 half-moves in the opening (MC < 6),
% 8 half-moves in the midgame and endgame.
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
% Uses move count as a heuristic (no board access).
%
% Opening (first 6 moves): depth 5 half-moves for fast response.
% Midgame and endgame: depth 8 half-moves for stronger play.
%
% Meaningful modes:
%   adaptive_depth(++, --) — compute depth from move count
%   adaptive_depth(++, +)  — verify expected depth
% Non-meaningful: arithmetic requires ground MoveCount.
adaptive_depth(MC, Depth) :-
    ( MC < 6 -> Depth = 5 ; Depth = 8 ).

% adaptive_depth(++Board, ++MoveCount, --Depth)
% Board-aware variant: reduces depth in late endgame (<=6 pieces)
% to avoid multi-second searches on sparse king-heavy boards.
%
% Meaningful modes:
%   adaptive_depth(++, ++, --) — compute depth from board and move count
% Non-meaningful: requires ground board and move count.
adaptive_depth(Board, MC, Depth) :-
    (   MC < 6 -> Depth = 5
    ;   total_pieces(Board, N), N =< 6 -> Depth = 6
    ;   Depth = 8
    ).

/** <examples>
?- adaptive_depth(0, D).
D = 5.

?- adaptive_depth(10, D).
D = 8.
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
% Generates all legal moves, orders them by static evaluation for better
% alpha-beta pruning, then selects the best via search.
% Fails if no legal moves exist.
%
% Meaningful modes:
%   best_move(++, ++, ++, ++, --) — find the optimal move at explicit depth
%   best_move(++, ++, ++, ++, _)  — existence check: fails when no legal moves (non-trivial)
% Non-meaningful: search requires a fully ground board.
best_move(Board, Player, MoveCount, Depth, BestMove) :-
    all_legal_moves(Board, Player, Moves),
    Moves \= [],
    order_moves(Board, Player, desc, Moves, Ordered),
    inf(Inf), NegInf is -Inf,
    ab_best(Ordered, Board, Player, MoveCount, Depth,
            NegInf, Inf, _, BestMove).

/** <examples>
?- initial_board(B), best_move(B, black, 0, BM).
BM = move(3-2,4-3,[]).
*/

% ============================================================
% Move ordering — improves alpha-beta pruning by searching likely-best
% moves first. Sorts by static evaluation of the resulting board.
% ============================================================

% maybe_order(++Board, ++Root, ++Dir, ++Moves, ++Depth, --Ordered)
% Applies move ordering only at the top levels of the search tree (Depth >= 4)
% where the pruning benefit outweighs the evaluation overhead.
% At deeper levels, moves are left in their original order.
%
% Meaningful modes:
%   maybe_order(++, ++, ++, ++, ++, --) — conditionally sort moves
% Non-meaningful: all inputs must be ground.
maybe_order(Board, Root, Dir, Moves, Depth, Ordered) :-
    (   Depth >= 4
    ->  order_moves(Board, Root, Dir, Moves, Ordered)
    ;   Ordered = Moves
    ).

% order_moves(++Board, ++Root, ++Dir, ++Moves, --Sorted)
% Sorts Moves by static evaluation. Dir = desc for MAX nodes, asc for MIN.
%
% Meaningful modes:
%   order_moves(++, ++, ++, ++, --) — sort moves by heuristic
% Non-meaningful: all inputs must be ground.
order_moves(Board, Root, Dir, Moves, Sorted) :-
    maplist(score_move(Board, Root), Moves, Scored),
    (Dir = desc -> sort(1, @>=, Scored, Pairs) ; sort(1, @=<, Scored, Pairs)),
    pairs_values(Pairs, Sorted).

% score_move(++Board, ++Root, ++Move, --Score-Move)
% Evaluates a move by applying it and computing the static score.
score_move(Board, Root, Move, Score-Move) :-
    apply_move(Board, Move, NB),
    evaluate(NB, Root, Score).

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
    Depth1 is Depth - 1,
    opponent(Player, Opp),
    ab_min(NB, Opp, Player, MC1, Depth1, Alpha, Beta, Val).

% Clause 2: multiple moves — iterate with pruning
ab_best([M|Rest], Board, Player, MC, Depth, Alpha, Beta, BestVal, BestMove) :-
    apply_move(Board, M, NB),
    MC1 is MC + 1,
    Depth1 is Depth - 1,
    opponent(Player, Opp),
    ab_min(NB, Opp, Player, MC1, Depth1, Alpha, Beta, Val),
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
    ;   MC >= 100
    ->  Val = 0
    ;   \+ has_piece(Board, Player)
    ->  opponent(Player, W),
        (W = RootPlayer -> inf(Val) ; inf(Inf), Val is -Inf)
    ;   all_legal_moves(Board, Player, Moves),
        (   Moves = []
        ->  opponent(Player, W),
            (W = RootPlayer -> inf(Val) ; inf(Inf), Val is -Inf)
        ;   opponent(Player, Opp),
            maybe_order(Board, RootPlayer, desc, Moves, Depth, Ordered),
            ab_max_list(Ordered, Board, Player, Opp, RootPlayer,
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
    ;   MC >= 100
    ->  Val = 0
    ;   \+ has_piece(Board, Player)
    ->  opponent(Player, W),
        (W = RootPlayer -> inf(Val) ; inf(Inf), Val is -Inf)
    ;   all_legal_moves(Board, Player, Moves),
        (   Moves = []
        ->  opponent(Player, W),
            (W = RootPlayer -> inf(Val) ; inf(Inf), Val is -Inf)
        ;   opponent(Player, Opp),
            maybe_order(Board, RootPlayer, asc, Moves, Depth, Ordered),
            ab_min_list(Ordered, Board, Player, Opp, RootPlayer,
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
