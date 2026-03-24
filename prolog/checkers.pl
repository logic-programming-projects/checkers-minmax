:- encoding(utf8).
% ============================================================
% checkers.pl — Логіка гри в шашки (English Draughts)
%
% Board representation:
%   A compound term b/64 (index 1..64 for arg/3).
%   Position (R, C) -> index = (R-1)*8 + C.
%   Dark (playable) squares: (R+C) mod 2 =:= 1.
%
% Cell values: none | empty | black | white | black_king | white_king
%   none       - light (non-playable) square
%   empty      - empty dark square
%   black      - black pawn
%   white      - white pawn
%   black_king - black king
%   white_king - white king
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
    apply_move/3,
    has_piece/2,
    game_over/3,
    evaluate/3,
    total_pieces/2,
    strings_to_board/2,
    board_to_list/2,
    move_to_dict/2
]).

% ---- Board helper predicates ----

% cell_idx(++R, ++C, -I)
% Computes the 1-based list index for a board position (R, C).
%
% Meaningful modes:
%   cell_idx(++, ++, --)  — compute index from row and column
%   cell_idx(++, ++, +)   — verify that a given index matches (R, C)
% Non-meaningful: arithmetic requires ground R and C.
cell_idx(R, C, I) :- I is (R-1)*8 + C.

% valid_pos(?R, ?C)
% Checks or generates playable (dark) board positions.
%
% Meaningful modes:
%   valid_pos(?, ?)  — generate all 32 dark squares
%   valid_pos(+, +)  — check if (R, C) is a dark square
%   valid_pos(+, +)  — existence check: non-trivial, fails for light squares
% Non-meaningful: all modes are meaningful.
valid_pos(R, C) :-
    between(1, 8, R),
    between(1, 8, C),
    (R + C) mod 2 =:= 1.

% get_cell(++Board, ++R, ++C, -V)
% Reads the value of a cell at position (R, C) on the board.
% Uses arg/3 for O(1) access into the compound term.
%
% Meaningful modes:
%   get_cell(++, ++, ++, --)  — read cell value
%   get_cell(++, ++, ++, +)   — verify cell value
% Non-meaningful: Board must be a ground compound.
get_cell(Board, R, C, V) :-
    cell_idx(R, C, I),
    arg(I, Board, V).

% set_cell(++Board, ++R, ++C, ++V, --NewBoard)
% Sets the cell at position (R, C) to value V, producing a new board.
% Copies the compound term and destructively updates the target cell.
%
% Meaningful modes:
%   set_cell(++, ++, ++, ++, --)  — produce new board with updated cell
% Non-meaningful: requires ground board and value.
set_cell(Board, R, C, V, NB) :-
    cell_idx(R, C, I),
    duplicate_term(Board, NB),
    nb_setarg(I, NB, V).

% ---- Initial board state ----

% initial_board(--Board)
% Generates the standard starting board for English Draughts.
%
% Meaningful modes:
%   initial_board(--)  — generate the initial board
% Non-meaningful: always succeeds (trivial existence).
%
% Black pawns occupy rows 1-3, white pawns occupy rows 6-8.
initial_board(Board) :-
    numlist(1, 64, Is),
    maplist(init_cell_val, Is, Cells),
    Board =.. [b | Cells].

% init_cell_val(++I, --V)
% Determines the initial cell value by linear index.
init_cell_val(I, V) :-
    R is (I - 1) // 8 + 1,
    C is (I - 1) mod 8 + 1,
    (   (R + C) mod 2 =:= 0 -> V = none    % non-playable square
    ;   R =< 3              -> V = black    % black pawn
    ;   R >= 6              -> V = white    % white pawn
    ;                          V = empty    % empty playable square
    ).

/** <examples>
?- valid_pos(1, 2).
true.

?- valid_pos(1, 1).
false.

?- initial_board(B), get_cell(B, 1, 2, V).
V = black.

?- initial_board(B), get_cell(B, 4, 1, V).
V = empty.
*/

% ---- Piece properties ----

% belongs_to(?Piece, ?Player)
% Determines which player owns a given piece.
%
% Meaningful modes:
%   belongs_to(+, -)   — look up the owner of a piece
%   belongs_to(-, +)   — enumerate pieces belonging to a player
%   belongs_to(+, +)   — verify piece-player association
%   belongs_to(+, _)   — existence check: non-trivial, fails for empty/none
% Non-meaningful: all modes are meaningful.
belongs_to(black,      black).
belongs_to(black_king, black).
belongs_to(white,      white).
belongs_to(white_king, white).

% opponent(?Player, ?Opp)
% Maps a player to their opponent.
%
% Meaningful modes:
%   opponent(+, -)  — look up the opponent
%   opponent(-, +)  — reverse look up
%   opponent(+, +)  — verify opponent relationship
% Non-meaningful: all modes are meaningful. Trivial existence for black/white.
opponent(black, white).
opponent(white, black).

% is_king(+Piece)
% Checks whether a piece is a king.
%
% Meaningful modes:
%   is_king(+)  — check if piece is a king
%   is_king(+)  — existence check: non-trivial, fails for regular pieces
% Non-meaningful: only (+) is meaningful.
is_king(black_king).
is_king(white_king).

% pawn_dr(++Player, --DR)
% Returns the row direction of movement for a player's pawns.
%
% Meaningful modes:
%   pawn_dr(++, --)  — get forward direction
%   pawn_dr(++, +)   — verify direction
% Non-meaningful: fixed facts only.
pawn_dr(black,  1).   % black moves down (towards row 8)
pawn_dr(white, -1).   % white moves up (towards row 1)

% promo_row(++Player, --Row)
% Returns the promotion row for a player's pawns.
%
% Meaningful modes:
%   promo_row(++, --)  — get promotion row
%   promo_row(++, +)   — verify promotion row
% Non-meaningful: fixed facts only.
promo_row(black, 8).
promo_row(white, 1).

% piece_dirs(++Piece, --Dirs)
% Returns the list of diagonal movement directions for a piece.
%
% Meaningful modes:
%   piece_dirs(++, --)  — get movement directions
% Non-meaningful: requires ground piece.
%
% Kings move in all 4 diagonals; pawns move in 2 forward diagonals.
piece_dirs(P, Dirs) :-
    (   is_king(P)
    ->  Dirs = [1-1, 1-(-1), (-1)-1, (-1)-(-1)]   % king: all 4 diagonals
    ;   belongs_to(P, Pl),
        pawn_dr(Pl, DR),
        Dirs = [DR-1, DR-(-1)]                     % pawn: 2 forward diagonals
    ).

% maybe_promote(++Piece, ++Row, --NewPiece)
% Promotes a pawn to a king if it reaches the promotion row.
%
% Meaningful modes:
%   maybe_promote(++, ++, --)  — compute (possibly promoted) piece
%   maybe_promote(++, ++, +)   — verify promotion result
% Non-meaningful: arithmetic requires ground terms.
maybe_promote(P, R, NP) :-
    belongs_to(P, Pl),
    promo_row(Pl, PR),
    (   R =:= PR
    ->  (Pl = black -> NP = black_king ; NP = white_king)
    ;   NP = P
    ).

/** <examples>
?- belongs_to(black, P).
P = black.

?- belongs_to(white_king, P).
P = white.

?- opponent(black, O).
O = white.
*/

% ---- Move generation ----

% all_legal_moves(++Board, ++Player, --Moves)
% Generates all legal moves for a player on a given board.
%
% Meaningful modes:
%   all_legal_moves(++, ++, --)  — generate all legal moves
% Non-meaningful: board and player must be ground.
%
% If captures exist, only captures are returned (mandatory capture rule).
% Otherwise, simple (non-capture) moves are returned.
all_legal_moves(Board, Player, Moves) :-
    findall(M, capture_move(Board, Player, M), Caps),
    sort(Caps, UCaps),
    (   UCaps \= []
    ->  Moves = UCaps
    ;   findall(M, simple_move(Board, Player, M), Simps),
        sort(Simps, Moves)
    ).

% simple_move(++Board, ++Player, --Move)
% Generates a simple (non-capture) move: move(R1-C1, R2-C2, []).
%
% Meaningful modes:
%   simple_move(++, ++, --)  — generate a simple move via backtracking
% Non-meaningful: board must be ground.
simple_move(Board, Player, move(R1-C1, R2-C2, [])) :-
    valid_pos(R1, C1),
    get_cell(Board, R1, C1, P),
    belongs_to(P, Player),
    piece_dirs(P, Dirs),
    member(DR-DC, Dirs),
    R2 is R1 + DR, C2 is C1 + DC,
    valid_pos(R2, C2),
    get_cell(Board, R2, C2, empty).

% capture_move(++Board, ++Player, --Move)
% Generates a full capture sequence: move(R1-C1, RF-CF, CapturedList).
%
% Meaningful modes:
%   capture_move(++, ++, --)  — generate a capture move via backtracking
% Non-meaningful: board must be ground.
capture_move(Board, Player, move(R1-C1, RF-CF, Caps)) :-
    valid_pos(R1, C1),
    get_cell(Board, R1, C1, P),
    belongs_to(P, Player),
    jump_chain(Board, Player, P, R1, C1, [], Caps, RF, CF),
    Caps \= [].

% jump_chain(++Board, ++Player, ++Piece, ++R, ++C, ++Used, --Caps, --FR, --FC)
% Builds a complete chain of captures, stopping when no further jumps are possible.
%
% Meaningful modes:
%   jump_chain(++, ++, ++, ++, ++, ++, --, --, --)  — build capture chain
% Non-meaningful: arithmetic requires all inputs ground.
%
% Clause 1: perform a jump and continue the chain.
jump_chain(Board, Player, P, R, C, Used,
           [MR-MC | MoreCaps], RF, CF) :-
    piece_dirs(P, Dirs),
    member(DR-DC, Dirs),
    MR is R + DR, MC is C + DC,
    valid_pos(MR, MC),
    get_cell(Board, MR, MC, MP),
    MP \= empty, MP \= none,
    belongs_to(MP, Opp), opponent(Player, Opp),
    \+ member(MR-MC, Used),                     % do not capture the same piece twice
    R2 is R + 2*DR, C2 is C + 2*DC,
    valid_pos(R2, C2),
    get_cell(Board, R2, C2, empty),
    % Temporarily apply the jump to build subsequent states
    set_cell(Board, R,  C,  empty, B1),
    set_cell(B1,   MR, MC, empty, B2),
    maybe_promote(P, R2, NP),
    set_cell(B2,   R2, C2, NP,   B3),
    NewUsed = [MR-MC | Used],
    % If piece was promoted, stop the chain (English Draughts rule)
    (   P \= NP
    ->  RF = R2, CF = C2, MoreCaps = []
    ;   jump_chain(B3, Player, NP, R2, C2, NewUsed, MoreCaps, RF, CF)
    ).

% Clause 2: base case — stop when no more jumps are possible.
jump_chain(Board, Player, P, R, C, Used, [], R, C) :-
    \+ can_jump(Board, Player, P, R, C, Used).

% can_jump(++Board, ++Player, ++Piece, ++R, ++C, ++Used)
% Checks whether a jump is possible from the current position.
%
% Meaningful modes:
%   can_jump(++, ++, ++, ++, ++, ++)  — check jump possibility
%   can_jump(++, ++, ++, ++, ++, ++)  — existence check: non-trivial, fails when no jump possible
% Non-meaningful: all inputs must be ground.
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

/** <examples>
?- initial_board(B), all_legal_moves(B, black, M), length(M, N).
N = 7.

?- initial_board(B), apply_move(B, move(3-2, 4-3, []), NB), get_cell(NB, 4, 3, V).
V = black.
*/

% ---- Move application ----

% apply_move(++Board, ++Move, --NewBoard)
% Applies a move to the board, producing a new board state.
%
% Meaningful modes:
%   apply_move(++, ++, --)  — apply move and produce new board
% Non-meaningful: requires ground board and move.
%
% Removes the piece from its origin, removes all captured pieces,
% promotes if applicable, and places the piece at its destination.
apply_move(Board, move(R1-C1, RF-CF, Caps), NewBoard) :-
    get_cell(Board, R1, C1, P),
    set_cell(Board, R1, C1, empty, B1),
    remove_pieces(B1, Caps, B2),
    maybe_promote(P, RF, NP),
    set_cell(B2, RF, CF, NP, NewBoard).

% remove_pieces(++Board, ++CapList, --NewBoard)
% Removes captured pieces from the board by setting their cells to empty.
%
% Meaningful modes:
%   remove_pieces(++, ++, --)  — remove all captured pieces
% Non-meaningful: requires ground board and capture list.
remove_pieces(B, [], B).
remove_pieces(B, [R-C | Rest], NB) :-
    set_cell(B, R, C, empty, B1),
    remove_pieces(B1, Rest, NB).

/** <examples>
?- initial_board(B), apply_move(B, move(3-2, 4-3, []), NB), get_cell(NB, 4, 3, V).
V = black.
*/

% ---- Game termination ----

% game_over(++Board, ++Player, -Result)
% Determines whether the game is over for the given player.
%
% Meaningful modes:
%   game_over(++, ++, --)  — detect game-over and get result
%   game_over(++, ++, +)   — verify a specific game-over result
%   game_over(++, ++, _)   — existence check: non-trivial, fails if game continues
% Non-meaningful: board and player must be ground.
%
% The game ends when the player has no pieces or no legal moves.
% Result is win(Winner) where Winner is the opponent.
game_over(Board, Player, win(Winner)) :-
    (   \+ has_piece(Board, Player)
    ;   all_legal_moves(Board, Player, [])
    ), !,
    opponent(Player, Winner).

% has_piece(++Board, +Player)
% Checks whether the player has at least one piece on the board.
%
% Meaningful modes:
%   has_piece(++, +)  — check if player has pieces
%   has_piece(++, +)  — existence check: non-trivial, fails if no pieces
% Non-meaningful: board must be ground.
has_piece(Board, Player) :-
    valid_pos(R, C),
    get_cell(Board, R, C, P),
    belongs_to(P, Player), !.

/** <examples>
?- initial_board(B), game_over(B, black, R).
false.
*/

% ---- Static position evaluation ----

% evaluate(++Board, ++Player, -Score)
% Computes a static evaluation score for the given player's position.
%
% Meaningful modes:
%   evaluate(++, ++, --)  — compute numeric evaluation score
%   evaluate(++, ++, +)   — verify a specific score
% Non-meaningful: arithmetic requires ground board and player.
%
% Positive score favours the given player.
% Components (static only — no move generation for speed):
%   Material:    pawn = 100 points, king = 300 points
%   Centre:      +10 for columns 3-6 (centre control)
%   Advancement: +5 per row forward (encourages advance)
evaluate(Board, Player, Score) :-
    opponent(Player, Opp),
    material_score(Board, Player, MyScore),
    material_score(Board, Opp,    OppScore),
    Score is MyScore - OppScore.

% material_score(++Board, ++Player, --Score)
% Computes the total material and positional score for a player.
%
% Meaningful modes:
%   material_score(++, ++, --)  — compute total score
% Non-meaningful: requires ground board and player.
material_score(Board, Player, Score) :-
    findall(S, piece_value(Board, Player, S), Vals),
    sum_list(Vals, Score).

% piece_value(++Board, ++Player, --Score)
% Evaluates a single piece: material + centre bonus + advancement bonus.
%
% Meaningful modes:
%   piece_value(++, ++, --)  — compute score for one piece via backtracking
% Non-meaningful: requires ground board and player.
piece_value(Board, Player, Score) :-
    valid_pos(R, C),
    get_cell(Board, R, C, P),
    belongs_to(P, Player),
    % Material value
    ( is_king(P) -> MatVal = 300 ; MatVal = 100 ),
    % Centre column bonus (columns 3-6)
    ( (C >= 3, C =< 6) -> CentBonus = 10 ; CentBonus = 0 ),
    % Advancement bonus (pawns only)
    (   is_king(P)
    ->  AdvBonus = 0
    ;   pawn_dr(Player, DR),
        ( DR =:= 1
        ->  AdvBonus is (R - 1) * 5   % black: farther from row 1 = better
        ;   AdvBonus is (8 - R) * 5   % white: farther from row 8 = better
        )
    ),
    Score is MatVal + CentBonus + AdvBonus.

% total_pieces(++Board, --N)
% Counts the total number of pieces (both players) on the board.
%
% Meaningful modes:
%   total_pieces(++, --) — count all pieces
% Non-meaningful: requires ground board.
total_pieces(Board, N) :-
    findall(1, (valid_pos(R,C), get_cell(Board,R,C,P), belongs_to(P,_)), Ps),
    length(Ps, N).

/** <examples>
?- initial_board(B), evaluate(B, black, S).
S = 0.
*/

% ---- JSON conversion ----

% strings_to_board(++Strings, --Board)
% Converts a list of JSON strings (or atoms) to a board compound term.
%
% Meaningful modes:
%   strings_to_board(++, --)  — convert string list to board compound
% Non-meaningful: requires ground string list.
strings_to_board(Strings, Board) :-
    maplist(normalize_cell, Strings, Cells),
    Board =.. [b | Cells].

% board_to_list(++Board, --List)
% Converts a board compound term back to a flat list of atoms.
%
% Meaningful modes:
%   board_to_list(++, --)  — convert board compound to list
% Non-meaningful: requires ground board.
board_to_list(Board, List) :-
    Board =.. [b | List].

% normalize_cell(+S, -A)
% Normalises a string or atom to an atom.
%
% Meaningful modes:
%   normalize_cell(+, -)  — convert to atom
% Non-meaningful: input must be ground.
normalize_cell(S, A) :-
    (atom(S) -> A = S ; atom_string(A, S)).

% move_to_dict(++Move, --Dict)
% Converts a move term to a dictionary suitable for JSON serialisation.
%
% Meaningful modes:
%   move_to_dict(++, --)  — convert move to dict
% Non-meaningful: requires ground move.
move_to_dict(move(R1-C1, R2-C2, Caps), Dict) :-
    maplist(cap_to_dict, Caps, CapsJson),
    Dict = move{from_row:R1, from_col:C1,
                to_row:R2,   to_col:C2,
                captures:CapsJson}.

% cap_to_dict(++RC, --Dict)
% Converts a capture position R-C to a dictionary.
%
% Meaningful modes:
%   cap_to_dict(++, --)  — convert capture to dict
% Non-meaningful: requires ground position.
cap_to_dict(R-C, cap{row:R, col:C}).

/** <examples>
?- strings_to_board(["black", "none"], B).
B = b(black,none).

?- move_to_dict(move(3-2, 5-4, [4-3]), D).
D = move{captures:[cap{col:3,row:4}],from_col:2,from_row:3,to_col:4,to_row:5}.
*/

% ---- Tests ----

test :-
    format("=== checkers.pl tests ===~n"),
    initial_board(B),
    get_cell(B, 1, 2, black),
    format("get_cell(1,2)=black: OK~n"),
    all_legal_moves(B, black, Moves), length(Moves, 7),
    format("Initial black moves=7: OK~n"),
    apply_move(B, move(3-2, 4-3, []), NB),
    get_cell(NB, 3, 2, empty), get_cell(NB, 4, 3, black),
    format("apply_move simple: OK~n"),
    evaluate(B, black, 0),
    format("Initial eval=0: OK~n"),
    format("All tests passed.~n").
