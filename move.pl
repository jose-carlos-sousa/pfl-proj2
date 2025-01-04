
/*

remove_blocked_stones(+Variant, +Board, -NewBoard)
Given the game variant checks if stones are blocked and if so removes the blocked stone and depending on Variant removes:
Medium -> Adjacent Black Stones
High -> All Black Stones

*/
remove_blocked_stones(Variant,[H|T], NewGameState):-
    length(H, NumCols),
    length([H|T], NumRows),
    remove_blocked_stones_helper(Variant,1-1, NumCols-NumRows, [H|T],[H|T], NewGameState) , !.

% has original board board accumulator and final 
remove_blocked_stones_helper(Variant,C-L, C-L, GameState,GameStateAcc, NewGameState):- 
    remove_blocked_stones_piece(Variant,GameState,GameStateAcc, C-L, NewGameState).

remove_blocked_stones_helper(Variant,C-L, C2-L2, GameState,GameStateAcc, NextGameState):-
    remove_blocked_stones_piece(Variant,GameState,GameStateAcc, C-L, NewGameState),
    next_position(C-L, C2-L2, NextC-NextL),
    remove_blocked_stones_helper(Variant,NextC-NextL, C2-L2,GameState ,NewGameState, NextGameState).



% if piece is empty nothing should be done
remove_blocked_stones_piece(_,GameState,GameAcc,C-L, GameAcc):-
    get_piece(GameState, C-L, empty), !.

% if piece is black nothing should be done
remove_blocked_stones_piece(_,GameState,GameAcc,C-L, GameAcc):-
    get_piece(GameState, C-L, black), !.

/*

remove_blocked_stones_piece(+Variant, +Board, +GameAcc, +C-L, -NewGameState )

Get Piece
Get adjacents
if there is no empty stone in adjacent the piece is blocked

For Normal: set the piece to empty
For Medium Churn: set the piece to empty && Remove all black pieces adjacents
For High Churn:  set the piece to empty && Remove all black pieces in the board

*/

remove_blocked_stones_piece(1, GameState,GameAcc,C-L, NewGameState ):-
    get_piece(GameState, C-L, Piece),
    adjacent_stones(GameState, C-L, Stones),
    \+ member(empty, Stones),
    set_piece(GameAcc, C-L, empty, NewGameState),!.

remove_blocked_stones_piece(2, GameState,GameAcc,C-L, NewGameState ):-
    get_piece(GameState, C-L, Piece),
    adjacent_stones(GameState, C-L, Stones),
    \+ member(empty, Stones),
    set_piece(GameAcc, C-L, empty, NewBoard1),
    remove_all_black_neighbours(NewBoard1, C-L, NewGameState), !.

remove_blocked_stones_piece(3, GameState,GameAcc,C-L, NewGameState ):-
    get_piece(GameState, C-L, Piece),
    adjacent_stones(GameState, C-L, Stones),
    \+ member(empty, Stones),
    set_piece(GameAcc, C-L, empty, NewBoard1),
    remove_all_black(NewBoard1, NewGameState), !.

%used for high churn removes all black pieces fromt the board and replaces them with empty
remove_all_black(GameState, NewBoard):-
    replace_pieces(GameState, black, empty,NewBoard).


remove_blocked_stones_piece( Variant,GameState,GameAcc,C-L, GameAcc):-
    get_piece(GameState, C-L, Piece),
    adjacent_stones(GameState, C-L, Stones),
    member(empty, Stones) , !.

%used for Medium Churn, recursively removes all neighboring black stones from the board
remove_all_black_neighbours(GameState, C-L, NewBoard):-
    adjacent_black_stones_coordinates(GameState, C-L, Stones),
    remove_all_black_neighbours(GameState, Stones, NewBoard).

remove_all_black_neighbours(GameState, [], GameState).
remove_all_black_neighbours(GameState, [C-L|OtherStones], NewBoard):-
   set_piece(GameState, C-L, empty, TempBoard),
    remove_all_black_neighbours(TempBoard, OtherStones, NewBoard).

%checks if move is valid queen move
check_move(GameState-Player,Move):-
    player_has_piece(GameState-Player, Move),
    is_destination_empty(GameState,Move),
    valid_direction(Move),
    path_is_clear(GameState, Move),!.
    
%checks if direction is either horizontal,vertical or diagonal 
valid_direction(C1-L1-C2-L2):-
    C1 =:= C2.
valid_direction(C1-L1-C2-L2):-
    L1 =:= L2.
valid_direction(C1-L1-C2-L2):-
    C1 - C2 =:= L1 - L2.
valid_direction(C1-L1-C2-L2):-
    C1 - C2 =:= L2 - L1.
    
%checks if there is no non empty stone in the way, uses next to get the next stone to visit while doing the path
path_is_clear(GameState, C1-L1-C2-L2):-
    path_is_clear(GameState, C1-L1-C2-L2, C1, L1).
path_is_clear(GameState, C1-L1-C2-L2, C, L):-
    C =:= C2, L =:= L2.
path_is_clear(GameState, C1-L1-C2-L2, C, L):-
    next(C, C2, NextC),
    next(L, L2, NextL),
    get_piece(GameState, NextC-NextL, empty),
    path_is_clear(GameState, C1-L1-C2-L2, NextC, NextL).

%if C2 is bigger, increase C
next(C, C2, NextC):-
    C < C2, NextC is C + 1.

%if C2 is smaller, decrease C
next(C, C2, NextC):-
    C > C2, NextC is C - 1.

%if they are the same dont move C
next(C, C2, C):-
    C =:= C2.

%if L2 is bigger increase L
next(L, L2, NextL):-
    L < L2, NextL is L + 1.

%if L2 is smaller decrease L
next(L, L2, NextL):-
    L > L2, NextL is L - 1.

%if same , keep L unchanged
next(L, L2, L):-
    L =:= L2.

%checks if there is no non empty stone in the destination
is_destination_empty(GameState, C1-L1-C2-L2):-
    get_piece(GameState, C2-L2, empty).

%for all pieces of a given color gets list of the ratios LenEmpt/LenTot sums values and returns negative is used for calculating state value
ratio_surrounding_color(GameState, Color, Num):-
    findall(Ratio,(
            get_piece(GameState,C-L,Color),
            adjacent_stones(GameState, C-L, Stones),
            findall(Emp, (member(empty,Stones)), Empties),
            length(Empties,LenEmpt),
            length(Stones, LenTot),
            Ratio is LenEmpt/LenTot),Ratios
    ),
    sumlist(Ratios,NumSimetric),
    Num is - NumSimetric.

%checks if a move is in board range
within_range(Move,GameState) :-
    length(GameState, Size),
    between(1, Size, FromX),
    between(1, Size, FromY),
    between(1, Size, ToX),
    between(1, Size, ToY),
    Move = FromX-FromY-ToX-ToY.
