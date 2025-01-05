
/*

remove_blocked_stones(+Variant, +Board, -NewBoard)
Given the game variant checks if stones are blocked and if so removes the blocked stone and depending on Variant removes:
Medium -> Adjacent Black Stones
High -> All Black Stones

*/
remove_blocked_stones(Variant,[H|T], NewBoard):-
    length(H, NumCols),
    length([H|T], NumRows),
    remove_blocked_stones_helper(Variant,1-1, NumCols-NumRows, [H|T],[H|T], NewBoard) , !.

% has original board board accumulator and final 
remove_blocked_stones_helper(Variant,C-L, C-L, Board,BoardAcc, NewBoard):- 
    remove_blocked_stones_piece(Variant,Board,BoardAcc, C-L, NewBoard).

remove_blocked_stones_helper(Variant,C-L, C2-L2, Board,BoardAcc, NextBoard):-
    remove_blocked_stones_piece(Variant,Board,BoardAcc, C-L, NewBoard),
    next_position(C-L, C2-L2, NextC-NextL),
    remove_blocked_stones_helper(Variant,NextC-NextL, C2-L2,Board ,NewBoard, NextBoard).




/*

remove_blocked_stones_piece(+Variant, +Board, +GameAcc, +C-L, -NewBoard )

Get Piece
Get adjacents
if there is no empty stone in adjacent the piece is blocked

For Normal: set the piece to empty
For Medium Churn: set the piece to empty && Remove all black pieces adjacents
For High Churn:  set the piece to empty && Remove all black pieces in the board

*/

% if piece is empty nothing should be done
remove_blocked_stones_piece(_,Board,GameAcc,C-L, GameAcc):-
    get_piece(Board, C-L, empty), !.

% if piece is black nothing should be done
remove_blocked_stones_piece(_,Board,GameAcc,C-L, GameAcc):-
    get_piece(Board, C-L, black), !.

remove_blocked_stones_piece(1, Board,GameAcc,C-L, NewBoard ):-
    adjacent_stones(Board, C-L, Stones),
    \+ member(empty, Stones),
    set_piece(GameAcc, C-L, empty, NewBoard),!.

remove_blocked_stones_piece(2, Board,GameAcc,C-L, NewBoard ):-
    adjacent_stones(Board, C-L, Stones),
    \+ member(empty, Stones),
    set_piece(GameAcc, C-L, empty, NewBoard1),
    remove_all_black_neighbours(NewBoard1, C-L, NewBoard), !.

remove_blocked_stones_piece(3, Board,GameAcc,C-L, NewBoard ):-
    adjacent_stones(Board, C-L, Stones),
    \+ member(empty, Stones),
    set_piece(GameAcc, C-L, empty, NewBoard1),
    remove_all_black(NewBoard1, NewBoard), !.

remove_blocked_stones_piece( _,Board,GameAcc,C-L, GameAcc):-
    adjacent_stones(Board, C-L, Stones),
    member(empty, Stones) , !.

%used for high churn removes all black pieces fromt the board and replaces them with empty
remove_all_black(Board, NewBoard):-
    replace_pieces(Board, black, empty,NewBoard).




%used for Medium Churn, recursively removes all neighboring black stones from the board
remove_all_black_neighbours(Board, C-L, NewBoard):-
    adjacent_black_stones_coordinates(Board, C-L, Stones),
    remove_all_black_neighbours(Board, Stones, NewBoard).

remove_all_black_neighbours(Board, [], Board).
remove_all_black_neighbours(Board, [C-L|OtherStones], NewBoard):-
   set_piece(Board, C-L, empty, TempBoard),
    remove_all_black_neighbours(TempBoard, OtherStones, NewBoard).

%checks if move is valid queen move
check_move(Board-Player,Move):-
    player_has_piece(Board-Player, Move),
    is_destination_empty(Board,Move),
    valid_direction(Move),
    path_is_clear(Board, Move), !.
    
%checks if direction is either horizontal,vertical or diagonal 
valid_direction(C1-_-C2-_):-
    C1 =:= C2.
valid_direction(_-L1-_-L2):-
    L1 =:= L2.
valid_direction(C1-L1-C2-L2):-
    C1 - C2 =:= L1 - L2.
valid_direction(C1-L1-C2-L2):-
    C1 - C2 =:= L2 - L1.
    
%checks if there is no non empty stone in the way, uses next to get the next stone to visit while doing the path
path_is_clear(Board, C1-L1-C2-L2):-
    path_is_clear(Board, C1-L1-C2-L2, C1, L1).
path_is_clear(_, _-_-C2-L2, C, L):-
    C =:= C2, L =:= L2.
path_is_clear(Board, C1-L1-C2-L2, C, L):-
    next(C, C2, NextC),
    next(L, L2, NextL),
    get_piece(Board, NextC-NextL, empty),
    path_is_clear(Board, C1-L1-C2-L2, NextC, NextL).

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
is_destination_empty(Board, _-_-C2-L2):-
    get_piece(Board, C2-L2, empty).

%for all pieces of a given color gets list of the ratios LenEmpt/LenTot sums values and returns negative is used for calculating state value
ratio_surrounding_color(Board, Color, Num):-
    findall(Ratio,(
            get_piece(Board,C-L,Color),
            adjacent_stones(Board, C-L, Stones),
            findall(_, (member(empty,Stones)), Empties),
            length(Empties,LenEmpt),
            length(Stones, LenTot),
            Ratio is LenEmpt/LenTot),Ratios
    ),
    sumlist(Ratios,NumSimetric),
    Num is - NumSimetric.

% counts the number of pieces for a given player
count_pieces(Board, PlayerColor, Count) :-
    findall(_, get_piece(Board, _-_, PlayerColor), Pieces),
    length(Pieces, Count).

% calculates the difference in the number of pieces between two players
piece_difference(Board, Player1Color, Player2Color, Difference) :-
    count_pieces(Board, Player1Color, Count1),
    count_pieces(Board, Player2Color, Count2),
    Difference is Count1 - Count2.

%generates moves for a given player
generate_moves(Board, Player, Move) :-
    player_piece(Color, Player),
    find_player_pieces(Board, Color, Coordinates),
    member(C1-L1, Coordinates),
    length(Board, Size),
    between(1, Size, C2),
    between(1, Size, L2),
    dif(C1-L1, C2-L2),
    Move = C1-L1-C2-L2.

