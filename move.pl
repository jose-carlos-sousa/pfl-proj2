
check_move(GameState-Player, Move):-
    within_range(Move,GameState),
    valid_queen_move(GameState, Move).


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



% se o atual for empty  eu n mudo
remove_blocked_stones_piece(_,GameState,GameAcc,C-L, GameAcc):-
    get_piece(GameState, C-L, empty), !.

remove_blocked_stones_piece(_,GameState,GameAcc,C-L, GameAcc):-
    get_piece(GameState, C-L, black), !.

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
    remove_all_black_neightbours(NewBoard1, C-L, NewGameState), !.

remove_blocked_stones_piece(3, GameState,GameAcc,C-L, NewGameState ):-
    get_piece(GameState, C-L, Piece),
    adjacent_stones(GameState, C-L, Stones),
    \+ member(empty, Stones),
    set_piece(GameAcc, C-L, empty, NewBoard1),
    remove_all_black(NewBoard1, NewGameState), !.

remove_all_black(GameState, NewBoard):-
    replace_pieces(GameState, black, empty,NewBoard).


remove_blocked_stones_piece( Variant,GameState,GameAcc,C-L, GameAcc):-
    get_piece(GameState, C-L, Piece),
    adjacent_stones(GameState, C-L, Stones),
    member(empty, Stones) , !.

% Recursively remove all neighboring black stones from the board
remove_all_black_neightbours(GameState, C-L, NewBoard):-
    adjacent_black_stones_coordinates(GameState, C-L, Stones),
    remove_all_black_neightbours(GameState, Stones, NewBoard).
remove_all_black_neightbours(GameState, [], GameState).
remove_all_black_neightbours(GameState, [C-L|OtherStones], NewBoard):-
   set_piece(GameState, C-L, empty, TempBoard),
    remove_all_black_neightbours(TempBoard, OtherStones, NewBoard).
% Find all black pieces adjacent to a given position


valid_queen_move(GameState,Move):-
    player_has_piece(GameState-Player, Move),
    is_destination_empty(GameState,Move),
    valid_direction(Move),
    path_is_clear(GameState, Move).
    
valid_direction(C1-L1-C2-L2):-
    C1 =:= C2.
valid_direction(C1-L1-C2-L2):-
    L1 =:= L2.
valid_direction(C1-L1-C2-L2):-
    C1 - C2 =:= L1 - L2.
valid_direction(C1-L1-C2-L2):-
    C1 - C2 =:= L2 - L1.
    
path_is_clear(GameState, C1-L1-C2-L2):-
    path_is_clear(GameState, C1-L1-C2-L2, C1, L1).
path_is_clear(GameState, C1-L1-C2-L2, C, L):-
    C =:= C2, L =:= L2.
path_is_clear(GameState, C1-L1-C2-L2, C, L):-
    next(C, C2, NextC),
    next(L, L2, NextL),
    get_piece(GameState, NextC-NextL, empty),
    path_is_clear(GameState, C1-L1-C2-L2, NextC, NextL).
next(C, C2, NextC):-
    C < C2, NextC is C + 1.
next(C, C2, NextC):-
    C > C2, NextC is C - 1.
next(C, C2, C):-
    C =:= C2.
next(L, L2, NextL):-
    L < L2, NextL is L + 1.
next(L, L2, NextL):-
    L > L2, NextL is L - 1.
next(L, L2, L):-
    L =:= L2.

is_destination_empty(GameState, C1-L1-C2-L2):-
    get_piece(GameState, C2-L2, empty).


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

within_range(Move,GameState) :-
    length(GameState, Size),
    between(1, Size, FromX),
    between(1, Size, FromY),
    between(1, Size, ToX),
    between(1, Size, ToY),
    Move = FromX-FromY-ToX-ToY.
