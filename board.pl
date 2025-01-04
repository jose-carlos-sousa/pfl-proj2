/*

initial_board(+Size, -Board) Takes the Size of the Board and gives the Board

*/
initial_board(Size, Board) :-
    findall(Row,(between(1,Size,X), initial_board_row(X,Size,Row)),Board). 

initial_board_row(Row, Size, BoardRow) :-
    findall(Piece,(between(1,Size,X),initial_board_cell(Row,X,Piece,Size)),BoardRow).
    
initial_board_cell(1,Y, red,Size) :-
    Y > 1,
    Y < Size,
    Y mod 2 =:= 1,!.

initial_board_cell(Size,Y, red,Size) :-
    Y > 1,
    Y < Size,
    Y mod 2 =:= 0,!.

initial_board_cell(X,1, blue,Size) :-
    X > 1,
    X < Size,
    X mod 2 =:= 0,!.

initial_board_cell(X,Size, blue,Size) :-
    X > 1,
    X < Size,
    X mod 2 =:= 1,!.

initial_board_cell(_,_, empty,_).

set_piece(GameState, C-L, Piece, NewGameState):-
    nth1(L, GameState, Row),
    replace(C, Row, Piece, NewRow),
    replace(L, GameState, NewRow, NewGameState).

replace_pieces(GameState, Color1, Color2, NewGameState):-
    length(GameState, L),
    replace_pieces(GameState, 1-1, L-L, Color1, Color2, NewGameState).

replace_pieces(GameState, C-L, C-L, Color1, Color2, NewGameState):-
    get_piece(GameState, C-L, Color1),
    set_piece(GameState, C-L, Color2, NewGameState), !.
replace_pieces(GameState, C-L, C-L, _ , _ ,GameState).
  
replace_pieces(GameState, C-L, C1-L1, Color1, Color2, NewGameState):-
    get_piece(GameState, C-L, Color1),
    set_piece(GameState, C-L, Color2, TempGameState),
    next_position(C-L, C1-L1, NextC-NextL),
    replace_pieces(TempGameState, NextC-NextL, C1-L1, Color1, Color2, NewGameState), !.
replace_pieces(GameState, C-L, C1-L1, Color1, Color2, NewGameState):-
    next_position(C-L, C1-L1, NextC-NextL),
    replace_pieces(GameState, NextC-NextL, C1-L1, Color1, Color2, NewGameState).

replace(1, [_|T], X, [X|T]).
replace(I, [H|T], X, [H|R]):-
    I > 1,
    I1 is I - 1,
    replace(I1, T, X, R).


adjacent(C-L, C2-L2):- 
    C2 is C + 1, L2 is L.
adjacent(C-L, C2-L2):-
    C2 is C - 1, L2 is L.
adjacent(C-L, C2-L2):-
    C2 is C, L2 is L + 1.
adjacent(C-L, C2-L2):-
    C2 is C, L2 is L - 1.
adjacent(C-L, C2-L2):-
    C2 is C + 1, L2 is L + 1.
adjacent(C-L, C2-L2):-
    C2 is C - 1, L2 is L - 1.
adjacent(C-L, C2-L2):-
    C2 is C + 1, L2 is L - 1.
adjacent(C-L, C2-L2):-
    C2 is C - 1, L2 is L + 1.

next_position(C-L, C-L2, NextC-NextL):-
    NextL is L + 1,
    NextC is 1.
next_position(C-L, _-_, NextC-NextL):-
    NextC is C + 1,
    NextL is L.

adjacent_stones(GameState, C-L, Stones):-
    findall(Piece, 
            (   adjacent(C-L, C2-L2),                 % Get adjacent coordinates
                get_piece(GameState, C2-L2, Piece)),   % Get the piece at the adjacent position
            Stones).
adjacent_black_stones_coordinates(GameState, C-L, Stones):-
    findall(C2-L2, 
            (   adjacent(C-L, C2-L2),                 % Get adjacent coordinates
                get_piece(GameState, C2-L2, black)),   % Get the piece at the adjacent position
            Stones).

get_piece(GameState, C-L, Piece):-
    nth1(L, GameState, Row),
    nth1(C, Row, Piece).


there_are_blue_left([]):- fail.
there_are_blue_left([CurRow|OtherRows]):-
    member(blue, CurRow), !.
there_are_blue_left([CurRow|OtherRows]):-
    \+ member(blue, CurRow),
    there_are_blue_left(OtherRows).

there_are_red_left([]):- fail.
there_are_red_left([CurRow|OtherRows]):-
    member(red, CurRow), !.
there_are_red_left([CurRow|OtherRows]):-
    \+ member(red, CurRow),
    there_are_red_left(OtherRows).
