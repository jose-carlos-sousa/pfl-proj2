/*

initial_board(+Size, -Board) Takes the Size of the Board and gives the Board

*/
initial_board(Size, Board) :-
    findall(Row,(between(1,Size,X), initial_board_row(X,Size,Row)),Board). 

/*

initial_board_row(+Row, +Size, -BoardRow) Given Row number and Size returns the given row

*/
initial_board_row(Row, Size, BoardRow) :-
    findall(Piece,(between(1,Size,X),initial_board_cell(Row,X,Size,Piece)),BoardRow).
    
/*

initial_board_cell(+Row, +Col, +Size, -Piece) Given row number, col number, size of board gives the piece

*/

%for row 1 red is in odd places
initial_board_cell(1,Y,Size, red) :-
    Y > 1,
    Y < Size,
    Y mod 2 =:= 1,!.

%for max row red is in even places
initial_board_cell(Size,Y,Size, red) :-
    Y > 1,
    Y < Size,
    Y mod 2 =:= 0,!.

%for col 1 blue is in even places
initial_board_cell(X,1,Size, blue) :-
    X > 1,
    X < Size,
    X mod 2 =:= 0,!.

%for col max blue is in odd places
initial_board_cell(X,Size, Size, blue) :-
    X > 1,
    X < Size,
    X mod 2 =:= 1,!.

%for every other place an empty piece is placed
initial_board_cell(_,_,_, empty).

%helper predicate that given a board replaces all pieces of color1 by color2 and gives new state
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

%replaces the element at a specified index(I) in a list with a new value
replace(1, [_|T], X, [X|T]).
replace(I, [H|T], X, [H|R]):-
    I > 1,
    I1 is I - 1,
    replace(I1, T, X, R).

%determines if two places on the board are adjacent 
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

%computes the next position on a board, either moving to the next column in the current row or advancing to the first column of the next row 
next_position(C-L, C-L2, NextC-NextL):-
    NextL is L + 1,
    NextC is 1.
next_position(C-L, _-_, NextC-NextL):-
    NextC is C + 1,
    NextL is L.

%gives the adjancent pieces to a given place(C-L)
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

%helper predicate that gets the Piece in a given place(C-L) of the board
get_piece(GameState, C-L, Piece):-
    nth1(L, GameState, Row),
    nth1(C, Row, Piece).

%helper predicate that finds all the pieces of a given player in the board
find_player_pieces(GameState, Player, Coordinates) :-
    findall(C-L, (
        nth1(L, GameState, Row),
        nth1(C, Row, Player)
    ), Coordinates).

%helper predicate that updates the board by setting a place(C-L) to a desired piece
set_piece(GameState, C-L, Piece, NewGameState):-
    nth1(L, GameState, Row),
    replace(C, Row, Piece, NewRow),
    replace(L, GameState, NewRow, NewGameState).

%predicate that checks board has blue stones left
there_are_blue_left([]):- fail.
there_are_blue_left([CurRow|OtherRows]):-
    member(blue, CurRow), !.
there_are_blue_left([CurRow|OtherRows]):-
    \+ member(blue, CurRow),
    there_are_blue_left(OtherRows).

%predicate that checks board has red stones left
there_are_red_left([]):- fail.
there_are_red_left([CurRow|OtherRows]):-
    member(red, CurRow), !.
there_are_red_left([CurRow|OtherRows]):-
    \+ member(red, CurRow),
    there_are_red_left(OtherRows).
