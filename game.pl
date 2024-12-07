:- consult('interface.pl').
:- use_module(library(between)).
:- use_module(library(lists)).

% initial statte has sqaure size and GameMode , gamestate has empty squares red blue black

init_board(Size, Board) :-
    init_board(Size, 1, [], Board). 

init_board(Size, Row, AccumulatedBoard, Board) :-
    Row =< Size, 
    init_board_row(Row, Size, BoardRow), 
    append(AccumulatedBoard, [BoardRow], NewAccumulatedBoard),
    NextRow is Row + 1, 
    init_board(Size, NextRow, NewAccumulatedBoard, Board). 

init_board(Size, Row, Board, Board) :-
    Row > Size.



init_board_row(Row, Size, BoardRow) :-
    init_board_row(Row, 1, Size, [], BoardRowHelper),
    reverse(BoardRowHelper, BoardRow).
    

% Helper predicate to accumulate cells for each column in the row
init_board_row(_, Col, Size, Acc, Acc) :- 
    Col > Size. 
init_board_row(Row, Col, Size, Acc, BoardRow) :-
    init_board_cell(Row, Col, BoardCell, Size),  
    NewCol is Col + 1, 
    init_board_row(Row, NewCol, Size, [BoardCell|Acc], BoardRow).

init_board_cell(1,Y, red,Size) :-
    Y > 1,
    Y < Size,
    Y mod 2 =:= 0,!.

init_board_cell(Size,Y, red,Size) :-
    Y > 1,
    Y < Size,
    Y mod 2 =:= 1,!.

init_board_cell(X,1, blue,Size) :-
    X > 1,
    X < Size,
    X mod 2 =:= 1,!.

init_board_cell(X,Size, blue,Size) :-
    X > 1,
    X < Size,
    X mod 2 =:= 0,!.

init_board_cell(X,Y, empty,Size).


initial_state(Size-GameMode,Board-red) :-
    init_board(Size,Board).
    
read_number(X) :-
    catch(read_digits(0, X), error(invalid_input, _), fail).

read_digits(A, A) :-
    peek_code(10),
    get_code(10), !.

read_digits(A, X) :-
    peek_code(Code),
    Code \= 10,
    get_code(Code),
    (   digit_value(Code, Digit)
    ->  A1 is A * 10 + Digit,
        read_digits(A1, X)
    ;   throw(error(invalid_input, Code))
    ).

digit_value(Code, Digit) :-
    Digit is Code - 48,
    Digit >= 0,
    Digit =< 9.


get_cell(Board, Row, Col, Cell) :-
    nth1(Row, Board, BoardRow), 
    nth1(Col, BoardRow, Cell).

valid_move(Board-CurPlayer,  StartRow-StartCol, EndRow-EndCol ) :-
    player_has_cell(Board, StartRow-StartCol, CurPlayer),
    write('Player has cell'),nl,
    is_valid_queen_move(Board, StartRow-StartCol, EndRow-EndCol),
    write('Valid queen move'),nl,
    is_destination_empty(Board,  EndRow-EndCol),
    write('empty des').

player_has_cell(Board, Row-Col, Player) :-
    get_cell(Board, Row, Col, Player).
is_valid_queen_move(Board, StartRow-StartCol, EndRow-EndCol) :-
    player_has_cell(Board, StartRow-StartCol, CurPlayer),
    valid_direction(StartRow, StartCol, EndRow, EndCol, DirRow, DirCol),
    path_is_clear(Board, StartRow-StartCol, EndRow-EndCol, DirRow, DirCol).


valid_direction(StartRow, StartCol, EndRow, EndCol, DirRow, DirCol) :-
    DirRow is sign(EndRow - StartRow), 
    DirCol is sign(EndCol - StartCol), 
    (DirRow \= 0; DirCol \= 0).       

path_is_clear(Board, StartRow-StartCol, EndRow-EndCol, DirRow, DirCol) :-
    NextRow is StartRow + DirRow,
    NextCol is StartCol + DirCol,
    (NextRow =:= EndRow, NextCol =:= EndCol ;
     get_cell(Board, NextRow, NextCol, empty),
     path_is_clear(Board, NextRow-NextCol, EndRow-EndCol, DirRow, DirCol)).

is_destination_empty(Board, Row-Col) :-
    get_cell(Board, Row, Col, empty).

move(Board-CurPlayer, StartRow-StartCol, EndRow-EndCol, NewBoard-NewCurPlayer) :-
    set_cell(Board, StartRow-StartCol, black, UpdatedBoard),
    write('set cell'),nl,
    set_cell(UpdatedBoard, EndRow-EndCol, CurPlayer, NewBoard),
    write('set cell'),nl,
    switch_player(CurPlayer, NewCurPlayer).
switch_player(red, blue).
switch_player(blue, red).
set_cell(Board, Row-Col, Value, NewBoard) :-
    nth1(Row, Board, BoardRow),
    replace(BoardRow, Col, Value, NewRow),
    replace(Board, Row, NewRow, NewBoard).
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).


main_loop(GameMode,Board-CurPlayer) :-
    get_move(CurPlayer, StartRow-StartCol, EndRow-EndCol),
    write('Got move'),nl,
    write(StartRow),write('-'),write(StartCol),write('-'),write(EndRow),write('-'),write(EndCol),nl,
    valid_move(Board-CurPlayer ,StartRow-StartCol, EndRow-EndCol),
    move(Board-CurPlayer, StartRow-StartCol, EndRow-EndCol, NewBoard-NewCurPlayer),
    write('Valid move'),nl,
    print_board(NewBoard),
    main_loop(GameMode,NewBoard-NewCurPlayer).
play :-
    draw_initial_menu(GameMode),
    get_board_size(Size),
    initial_state(Size-GameMode,Board-CurPlayer),
    print_board(Board),
    main_loop(GameMode,Board-CurPlayer).
    

