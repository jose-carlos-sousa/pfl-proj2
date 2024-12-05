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


initial_state(Size-GameMode,Board-0) :-
    init_board(Size,Board).
    
play :-
    draw_initial_menu(X),
    initial_state(6-X,Board-State),
    print_board(Board).
    