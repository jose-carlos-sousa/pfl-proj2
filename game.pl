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

get_board_size(Size) :-
    repeat,
    write('Please select the board size (must be an even number larger than 2): '),
    (   read_number(Value) ->
        (   Value > 2, Value mod 2 =:= 0 ->
            Size = Value, !;
            write('Invalid input. Try again.'), nl, fail
        );
        write('Invalid input. Try again.'), nl, skip_line, fail
    ).

play :-
    draw_initial_menu(X),
    get_board_size(Size),
    initial_state(Size-X,Board-State),
    print_board(Board).
    

% consult('game.pl').