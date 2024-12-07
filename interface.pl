:-ensure_loaded('game.pl').
draw_initial_menu(X) :-
    write('Welcome to BlackStone'), nl,
    write('Please select an option:'), nl,
    write('1. H/H -> Human vs Human'), nl,
    write('2. H/PC -> Human against PC'), nl,
    write('3. PC/H -> PC against Human'), nl,
    write('4. PC/PC -> PC against PC'), nl,
    read_number(X).

print_board(Board) :-
    length(Board, Size),
    print_column_labels(Size),
    print_board_rows(Board, 1).


print_column_labels(Size) :-
    write('   '), % Spacing for row labels
    print_columns(1, Size),
    nl.


print_columns(Col, Size) :-
    Col =< Size,
    LetterCode is Col + 64,
    char_code(Letter, LetterCode),
    format('~w  ', [Letter]),
    NextCol is Col + 1,
    print_columns(NextCol, Size).
print_columns(_, _).


print_board_rows([], _).
print_board_rows([Row | Rest], RowNum) :-
    format('~w |', [RowNum]), % Row label
    print_row(Row),
    nl,
    NextRowNum is RowNum + 1,
    print_board_rows(Rest, NextRowNum).

print_row([]).
print_row([Cell | Rest]) :-
    print_cell(Cell),
    write(' '),
    print_row(Rest).

print_cell(empty) :- 
    write('\e[0m.'), 
    write(' ').
print_cell(red) :-   
    write('\e[31mO'), 
    write('\e[0m'),    
    write(' ').
print_cell(blue) :-  
    write('\e[34m0'), 
    write('\e[0m'),    
    write(' ').
print_cell(black) :- 
    write('\e[30mX'),
    write('\e[0m'),   
    write(' ').

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


letter_to_col(Code, Col) :-
    BaseCode is 65,     
    Col is Code - BaseCode + 1.

read_move( StartRow-StartCol) :-
    get_code(ColChar),      
    read_number(StartRow),
    letter_to_col(ColChar, StartCol).       


get_move(CurPlayer, StartRow-StartCol, EndRow-EndCol) :-
    repeat,
    write('Player '), write(CurPlayer), write(' from: '),
    read_move( StartRow-StartCol),
    write('Move to: '),
    read_move( EndRow-EndCol),
    !.