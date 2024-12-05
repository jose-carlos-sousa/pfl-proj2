:-ensure_loaded('game.pl').
draw_initial_menu(X) :-
    write('Welcome to BlackStone'), nl,
    write('Please select an option:'), nl,
    write('1. H/H -> Human vs Human'), nl,
    write('2. H/PC -> Human against PC'), nl,
    write('3. PC/H -> PC against Human'), nl,
    write('4. PC/PC -> PC against PC'), nl,
    get_char(X).

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
    format('~w ', [Col]),
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

print_cell(empty) :- write('.').
print_cell(red)   :- write('R').
print_cell(blue)  :- write('B').
print_cell(black) :- write('W').