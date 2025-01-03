display_game(GameState-Player):-
    display_player(Player),
    display_board(GameState).

display_player(Player):-
    nl, write('Player: '),
    display_colored_player(Player),
    nl.

display_colored_player(player1) :-
    write('\e[31mRed Player'), 
    write('\e[0m'), nl.
display_colored_player(player2) :-
    write('\e[34mBlue Player'), 
    write('\e[0m'), nl.  
display_colored_player(computer1) :-
    write('\e[31mRed Computer'), 
    write('\e[0m'), nl.  
display_colored_player(computer2) :-
    write('\e[34mBlue Computer'), 
    write('\e[0m'), nl.  
display_colored_player(Player) :-
    write(Player), nl.


display_board(Board) :-
    length(Board, Size),
    Size < 10,
    display_column_labels(Size),
    display_board_rows(Board, Size).

display_board(Board) :-
    length(Board, Size),
    Size >= 10,
    display_column_labels_big(Size),
    display_board_rows(Board, Size).


display_column_labels(Size) :-
    write('   '), % Spacing for row labels
    display_columns(0, Size),
    nl.

display_column_labels_big(Size) :-
    write('    '), % Spacing for row labels
    display_columns(0, Size),
    nl.

display_columns(Col, Size) :-
    Col < Size,
    Letter is Col + 97, % ASCII value for 'a' is 97
    format('~c  ', [Letter]),
    NextCol is Col + 1,
    display_columns(NextCol, Size).
display_columns(_, _).


display_board_rows(Board, Size) :-
    Size < 10,
    reverse(Board, ReversedBoard),
    display_board_rows_reversed(ReversedBoard, Size, Size).

display_board_rows(Board, Size) :-
    Size >= 10,
    reverse(Board, ReversedBoard),
    display_board_rows_reversed_big(ReversedBoard, Size, Size).

display_board_rows_reversed([], _, _).
display_board_rows_reversed([Row | Rest], RowNum, Size) :-
    format('~w |', [RowNum]),
    display_row(Row),
    nl,
    NextRowNum is RowNum - 1,
    display_board_rows_reversed(Rest, NextRowNum, Size).

display_board_rows_reversed_big([], _, _).
display_board_rows_reversed_big([Row | Rest], RowNum, Size) :-
    RowNum >= 10,
    format('~w |', [RowNum]),
    display_row(Row),
    nl,
    NextRowNum is RowNum - 1,
    display_board_rows_reversed_big(Rest, NextRowNum, Size).
display_board_rows_reversed_big([Row | Rest], RowNum, Size) :-
    RowNum < 10,
    format('~w  |', [RowNum]),
    display_row(Row),
    nl,
    NextRowNum is RowNum - 1,
    display_board_rows_reversed_big(Rest, NextRowNum, Size).

display_row([]).
display_row([Cell | Rest]) :-
    display_cell(Cell),
    write(' '),
    display_row(Rest).

display_cell(empty) :- 
    write('\e[0m.'), 
    write(' ').
display_cell(red) :-   
    write('\e[31mO'), 
    write('\e[0m'),    
    write(' ').
display_cell(blue) :-  
    write('\e[34m0'), 
    write('\e[0m'),    
    write(' ').
display_cell(black) :- 
    write('\e[30mX'),
    write('\e[0m'),   
    write(' ').

congratulate(red) :-
    nl,
    write('\e[31mCongratulations, Red!'),
    write('\e[0m'), nl, nl.

congratulate(blue) :-
    nl,
    write('\e[34mCongratulations, Blue!'),
    write('\e[0m'), nl, nl.
