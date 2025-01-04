display_game(GameState-Player-_-_):-
    display_player(Player),
    display_board(GameState).

display_player(Player):-
    nl, write('Player: '),
    display_colored_player(Player),
    nl.

display_colored_player(Player) :-
    player_piece(red,Player),
    write('\e[31mRed Player'), 
    write('\e[0m'), nl.
display_colored_player(Player) :-
    player_piece(blue,Player),
    write('\e[34mBlue Player'), 
    write('\e[0m'), nl.  



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


get_board_size(ValidSize):-
    write('Enter board size (even number between 4 and 26):'), nl,
    catch(read(Size), _, (write('Read error. This may cause the next reads to fail.'), nl, get_board_size(Size))),
    validate_size(Size, ValidSize).


validate_size(Size, ValidSize) :-
    integer(Size),
    Size >= 4,
    Size =< 26,
    Size mod 2 =:= 0,
    !,
    ValidSize = Size.

validate_size(_, ValidSize) :-
    write('Invalid input.'), nl,
    get_board_size(ValidSize).


get_variant(Variant):-
    write('Choose variant:'), nl,
    write('1. Standard'), nl,
    write('2. Medium Churn'), nl,
    write('3. High Churn'), nl,
    catch(read(Variant_Chosen), _, (write('Read error. This may cause the next reads to fail.'), nl, get_variant(Variant_Chosen))),
    validate_variant(Variant_Chosen, Variant).

validate_variant(Variant_Chosen, Variant) :-
    member(Variant_Chosen, [1, 2, 3]),
    !,
    Variant = Variant_Chosen.
validate_variant(_, Variant) :-
    nl, write('Invalid variant chosen.'), nl, nl,
    get_variant(Variant).

get_game_mode(GameMode):-
    write('Choose game mode:'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer'), nl,
    write('3. Computer vs Player'), nl,
    write('4. Computer vs Computer'), nl,
    catch(read(Mode), _, (write('Read error. This may cause the next reads to fail.'), nl, get_game_mode(Mode))),
    validate_mode(Mode, GameMode).

validate_mode(Mode, GameMode) :-
    member(Mode, [1, 2, 3, 4]),
    !,
    GameMode = Mode.
validate_mode(_, GameMode) :-
    nl, write('Invalid mode chosen.'), nl, nl,
    get_game_mode(GameMode).

get_ai_level(Level):-
    write('Choose AI level:'), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    catch(read(AI), _, (write('Read error. This may cause the next reads to fail.'), nl, get_ai_level(AI))),
    validate_AI(AI, Level).
validate_AI(AI, Level) :-
    member(AI, [1, 2]),
    !,
    Level = AI.
validate_AI(_, Level) :-
    nl, write('Invalid AI level chosen.'), nl, nl,
    get_ai_level(Level).


get_move(Move) :-
    nl, write('Enter move (e.g., b1-c3): '), nl,
    catch(read(InputMove), _, (nl, write('Read error. This may cause the next reads to fail.'), nl, fail)),
    validate_move_format(InputMove),
    transform_move(InputMove, TransformedMove),
    Move = TransformedMove.

display_color(Player):-
    player_piece(red,Player),
    write('Red').

display_color(Player):-
    player_piece(blue,Player),
    write('Blue').

validate_move_format(Start-End) :-
    atom_chars(Start, StartChars), % Posição inicial
    atom_chars(End, EndChars),     % Posição final
    validate_position_format(StartChars),
    validate_position_format(EndChars).

validate_position_format([Col|RowChars]) :-
    char_code(Col, ColCode),
    ColCode >= 97, ColCode =< 122, % coluna é uma letra
    maplist(char_code, RowChars, RowCodes),
    maplist(between(48, 57), RowCodes). % linha é um número

transform_move(Start-End, StartColNum-StartRowNum-EndColNum-EndRowNum) :-
    atom_chars(Start, [StartCol|StartRowChars]), % Separar coluna e linha
    atom_chars(End, [EndCol|EndRowChars]),       % Separar coluna e linha
    col_to_num(StartCol, StartColNum),
    col_to_num(EndCol, EndColNum),
    maplist(char_code, StartRowChars, StartRowCodes),
    maplist(char_code, EndRowChars, EndRowCodes),
    number_codes(StartRowNum, StartRowCodes), % Converter string para número
    number_codes(EndRowNum, EndRowCodes).     % Converter string para número

col_to_num(Col, Num) :-
    char_code(Col, Code),
    Num is Code - 96. % 'a' -> 1

inverse_transform_move(StartColNum-StartRowNum-EndColNum-EndRowNum, Start-End) :-
    num_to_col(StartColNum, StartCol),
    num_to_col(EndColNum, EndCol),
    number_codes(StartRowNum, StartRowCodes),
    number_codes(EndRowNum, EndRowCodes),
    atom_codes(StartRow, StartRowCodes),
    atom_codes(EndRow, EndRowCodes),
    atom_concat(StartCol, StartRow, Start),
    atom_concat(EndCol, EndRow, End).

num_to_col(Num, Col) :-
    Code is Num + 96, % 1 -> 'a'
    char_code(Col, Code).

