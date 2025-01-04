:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).

:- consult('interface.pl').

%play/0 initial predicate, gives access to game menu
%allows you to choose game mode and board size

play:-
    get_game_mode(GameMode),
    get_board_size(Size),
    initial_state(GameMode-Size, GameState-Player-NextPlayer-Variant),
    display_game(GameState-Player-NextPlayer-Variant),
    game_cycle(GameState-Player-NextPlayer-Variant, GameMode).



%initial_state(+GameConfig, -GameState)
%takes as input GameConfig with the specified mode and board size and outputs the initial gamestate
%the game state consists of Board 2D list , curplayer, nextplayer and variant
initial_state(GameMode-Size, Board-Player-NextPlayer-Variant):-
    initial_board(Size,Board),
    initial_players(GameMode, Player,NextPlayer),
    get_variant(Variant).

get_ai_player(1,second,computer2_easy).
get_ai_player(2,second,computer2_hard).
get_ai_player(1,first,computer1_easy).
get_ai_player(2,first,computer1_hard).

initial_players(1, player1,player2):- !.
initial_players(2,player1,Computer):-
    get_AI_level(X),
    get_ai_player(X,second,Computer).


initial_players(3,Computer,player2):-
    get_AI_level(X),
    get_ai_player(X,first,Computer).

initial_players(4,Computer1,Computer2):-
    get_AI_level(X),
    get_ai_player(X,first,Computer1),
    get_ai_player(X,second,Computer2).
    

level_of_ai(player1,na).
level_of_ai(player2,na).
level_of_ai(computer1_easy,1).
level_of_ai(computer1_hard,2).
level_of_ai(computer2_easy,1).
level_of_ai(computer2_hard,2).

initial_board(Size,Board):-
    init_board(Size, Board).

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
    Y mod 2 =:= 1,!.

init_board_cell(Size,Y, red,Size) :-
    Y > 1,
    Y < Size,
    Y mod 2 =:= 0,!.

init_board_cell(X,1, blue,Size) :-
    X > 1,
    X < Size,
    X mod 2 =:= 0,!.

init_board_cell(X,Size, blue,Size) :-
    X > 1,
    X < Size,
    X mod 2 =:= 1,!.

init_board_cell(_,_, empty,_).

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

get_AI_level(Level):-
    write('Choose AI level:'), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    catch(read(AI), _, (write('Read error. This may cause the next reads to fail.'), nl, get_AI_level(AI))),
    validate_AI(AI, Level).
validate_AI(AI, Level) :-
    member(AI, [1, 2]),
    !,
    Level = AI.
validate_AI(_, Level) :-
    nl, write('Invalid AI level chosen.'), nl, nl,
    get_AI_level(Level).


game_cycle(GameState-Player-_-_,GameMode):-
    game_over(GameState-Player, Winner), !,
    congratulate(Winner).
game_cycle(GameState-Player-NextPlayer-Variant,GameMode):-
    level_of_ai(Player, Level),
    choose_move(GameState-Player-NextPlayer-Variant,Level, Move),
    move(GameState-Player-_-Variant, Move, NewGameState),
    display_game(NewGameState-NextPlayer-Player-Variant),
    game_cycle(NewGameState-NextPlayer-Player-Variant,GameMode).

game_cycle(GameState-Player-NextPlayer-Variant,GameMode):-
    display_game(GameState-Player-NextPlayer-Variant),
    game_cycle(GameState-Player-NextPlayer-Variant,GameMode).

% basicamente vemos se o move é válido e se vamos buscar a peça , metemos uma preta no sitio dela, e depois metemos a peça no sitio de destino
move(GameState-Player-_-Variant, C1-L1-C2-L2, NewGameState):-
    check_move(GameState-Player, C1-L1-C2-L2),
    !,
    get_piece(GameState, C1-L1, Piece),
    set_piece(GameState, C1-L1, black, TempGameState),
    set_piece(TempGameState, C2-L2, Piece, TempGameState2),
    remove_blocked_stones(Variant,TempGameState2, TempGameState3),
    NewGameState = TempGameState3.

move(GameState-_-_-_, _, GameState):- fail.



check_move(GameState-Player, Move):-
    player_has_piece(GameState-Player, Move),
    valid_queen_move(GameState, Move),
    is_destination_empty(GameState, Move).
player_has_piece(GameState-Player, C1-L1-_-_):-
    get_piece(GameState, C1-L1, Piece),
    player_piece(Piece, Player).



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


next_position(C-L, C-L2, NextC-NextL):-
    NextL is L + 1,
    NextC is 1.
next_position(C-L, _-_, NextC-NextL):-
    NextC is C + 1,
    NextL is L.
    

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
    
    
% if we gucci lets just chill i mmmmmmmma fell n sei que light alivevevveveve i see forever in ur eyes she smile smileleeeenjewjbalvlqeLVKJEDAS<
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
valid_queen_move(GameState, C1-L1-C2-L2):-
    valid_direction(C1-L1-C2-L2),
    path_is_clear(GameState, C1-L1-C2-L2).
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
get_piece(GameState, C-L, Piece):-
    nth1(L, GameState, Row),
    nth1(C, Row, Piece).
player_piece(red, player1).
player_piece(red, computer1_easy).
player_piece(red, computer1_hard).
player_piece(blue, player2).
player_piece(blue, computer2_easy).
player_piece(blue, computer2_hard).

game_over(GameState-Player, Winner):-
    there_are_blue_left(GameState),
    there_are_red_left(GameState),
    fail.

game_over(GameState-Player, Winner):-
    there_are_blue_left(GameState), 
    \+ there_are_red_left(GameState),
    Winner = blue.

game_over(GameState-Player, Winner):-
    there_are_red_left(GameState), 
    \+ there_are_blue_left(GameState),
    Winner = red.

game_over(GameState-Player, Winner):-
    \+ there_are_red_left(GameState), 
    \+ there_are_blue_left(GameState),
    get_draw_winner(Player, Winner).

get_draw_winner(player1, blue).
get_draw_winner(player2, red).
get_draw_winner(computer1_easy, blue).
get_draw_winner(computer1_hard, blue).
get_draw_winner(computer2_easy, red).
get_draw_winner(computer2_hard, red).

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


% interaction to select move
choose_move(GameState-player1-_-Variant,Level, Move):-
    get_move(Move).
choose_move(GameState-player2-_-Variant,Level, Move):-
    get_move(Move).
    
value(GameState, Player, Value):-
    player_piece(red,Player),
    ratio_surrounding_color(GameState, red, NumRed),
    ratio_surrounding_color(GameState, blue, NumBlue),
    Value is NumRed - NumBlue.

value(GameState, Player, Value):-
    player_piece(blue,Player),
    ratio_surrounding_color(GameState, red, NumRed),
    ratio_surrounding_color(GameState, blue, NumBlue),
    Value is NumBlue - NumRed.

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

valid_moves(GameState-Player-_-Variant, Moves) :-
    findall(Move, (
        within_range(Move,GameState),
        move(GameState-Player-_-Variant, Move, NewState)  % No trailing comma here!
    ), Moves).

choose_move(GameState-computer1_easy-_-Variant,1, Move) :-
    valid_moves(GameState-computer1_easy-_-Variant, Moves),
    random_select(Move, Moves, _Rest),
    inverse_transform_move(Move, TransformedMove),
    nl, write('Red Computer (random) chose move: '), write(TransformedMove), nl.

choose_move(GameState-computer1_hard-_-Variant,2, Move):-
    valid_moves(GameState-computer1_hard-_-Variant, Moves),
    setof(Value, NewState^Mv^( member(Mv, Moves),
        move(GameState-computer1_hard-_-Variant, Mv, NewState),
        value(NewState, computer1_hard, Value) ), [V|_]),
    findall(Mv, NewState^( member(Mv, Moves),
        move(GameState-computer1_hard-_-Variant, Mv, NewState),
        value(NewState, computer1_hard, V) ), GoodMoves),
    random_select(Move,GoodMoves,_Rest),
    inverse_transform_move(Move, TransformedMove),
    nl, write('Red Computer (greedy) chose move: '), write(TransformedMove), nl.

choose_move(GameState-computer2_easy-_-Variant,1, Move) :-
    valid_moves(GameState-computer2_easy-_-Variant, Moves),
    random_select(Move, Moves, _Rest),
    inverse_transform_move(Move, TransformedMove),
    nl, write('Blue Computer (random) chose move: '), write(TransformedMove), nl.

choose_move(GameState-computer2_hard-_-Variant,2, Move):-
    valid_moves(GameState-computer2_hard-_-Variant, Moves),
    setof(Value, NewState^Mv^( member(Mv, Moves),
        move(GameState-computer2_hard-_-Variant, Mv, NewState),
        value(NewState, computer2_hard, Value) ), [V|_]),
    findall(Mv, NewState^( member(Mv, Moves),
        move(GameState-computer2_hard-_-Variant, Mv, NewState),
        value(NewState, computer2_hard, V) ), GoodMoves),
    random_select(Move,GoodMoves,_Rest),
    inverse_transform_move(Move, TransformedMove),
    nl, write('Blue Computer (greedy) chose move: '), write(TransformedMove), nl.

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


get_move(Move) :-
    nl, write('Enter move (e.g., b1-c3): '), nl,
    catch(read(InputMove), _, (nl, write('Read error. This may cause the next reads to fail.'), nl, fail)),
    validate_move_format(InputMove),
    transform_move(InputMove, TransformedMove),
    Move = TransformedMove.

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

