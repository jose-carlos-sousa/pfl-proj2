:- use_module(library(lists)).
:- use_module(library(random)).

:- consult('interface.pl').
initial_state(GameState-player1):-
    initial_board(Board),
    GameState = Board.
initial_board(Board):-
    write('Enter board size (even number greater than 2):'), nl,
    read(Size),
    validate_size(Size, ValidSize),
    init_board(ValidSize, Board).

validate_size(Size, ValidSize) :-
    (   integer(Size),
        Size > 2,
        Size mod 2 =:= 0
    ->  ValidSize = Size
    ;   write('Invalid input. Please enter an even number greater than 2.'), nl,
        read(NewSize),
        validate_size(NewSize, ValidSize)
    ).

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

play_game:-
    initial_state(GameState-Player),
    display_game(GameState-Player),
    game_cycle(GameState-Player).

game_cycle(GameState-Player):-
    game_over(GameState, Winner), !,
    congratulate(Winner).
game_cycle(GameState-Player):-
    choose_move(GameState, Player, Move),
    move(GameState-Player, Move, NewGameState),
    next_player(Player, NextPlayer), % could be done in move/3
    display_game(NewGameState-NextPlayer),
    game_cycle(NewGameState-NextPlayer).

game_cycle(GameState-Player):-
    display_game(GameState-Player),
    game_cycle(GameState-Player).
% basicamente vemos se o move é válido e se vamos buscar a peça , metemos uma preta no sitio dela, e depois metemos a peça no sitio de destino
move(GameState-Player, C1-L1-C2-L2, NewGameState):-
    check_move(GameState-Player, C1-L1-C2-L2),
    get_piece(GameState, C1-L1, Piece),
    set_piece(GameState, C1-L1, black, TempGameState),
    set_piece(TempGameState, C2-L2, Piece, TempGameState2),
    remove_blocked_stones(TempGameState2, TempGameState3),
    NewGameState = TempGameState3.


next_player(player1, player2).
next_player(player2, player1).
check_move(GameState-Player, Move):-
    player_has_piece(GameState-Player, Move),
    valid_queen_move(GameState, Move),
    is_destination_empty(GameState, Move).
player_has_piece(GameState-Player, C1-L1-C2-L2):-
    get_piece(GameState, C1-L1, Piece),
    player_piece(Piece, Player).



set_piece(GameState, C-L, Piece, NewGameState):-
    nth1(L, GameState, Row),
    replace(C, Row, Piece, NewRow),
    replace(L, GameState, NewRow, NewGameState).

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

remove_blocked_stones([H|T], NewGameState):-
    length(H, NumCols),
    length([H|T], NumRows),
    remove_blocked_stones_helper(1-1, NumCols-NumRows, [H|T],[H|T], NewGameState) , !.
% has original board board accumulator and final 
remove_blocked_stones_helper(C-L, C-L, GameState,GameStateAcc, NewGameState):- 
    remove_blocked_stones_piece(GameState,GameStateAcc, C-L, NewGameState).
remove_blocked_stones_helper(C-L, C2-L2, GameState,GameStateAcc, NextGameState):-
    remove_blocked_stones_piece(GameState,GameStateAcc, C-L, NewGameState),
    next_position(C-L, C2-L2, NextC-NextL),
    remove_blocked_stones_helper(NextC-NextL, C2-L2,GameState ,NewGameState, NextGameState).

next_position(C-L, C-L2, NextC-NextL):-
    NextL is L + 1,
    NextC is 1.
next_position(C-L, C2-L2, NextC-NextL):-
    NextC is C + 1,
    NextL is L.
    

% se o atual for empty  eu n mudo
remove_blocked_stones_piece(GameState,GameAcc,C-L, GameAcc):-
    get_piece(GameState, C-L, empty), !.
$ se o atual for black eu n mudo

remove_blocked_stones_piece(GameState,GameAcc,C-L, GameAcc):-
    get_piece(GameState, C-L, black), !.

remove_blocked_stones_piece( GameState,GameAcc,C-L, NewGameState ):-
    get_piece(GameState, C-L, Piece),
    adjacent_stones(GameState, C-L, Stones),
    \+ member(empty, Stones),
    set_piece(GameAcc, C-L, empty, NewBoard1),
    remove_all_black_neightbours(NewBoard1, C-L, NewGameState), !.
% if we gucci lets just chill i mmmmmmmma fell n sei que light alivevevveveve i see forever in ur eyes she smile smileleeeenjewjbalvlqeLVKJEDAS<
remove_blocked_stones_piece( GameState,GameAcc,C-L, GameAcc):-
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
    C1 =:=C2.
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
player_piece(blue, player2).
game_over(GameState, Winner):-
    there_are_blue_left(GameState), 
    there_are_red_left(GameState),
    fail.
game_over(GameState, Winner):-
    there_are_blue_left(GameState), 
    \+ there_are_red_left(GameState),
    Winner = blue.
game_over(GameState, Winner):-
    there_are_red_left(GameState), 
    \+ there_are_blue_left(GameState),
    Winner = red.
game_over(GameState, Winner):-
    \+ there_are_red_left(GameState), 
    \+ there_are_blue_left(GameState),
    Winner = draw.
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
choose_move(GameState, player1, Move):-
    get_move(Move).
choose_move(GameState, player2, Move):-
    get_move(Move).

get_move(Move):-
    write('Enter move: '),nl,
    read(Move).

choose_move(GameState, computer-Level, Move):-
    valid_moves(GameState, ValidMoves),
    choose_move(Level, GameState, ValidMoves, Move).
valid_moves(GameState, Moves):-
    findall(Move, move(GameState, Move, NewState), Moves).
move(GameState, Move, NewState):-
    
% A move is c1-l1-c2-l2
choose_move(1, _GameState, Moves, Move):-
    random_select(Move, Moves, _Rest).
choose_move(2, GameState, Moves, Move):-
    setof(Value-Mv, NewState^( member(Mv, Moves),
    move(GameState, Mv, NewState),
    evaluate_board(NewState, Value) ), [_V-Move|_]).