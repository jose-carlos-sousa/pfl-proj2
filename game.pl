:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).


:- consult('interface.pl').

initial_state(GameMode-Size, Board-Player):-
    initial_board(Size,Board),
    initial_player(GameMode, Player).

initial_player(1, player1).
initial_player(2, player1).
initial_player(3, computer1).
initial_player(4, computer1).

initial_board(Size,Board):-
    init_board(Size, Board).

get_board_size(ValidSize):-
    write('Enter board size (even number greater than 2):'), nl,
    catch(read(Size), _, (write('Read error. This may cause the next reads to fail.'), nl, get_board_size(Size))),
    validate_size(Size, ValidSize).

validate_size(Size, ValidSize) :-
    integer(Size),
    Size > 2,
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

play:-
    get_game_mode(GameMode),
    get_board_size(Size),
    play(GameMode, Size).

% pessoas reais
play(1, Size):-
    initial_state(1-Size, GameState-Player),
    display_game(GameState-Player),
    game_cycle(GameState-Player, _, 1).

% computador vai em segundo
play(2, Size):-
    get_AI_level(Level),
    initial_state(2-Size, GameState-Player),
    display_game(GameState-Player),
    game_cycle(GameState-Player, Level, 2).

% computador vai em primeiro
play(3, Size):-
    get_AI_level(Level),
    initial_state(3-Size, GameState-Player),
    display_game(GameState-Player),
    game_cycle(GameState-Player, Level, 3).

% trabalhar nesta opção depois
play(4, Size):-
    get_AI_level(Level),
    initial_state(4-Size, GameState-Player),
    display_game(GameState-Player),
    game_cycle(GameState-Player, Level, 4).

game_cycle(GameState-Player,Level,GameMode):-
    game_over(GameState, Winner), !,
    congratulate(Winner).
game_cycle(GameState-Player,Level,GameMode):-
    choose_move(GameState-Player,Level, Move),
    move(GameState-Player, Move, NewGameState),
    next_player(GameMode, Player, NextPlayer), % could be done in move/3
    display_game(NewGameState-NextPlayer),
    game_cycle(NewGameState-NextPlayer,Level,GameMode).

game_cycle(GameState-Player,Level,GameMode):-
    display_game(GameState-Player),
    game_cycle(GameState-Player,Level,GameMode).

% basicamente vemos se o move é válido e se vamos buscar a peça , metemos uma preta no sitio dela, e depois metemos a peça no sitio de destino
move(GameState-Player, C1-L1-C2-L2, NewGameState):-
    check_move(GameState-Player, C1-L1-C2-L2),
    !,
    get_piece(GameState, C1-L1, Piece),
    set_piece(GameState, C1-L1, black, TempGameState),
    set_piece(TempGameState, C2-L2, Piece, TempGameState2),
    remove_blocked_stones(TempGameState2, TempGameState3),
    NewGameState = TempGameState3.

move(GameState-Player, _, GameState):- fail.

next_player(1,player1, player2).
next_player(1,player2, player1).
next_player(2,player1, computer2).
next_player(2,computer2, player1).
next_player(3,computer1, player2).
next_player(3,player2, computer1).
next_player(4,computer1, computer2).
next_player(4,computer2, computer1).

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
player_piece(red, computer1).
player_piece(blue, player2).
player_piece(blue, computer2).
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
choose_move(GameState-player1,Level, Move):-
    get_move(Move).
choose_move(GameState-player2,Level, Move):-
    get_move(Move).


within_range(Move,GameState) :-
    length(GameState, Size),
    Size1 is Size + 1,
    between(1, Size1, FromX),
    between(1, Size1, FromY),
    between(1, Size1, ToX),
    between(1, Size1, ToY),
    Move = FromX-FromY-ToX-ToY.
    
evaluate_board(GameState, Value, red):-
    ratio_surrounding_color(GameState, red, NumRed),
    ratio_surrounding_color(GameState, blue, NumBlue),
    Value is NumRed - NumBlue.

evaluate_board(GameState, Value, blue):-
    ratio_surrounding_color(GameState, red, NumRed),
    ratio_surrounding_color(GameState, blue, NumBlue),
    Value is NumBlue - NumRed.

ratio_surrounding_color(GameState, Color, Num):-
    findall(Stones, (
            get_piece(GameState, C-L, Color),
            adjacent_stones(GameState, C-L, Stones)
    ), StonesList),
    findall(EmptyStone, (
            member(Stones, StonesList),
            member(EmptyStone, Stones),
            EmptyStone == empty
    ), EmptyStones),
    findall(NonEmptyStone, (
            member(Stones, StonesList),
            member(NonEmptyStone, Stones),
            NonEmptyStone \= empty
    ), NonEmptyStones),
    length(NonEmptyStones, NonEmptyStoneLen),
    length(EmptyStones, EmptyStonesLen),
    Num is NonEmptyStoneLen - EmptyStonesLen.

    
choose_move(GameState-computer1,1, Move) :-
    valid_moves(GameState-computer1, Moves),
    random_select(Move, Moves, _Rest),
    nl, write('Computer1 (random) chose move: '), write(Move), nl.
choose_move(GameState-computer1,2, Move):-
    valid_moves(GameState-computer1, Moves),
    setof(Value-Mv, NewState^( member(Mv, Moves),
        move(GameState-computer1, Mv, NewState),
        evaluate_board(NewState, Value,red) ), [_V-Move|_]).
    nl, write('Computer1 (greedy) chose move: '), write(Move), nl.
choose_move(GameState-computer2,1, Move) :-
    valid_moves(GameState-computer2, Moves),
    random_select(Move, Moves, _Rest),
    nl, write('Computer2 (random) chose move: '), write(Move), nl.
choose_move(GameState-computer2,2, Move):-
    valid_moves(GameState-computer2, Moves),
    setof(Value-Mv, NewState^( member(Mv, Moves),
        move(GameState-computer2, Mv, NewState),
        evaluate_board(NewState, Value,blue) ), [_V-Move|_]),
    nl, write('Computer2 (greedy) chose move: '), write(Move), nl.


valid_moves(GameState-Player, Moves) :-
    findall(Move, (
        within_range(Move,GameState),
        move(GameState-Player, Move, NewState)  % No trailing comma here!
    ), Moves).

get_move(Move):-
    write('Enter move (x1-y1-x2-y2): '), nl,
    catch(read(InputMove), _, (nl, write('Read error. This may cause the next reads to fail.'), nl, fail)),
    validate_move_format(InputMove, ValidMove),
    Move = ValidMove.

validate_move_format(Move, ValidMove) :-
    is_valid_format(Move),
    !,
    ValidMove = Move.

validate_move_format(_, ValidMove) :-
    write('Invalid move format. Please enter move in format x1-y1-x2-y2.'), nl,
    get_move(ValidMove).

is_valid_format(X1-Y1-X2-Y2) :-
    integer(X1), integer(Y1), integer(X2), integer(Y2).

