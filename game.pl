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
    get_ai_level(X),
    get_ai_player(X,second,Computer).


initial_players(3,Computer,player2):-
    get_ai_level(X),
    get_ai_player(X,first,Computer).

initial_players(4,Computer1,Computer2):-
    get_ai_level(X),
    get_ai_player(X,first,Computer1),
    get_ai_player(X,second,Computer2).
    

level_of_ai(player1,na).
level_of_ai(player2,na).
level_of_ai(computer1_easy,1).
level_of_ai(computer1_hard,2).
level_of_ai(computer2_easy,1).
level_of_ai(computer2_hard,2).



initial_board(Size, Board) :-
    findall(Row,(between(1,Size,X), initial_board_row(X,Size,Row)),Board). 

initial_board_row(Row, Size, BoardRow) :-
    findall(Piece,(between(1,Size,X),initial_board_cell(Row,X,Piece,Size)),BoardRow).
    
initial_board_cell(1,Y, red,Size) :-
    Y > 1,
    Y < Size,
    Y mod 2 =:= 1,!.

initial_board_cell(Size,Y, red,Size) :-
    Y > 1,
    Y < Size,
    Y mod 2 =:= 0,!.

initial_board_cell(X,1, blue,Size) :-
    X > 1,
    X < Size,
    X mod 2 =:= 0,!.

initial_board_cell(X,Size, blue,Size) :-
    X > 1,
    X < Size,
    X mod 2 =:= 1,!.

initial_board_cell(_,_, empty,_).


game_cycle(GameState-Player-_-_,GameMode):-
    game_over(GameState-Player-_-_, Winner), !,
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
    check_move(GameState-Player, C1-L1-C2-L2),!,
    get_piece(GameState, C1-L1, Piece),
    set_piece(GameState, C1-L1, black, TempGameState),
    set_piece(TempGameState, C2-L2, Piece, TempGameState2),
    remove_blocked_stones(Variant,TempGameState2, TempGameState3),
    NewGameState = TempGameState3.



check_move(GameState-Player, Move):-
    within_range(Move,GameState),
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

game_over(GameState-Player-_-_, Winner):-
    there_are_blue_left(GameState),
    there_are_red_left(GameState),
    fail.

game_over(GameState-Player-_-_, Winner):-
    there_are_blue_left(GameState), 
    \+ there_are_red_left(GameState),
    Winner = blue.

game_over(GameState-Player-_-_, Winner):-
    there_are_red_left(GameState), 
    \+ there_are_blue_left(GameState),
    Winner = red.

game_over(GameState-Player-_-_, Winner):-
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

choose_move(GameState-Player-_-Variant,1, Move) :-
    write(GameState-Player-_-Variant),
    valid_moves(GameState-Player-_-Variant, Moves),
    random_select(Move, Moves, _Rest),
    inverse_transform_move(Move, TransformedMove),
    nl, display_color(Player) , write(' Computer (random) chose move: '), write(TransformedMove), nl.

choose_move(GameState-Player-_-Variant,2, Move):-
    valid_moves(GameState-Player-_-Variant, Moves),
    setof(Value, NewState^Mv^( member(Mv, Moves),
        move(GameState-Player-_-Variant, Mv, NewState),
        value(NewState, Player, Value) ), [V|_]),
    findall(Mv, NewState^( member(Mv, Moves),
        move(GameState-Player-_-Variant, Mv, NewState),
        value(NewState, Player, V) ), GoodMoves),
    random_select(Move,GoodMoves,_Rest),
    inverse_transform_move(Move, TransformedMove),
    nl, display_color(Player), write(' Computer (greedy) chose move: '), write(TransformedMove), nl.




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
        move(GameState-Player-_-Variant, Move, _)
        ,nl , write(Move),nl  % No trailing comma here!
    ), Moves).

