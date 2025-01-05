:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- consult('move.pl').
:- consult('interface.pl').
:- consult('board.pl').
:- consult('player.pl').


/*

play/0 
Initial predicate that gives access to the game menu where the user can choose the Game Type, Difficulty Level, Board Size
It calls the game loop predicate game_cycle

*/

play:-
    get_game_mode(GameMode),
    get_board_size(Size),
    initial_state(GameMode-Size, Board-Player-NextPlayer-Variant),
    display_game(Board-Player-NextPlayer-Variant),
    game_cycle(Board-Player-NextPlayer-Variant, GameMode).



/*

initial_state(+GameConfig, -GameState) Recieves the GameConfig -> GameMode-Size and returns Board -> Board-Player-NextPlayer-Variant

The Board  has the following:

Board -> Matrix that represents the board
Player -> Current Player who will make move
NextPlayer -> Next Player
Variant -> Blackstone has 3 variants either Normal,  MEDIUM CHURN or HIGH CHURN 

*/

initial_state(GameMode-Size, Board-Player-NextPlayer-Variant):-
    initial_board(Size,Board),
    initial_players(GameMode, Player,NextPlayer),
    get_variant(Variant).

/*

display_game(+GameState) takes the current state and displays it visually

Displays the Color of the current player
Displays the board as a Matrix using numbers for rows and letters for columns


*/

display_game(Board-Player-_-_):-
    display_player(Player),
    display_board(Board).


/*

game_cycle(+GameState,+GameMode) defines the actions to be taken in a given turn 

- checks if the game is over
- if it is not gets player/computer move
- makes the move
- display new state
- goes to new iteration with the new state

*/
game_cycle(Board-Player-_-_,_):-
    game_over(Board-Player-_-_, Winner), !,
    congratulate(Winner).
game_cycle(Board-Player-NextPlayer-Variant,GameMode):-
    level_of_ai(Player, Level),
    choose_move(Board-Player-NextPlayer-Variant,Level, Move),
    move(Board-Player-NextPlayer-Variant, Move, NewGameState),
    display_game(NewGameState),
    game_cycle(NewGameState,GameMode).

game_cycle(Board-Player-NextPlayer-Variant,GameMode):-
    display_game(Board-Player-NextPlayer-Variant),
    game_cycle(Board-Player-NextPlayer-Variant,GameMode).

/*

move(+GameState, +Move, -NewGameState) given the current game state and a move C1-L1-C2-L2 returns the resulting state

- using check_move checks if the move can be taken
- gets the piece that the user played using get_piece/3
- sets the piece in the position of the piece played to black using set_piece/4
- sets the piece in the new position to the piece that the user played using set_piece/4
- removes blocked stones and black stones(depending on variant) using remove_blocked_stones/3

*/
move(Board-Player-NextPlayer-Variant, C1-L1-C2-L2, NewBoard-NextPlayer-Player-Variant):-
    check_move(Board-Player, C1-L1-C2-L2),
    get_piece(Board, C1-L1, Piece),
    set_piece(Board, C1-L1, black, TempBoard),
    set_piece(TempBoard, C2-L2, Piece, TempBoard2),
    remove_blocked_stones(Variant,TempBoard2, TempBoard3),
    NewBoard = TempBoard3.


/*

game_over(+GameState, -Winner) takes current Board and gives the Winner Player

The last player with Stones remaining wins
If a move eliminates all stones of both players, the one that made that move is the Winner


*/

game_over(Board-_-_-_, _):-
    there_are_blue_left(Board),
    there_are_red_left(Board),
    fail.

game_over(Board-_-_-_, Winner):-
    there_are_blue_left(Board), 
    \+ there_are_red_left(Board),
    Winner = blue.

game_over(Board-_-_-_, Winner):-
    there_are_red_left(Board), 
    \+ there_are_blue_left(Board),
    Winner = red.

game_over(Board-Player-_-_, Winner):-
    \+ there_are_red_left(Board), 
    \+ there_are_blue_left(Board),
    player_piece(Winner,Player).


/*

choose_move(+GameState,+Level, -Move) takes Board and AI Level and return a Move

For Human Player:
Just asks for input

For AI Player:
Gets Valid Moves using valid_moves/2
Level 1 -> chooses a random move using random_select/3
Level 2 -> picks randomly one of the moves with best(smallest) Value

*/

choose_move(_-player1-_-_,_, Move):-
    get_move(Move).
choose_move(_-player2-_-_,_, Move):-
    get_move(Move).

choose_move(Board-Player-_-Variant,1, Move) :-
    valid_moves(Board-Player-_-Variant, Moves),
    random_select(Move, Moves, _Rest),
    inverse_transform_move(Move, TransformedMove),
    nl, display_color(Player) , write(' Computer (random) chose move: '), write(TransformedMove), nl.

choose_move(Board-Player-_-Variant,2, Move):-
    valid_moves(Board-Player-_-Variant, Moves),
    setof(Value, NewBoard^Mv^( member(Mv, Moves),
        move(Board-Player-_-Variant, Mv, NewBoard-_-_-_),
        value(NewBoard-_-_-_, Player, Value) ), [V|_]),
    findall(Mv, NewBoard^( member(Mv, Moves),
        move(Board-Player-_-Variant, Mv, NewBoard-_-_-_),
        value(NewBoard-_-_-_, Player, V) ), GoodMoves),
    random_select(Move,GoodMoves,_Rest),
    inverse_transform_move(Move, TransformedMove),
    nl, display_color(Player), write(' Computer (greedy) chose move: '), write(TransformedMove), write(' with value: '), write(V), nl.


/*

value(+GameState, +Player, -Value) Given a state and a player gives the Value of that State to the Player

Value is given by:
    -MyColorRatio + OtherColorRatio
    ColorRatio is Sum(pieceOfColor1*space1 + pieceOfColor2*space2 ...)
    spacei = Div(numOfEmptyNeighbours, numOfTotalNeighbours)

Basically a move is good if it increases the number of your pieces that have space to move and are far away from being killed and it decreases the number and freedom of enemy pieces

*/
value(Board-_-_-_, Player, Value):-
    player_piece(red,Player),
    ratio_surrounding_color(Board, red, NumRed),
    ratio_surrounding_color(Board, blue, NumBlue),
    piece_difference(Board, red, blue, Difference),
    Value is NumRed - NumBlue - 2 * Difference.

value(Board-_-_-_, Player, Value):-
    player_piece(blue,Player),
    ratio_surrounding_color(Board, red, NumRed),
    ratio_surrounding_color(Board, blue, NumBlue),
    piece_difference(Board, blue, red, Difference),
    Value is NumBlue - NumRed - 2 * Difference.

/*

valid_moves(+GameState, -ListOfMoves) Given a GameState the the valid moves the player can make

*/
valid_moves(Board-Player-_-_, Moves) :-
    findall(Move, (
        generate_moves(Board, Player, Move),
        check_move(Board-Player, Move)
    ), Moves).


/*

Test States

*/

%use get_initial_state(State).
get_initial_state(State):-
    State = 
    [[empty,empty,red,empty,red,empty],
    [blue,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,blue],
    [blue,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,blue],
    [empty,red,empty,red,empty,empty]]-player1-player2-1.

%move that kills blue piece is a7-a8.
%display the state get_blue_piece_surrounded_v1(State),display_game(State).
%use get_blue_piece_surrounded_v1(State),move(State,1-7-1-8,NewState),display_game(NewState).
get_blue_piece_surrounded_v1(State):-
    State =
    [[empty,empty,red,empty,red,empty,red,empty],
    [blue,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,blue],
    [blue,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,blue],
    [black,red,empty,empty,empty,empty,empty,empty],
    [blue,black,empty,empty,empty,empty,empty,blue],
    [empty,black,empty,red,empty,red,empty,empty]]-player2-player1-1.

%use get_blue_piece_surrounded_v2(State),move(State,1-7-1-8,NewState),display_game(NewState).
get_blue_piece_surrounded_v2(State):-
    State =
    [[empty,empty,red,empty,red,empty,red,empty],
    [blue,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,blue],
    [blue,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,blue],
    [black,red,empty,empty,empty,empty,empty,empty],
    [blue,black,empty,empty,empty,empty,empty,blue],
    [empty,black,empty,red,empty,red,empty,empty]]-player2-player1-2.

%use get_blue_piece_surrounded_v3(State),move(State,1-7-1-8,NewState),display_game(NewState).
get_blue_piece_surrounded_v3(State):-
    State =
    [[empty,empty,red,empty,red,empty,red,empty],
    [blue,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,blue],
    [blue,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,blue],
    [black,red,empty,empty,empty,empty,empty,empty],
    [blue,black,empty,empty,empty,empty,empty,blue],
    [empty,black,empty,red,empty,red,empty,empty]]-player2-player1-3.

%use get_intermediate_state(State)
get_intermediate_state(State):-
    State = 
    [[red,empty,black,empty,black,empty,black,empty],
    [blue,black,black,empty,blue,red,blue,empty],
    [empty,empty,empty,empty,black,empty,black,black],
    [black,red,blue,black,black,black,black,empty],
    [empty,empty,black,red,empty,black,empty,black],
    [black,black,blue,black,black,black,empty,black],
    [empty,black,black,black,empty,black,blue,black],
    [empty,red,empty,black,empty,black,empty,red]]-computer2_hard-computer1_hard-2.

%use get_end_state(State),move(State,4-4-3-4,FState),display_game(FState),game_over(FState,Winner).
get_end_state(State):-
    State = 
    [[empty,black,black,empty,empty,empty],
    [black,empty,empty,red,empty,empty],
    [empty,black,black,black,black,empty],
    [black,black,empty,blue,black,black],
    [empty,black,black,black,black,black],
    [black,black,empty,black,empty,red]]-computer2_hard-computer1_hard-2.

%use get_end_draw_state(State), game_over(State,Winner).
get_end_draw_state(State):-
    State = 
    [[empty,black,black,empty,empty,empty],
    [black,empty,empty,empty,empty,empty],
    [empty,black,black,black,black,empty],
    [black,black,empty,empty,black,black],
    [empty,black,black,black,black,black],
    [black,black,empty,black,empty,empty]]-computer2_hard-computer1_hard-2.