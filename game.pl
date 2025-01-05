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
    initial_state(GameMode-Size, GameState-Player-NextPlayer-Variant),
    display_game(GameState-Player-NextPlayer-Variant),
    game_cycle(GameState-Player-NextPlayer-Variant, GameMode).



/*

initial_state(+GameConfig, -GameState) Recieves the GameConfig -> GameMode-Size and returns GameState -> Board-Player-NextPlayer-Variant

The GameState  has the following:

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

display_game(GameState-Player-_-_):-
    display_player(Player),
    display_board(GameState).


/*

game_cycle(+GameState,+GameMode) defines the actions to be taken in a given turn 

- checks if the game is over
- if it is not gets player/computer move
- makes the move
- display new state
- goes to new iteration with the new state

*/
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

/*

move(+GameState, +Move, -NewGameState) given the current game state and a move C1-L1-C2-L2 returns the resulting state

- using check_move checks if the move can be taken
- gets the piece that the user played using get_piece/3
- sets the piece in the position of the piece played to black using set_piece/4
- sets the piece in the new position to the piece that the user played using set_piece/4
- removes blocked stones and black stones(depending on variant) using remove_blocked_stones/3

*/
move(GameState-Player-_-Variant, C1-L1-C2-L2, NewGameState):-
    check_move(GameState-Player, C1-L1-C2-L2),
    get_piece(GameState, C1-L1, Piece),
    set_piece(GameState, C1-L1, black, TempGameState),
    set_piece(TempGameState, C2-L2, Piece, TempGameState2),
    remove_blocked_stones(Variant,TempGameState2, TempGameState3),
    NewGameState = TempGameState3.
move(GameState-player1-_-Variant, C1-L1-C2-L2, NewGameState):-
    nl, write(' Invalid Move!'), nl,
    fail.
move(GameState-player2-_-Variant, C1-L1-C2-L2, NewGameState):-
    nl, write('Invalid Move!'), nl,
    fail.

/*

game_over(+GameState, -Winner) takes current gamestate and gives the Winner Player

The last player with Stones remaining wins
If a move eliminates all stones of both players, the one that made that move is the Winner


*/

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
    player_piece(Winner,Player).


/*

choose_move(+GameState,+Level, -Move) takes GameState and AI Level and return a Move

For Human Player:
Just asks for input

For AI Player:
Gets Valid Moves using valid_moves/2
Level 1 -> chooses a random move using random_select/3
Level 2 -> picks randomly one of the moves with best(smallest) Value

*/

choose_move(GameState-player1-_-Variant,Level, Move):-
    get_move(Move).
choose_move(GameState-player2-_-Variant,Level, Move):-
    get_move(Move).

choose_move(GameState-Player-_-Variant,1, Move) :-
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
    nl, display_color(Player), write(' Computer (greedy) chose move: '), write(TransformedMove), write(' with value: '), write(V), nl.


/*

value(+GameState, +Player, -Value) Given a state and a player gives the Value of that State to the Player

Value is given by:
    -MyColorRatio + OtherColorRatio
    ColorRatio is Sum(pieceOfColor1*space1 + pieceOfColor2*space2 ...)
    spacei = Div(numOfEmptyNeighbours, numOfTotalNeighbours)

Basically a move is good if it increases the number of your pieces that have space to move and are far away from being killed and it decreases the number and freedom of enemy pieces

*/
value(GameState, Player, Value):-
    player_piece(red,Player),
    ratio_surrounding_color(GameState, red, NumRed),
    ratio_surrounding_color(GameState, blue, NumBlue),
    piece_difference(GameState, red, blue, Difference),
    Value is NumRed - NumBlue - 2 * Difference.

value(GameState, Player, Value):-
    player_piece(blue,Player),
    ratio_surrounding_color(GameState, red, NumRed),
    ratio_surrounding_color(GameState, blue, NumBlue),
    piece_difference(GameState, blue, red, Difference),
    Value is NumBlue - NumRed - 2 * Difference.

