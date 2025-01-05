/*

initial_players(+GameMode, -Player1,-Player2)
Given gamemode gives player and nextplayer

*/
initial_players(1, player1,player2):- !.
initial_players(2,player1,Computer):-
    get_second_ai_level(X),
    get_ai_player(X,second,Computer).


initial_players(3,Computer,player2):-
    get_first_ai_level(X),
    get_ai_player(X,first,Computer).

initial_players(4,Computer1,Computer2):-
    get_first_ai_level(X),
    get_second_ai_level(Y),
    get_ai_player(X,first,Computer1),
    get_ai_player(Y,second,Computer2).
    
%establishes connection between AI level turn and player
get_ai_player(1,second,computer2_easy).
get_ai_player(2,second,computer2_hard).
get_ai_player(1,first,computer1_easy).
get_ai_player(2,first,computer1_hard).

%gives ai level of each player
level_of_ai(player1,na).
level_of_ai(player2,na).
level_of_ai(computer1_easy,1).
level_of_ai(computer1_hard,2).
level_of_ai(computer2_easy,1).
level_of_ai(computer2_hard,2).

%checks if player has a given piece
player_has_piece(Board-Player, C1-L1-_-_):-
    get_piece(Board, C1-L1, Piece),
    player_piece(Piece, Player).

%gives color of each player    
player_piece(red, player1).
player_piece(red, computer1_easy).
player_piece(red, computer1_hard).
player_piece(blue, player2).
player_piece(blue, computer2_easy).
player_piece(blue, computer2_hard).