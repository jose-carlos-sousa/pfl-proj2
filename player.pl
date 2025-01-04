
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


player_has_piece(GameState-Player, C1-L1-_-_):-
    get_piece(GameState, C1-L1, Piece),
    player_piece(Piece, Player).

    
player_piece(red, player1).
player_piece(red, computer1_easy).
player_piece(red, computer1_hard).
player_piece(blue, player2).
player_piece(blue, computer2_easy).
player_piece(blue, computer2_hard).