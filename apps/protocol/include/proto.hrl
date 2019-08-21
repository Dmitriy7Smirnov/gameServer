-define(service_auth, 1).
-define(cs_login, 1).
-define(sc_login_reply, 2).
-define(cs_logout, 3).
-define(sc_logout_reply, 4).

-define(service_lobby, 2).
-define(cs_find_opponent,1).
-define(sc_find_opponent_reply, 2).

-define(service_match, 3).
-define(cs_start_battle, 1).
-define(sc_start_battle_reply, 2).
-define(cs_attack, 3).
-define(sc_attack_reply, 4).

-define(error, 0).
-define(ok, 1).

-record(player, {
    hp = 0,
    hp_max = 0,
    id = 0,
    username = list_to_binary("unknown"),
    socket,
    pid,
    opponent_pid
}).
