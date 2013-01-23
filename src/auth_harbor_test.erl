%%%
%% auth_harbor_test.erl
%%

-include_lib("eunit/include/eunit.hrl").

-module(auth_harbor_test).

starttest() ->
     application:start(inets), %% may already be loaded
     application:start(ejabberd),
     ejabberd_auth:init([]),
    ok.

stoptest() ->
     application:stop(inets),
     application:stop(ejabberd),
    ok.

%% ejabberd_auth_couchdb_register_test() ->
%%     starttest(),
%%     ejabberd_auth_couchdb:remove_user("tofu","localhost"),
%%     {atomic, ok} = ejabberd_auth_couchdb:try_register("tofu","localhost","test"),
%%     true = ejabberd_auth_couchdb:is_user_exists("tofu","localhost"),
%%     stoptest().
%% 
%% 
%% ejabberd_auth_couchdb_set_password_test() ->
%%     starttest(),
%%     case ejabberd_auth_couchdb:is_user_exists("tofu","localhost") of
%% 	true ->
%% 	    ok;
%% 	false ->
%% 	    {atomic, ok} = ejabberd_auth_couchdb:try_register("tofu","localhost","test")
%%     end,
%%     ok = ejabberd_auth_couchdb:set_password("tofu","localhost","test123"),
%%     true = ejabberd_auth_couchdb:check_password("tofu","localhost","test123"),
%%     stoptest().
%% 
%% ejabberd_auth_couchdb_check_password_test() ->
%%     starttest(),
%%     case ejabberd_auth_couchdb:is_user_exists("tofu","localhost") of
%% 	true ->
%% 	    ok;
%% 	false ->
%% 	    {atomic, ok} = ejabberd_auth_couchdb:try_register("tofu","localhost","test123")
%%     end,
%%     %% test for true and false results
%%     false = ejabberd_auth_couchdb:check_password("tofu","localhost","test"),
%%     true = ejabberd_auth_couchdb:check_password("tofu","localhost","test123"),
%%     stoptest().
    
