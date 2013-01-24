%%%
%% auth_harbor_test.erl
%%

-include_lib("eunit/include/eunit.hrl").

-module(auth_harbor_test).

starttest() ->
     application:start(inets), %% may already be loaded
     application:start(ejabberd),
    ok.

stoptest() ->
     application:stop(inets),
     application:stop(ejabberd),
    ok.

ejabberd_auth_harbor_check_password_test() ->
    starttest(),
    false = ejabberd_auth_harbor:check_password("tofu","localhost","test"),
    stoptest().
