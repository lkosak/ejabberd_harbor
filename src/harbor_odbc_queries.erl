-module(harbor_odbc_queries).
-author("lkosak@gmail.com").

-export([get_auth_token/2]).

-include("ejabberd.hrl").


get_auth_token(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select auth_token from users "
       "where username='", Username, "';"]).

