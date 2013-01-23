-module(harbor_odbc_queries).
-author("lkosak@gmail.com").

-export([get_user_auth_credentials/2]).

-include("ejabberd.hrl").


get_user_auth_credentials(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select password_digest,auth_token from users "
       "where username='", Username, "';"]).

