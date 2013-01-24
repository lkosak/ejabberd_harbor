%%%----------------------------------------------------------------------
%%% File    : ejabberd_harbor_odbc.erl
%%% Author  : Lou Kosak <lkosak@gmail.com>
%%% Purpose : Authentification via Harbor DB
%%% Created : 22 Jan 2013
%%%----------------------------------------------------------------------

-module(ejabberd_auth_harbor).
-author('lkosak@gmail.com').

%% External exports
-export([start/1,
   set_password/3,
   check_password/3,
   check_password/5,
   try_register/3,
   dirty_get_registered_users/0,
   get_vh_registered_users/1,
   get_vh_registered_users/2,
   get_vh_registered_users_number/1,
   get_vh_registered_users_number/2,
   get_password/2,
   get_password_s/2,
   is_user_exists/2,
   remove_user/2,
   remove_user/3,
   plain_password_required/0,
   store_type/0
  ]).

-include("ejabberd.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) ->
    ?INFO_MSG("Initializing harbor authentication", []),
    ok.

plain_password_required() ->
  false.

store_type() ->
  external.

%% @spec (User, Server, Password) -> true | false | {error, Error}
check_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
  error ->
      ?INFO_MSG("nodeprep failed for user ~p", [User]),
      false;
  LUser ->
      Username = ejabberd_odbc:escape(LUser),
      LServer = jlib:nameprep(Server),
      try harbor_odbc_queries:get_auth_token(LServer, Username) of
    {selected, ["auth_token"], [{Password}]} ->
        Password /= "";
    {selected, ["auth_token"], [{_Invalid}]} ->
      ?INFO_MSG("Invalid password specified for user ~p", [Username]),
        false; %% Password is not correct
    {selected, ["auth_token"], []} ->
        ?INFO_MSG("No account exists with username ~p", [Username]),
        false; %% Account does not exist
    {error, _Error} ->
        false %% Typical error is that table doesn't exist
      catch
    _:_ ->
        false %% Typical error is database not accessible
      end
    end.

%% @spec (User, Server, Password, Digest, DigestGen) -> true | false | {error, Error}
check_password(User, Server, Password, _Digest, _DigestGen) ->
  check_password(User, Server, Password).

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(_User, _Server, _Password) ->
  {error, invalid_jid}.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid}
try_register(_User, _Server, _Password) ->
  {error, not_allowed}. %% figure out how to return a better error response

dirty_get_registered_users() ->
  [].

get_vh_registered_users(_Server) ->
  [].

get_vh_registered_users(_Server, _Opts) ->
  [].

get_vh_registered_users_number(_Server) ->
  0.

get_vh_registered_users_number(_Server, _Opts) ->
  0.

get_password(_User, _Server) ->
  false.

get_password_s(_User, _Server) ->
  "".

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    case jlib:nodeprep(User) of
  error ->
      false;
  LUser ->
      Username = ejabberd_odbc:escape(LUser),
      LServer = jlib:nameprep(Server),
      try harbor_odbc_queries:get_auth_token(LServer, Username) of
    {selected, ["auth_token"], [{_Anything}]} ->
        true; %% Account exists
    {selected, ["auth_token"], []} ->
        false; %% Account does not exist
    {error, Error} ->
        {error, Error} %% Typical error is that table doesn't exist
      catch
    _:B ->
        {error, B} %% Typical error is database not accessible
      end
    end.

%% @spec (User, Server) -> ok | error
%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
remove_user(_User, _Server) ->
  {error, not_allowed}.

%% @spec (User, Server, Password) -> ok | error | not_exists | not_allowed
%% @doc Remove user if the provided password is correct.
remove_user(_User, _Server, _Password) ->
  not_allowed.
