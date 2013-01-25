-module(mod_shared_roster_harbor).
-author('lkosak@gmail.com').

-behaviour(gen_mod).

-export([start/2, stop/1,
  item_to_xml/1,
  get_user_roster/2,
  get_subscription_lists/3,
  get_jid_info/4,
  process_item/2,
  in_subscription/6,
  out_subscription/4,
  user_available/1,
  unset_presence/4,
  list_groups/1,
  create_group/2,
  create_group/3,
  delete_group/2,
  get_group_opts/2,
  set_group_opts/3,
  get_group_users/2,
  get_group_explicit_users/2,
  is_user_in_group/3,
  add_user_to_group/3,
  remove_user_from_group/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").

-record(sr_group, {group_host, opts}).
-record(sr_user, {us, group_host}).

start(Host, _Opts) ->
  ejabberd_hooks:add(roster_get, Host,
    ?MODULE, get_user_roster, 70),
  ejabberd_hooks:add(roster_in_subscription, Host,
    ?MODULE, in_subscription, 30),
  ejabberd_hooks:add(roster_out_subscription, Host,
    ?MODULE, out_subscription, 30),
  ejabberd_hooks:add(roster_get_subscription_lists, Host,
    ?MODULE, get_subscription_lists, 70),
  ejabberd_hooks:add(roster_get_jid_info, Host,
    ?MODULE, get_jid_info, 70),
  ejabberd_hooks:add(roster_process_item, Host,
    ?MODULE, process_item, 50),
  ejabberd_hooks:add(user_available_hook, Host,
    ?MODULE, user_available, 50),
  ejabberd_hooks:add(unset_presence_hook, Host,
    ?MODULE, unset_presence, 50).

stop(Host) ->
  ejabberd_hooks:delete(roster_get, Host,
    ?MODULE, get_user_roster, 70),
  ejabberd_hooks:delete(roster_in_subscription, Host,
    ?MODULE, in_subscription, 30),
  ejabberd_hooks:delete(roster_out_subscription, Host,
    ?MODULE, out_subscription, 30),
  ejabberd_hooks:delete(roster_get_subscription_lists, Host,
    ?MODULE, get_subscription_lists, 70),
  ejabberd_hooks:delete(roster_get_jid_info, Host,
    ?MODULE, get_jid_info, 70),
  ejabberd_hooks:delete(roster_process_item, Host,
    ?MODULE, process_item, 50),
  ejabberd_hooks:delete(user_available_hook, Host,
    ?MODULE, user_available, 50),
  ejabberd_hooks:delete(unset_presence_hook, Host,
    ?MODULE, unset_presence, 50).

get_user_roster(Items, US) ->
    {U, S} = US,
    DisplayedGroups = get_user_displayed_groups(US),
    %% Get shared roster users in all groups and remove self:
    SRUsers =
  lists:foldl(
    fun(Group, Acc1) ->
      GroupName = get_group_name(S, Group),
      lists:foldl(
        fun(User, Acc2) ->
          if User == US -> Acc2;
             true -> dict:append(User,
               GroupName,
               Acc2)
          end
        end, Acc1, get_group_users(S, Group))
    end, dict:new(), DisplayedGroups),

    %% If partially subscribed users are also in shared roster, show them as
    %% totally subscribed:
    {NewItems1, SRUsersRest} =
  lists:mapfoldl(
    fun(Item, SRUsers1) ->
      {_, _, {U1, S1, _}} = Item#roster.usj,
      US1 = {U1, S1},
      case dict:find(US1, SRUsers1) of
          {ok, _GroupNames} ->
        {Item#roster{subscription = both, ask = none},
         dict:erase(US1, SRUsers1)};
          error ->
        {Item, SRUsers1}
      end
    end, SRUsers, Items),

    %% Export items in roster format:
    ModVcard = get_vcard_module(S),
    SRItems = [#roster{usj = {U, S, {U1, S1, ""}},
           us = US,
           jid = {U1, S1, ""},
           name = get_rosteritem_name(ModVcard, U1, S1),
           subscription = both,
           ask = none,
           groups = GroupNames} ||
      {{U1, S1}, GroupNames} <- dict:to_list(SRUsersRest)],
    SRItems ++ NewItems1.

get_vcard_module(Server) ->
    Modules = gen_mod:loaded_modules(Server),
    [M || M <- Modules,
    (M == mod_vcard) or (M == mod_vcard_ldap)].

get_rosteritem_name([], _, _) ->
    "";
get_rosteritem_name([ModVcard], U, S) ->
    From = jlib:make_jid("", S, ?MODULE),
    To = jlib:make_jid(U, S, ""),
    IQ = {iq,"",get,"vcard-temp","",
    {xmlelement,"vCard",[{"xmlns","vcard-temp"}],[]}},
    IQ_Vcard = ModVcard:process_sm_iq(From, To, IQ),
    try get_rosteritem_name_vcard(IQ_Vcard#iq.sub_el)
    catch E1:E2 ->
      ?ERROR_MSG("Error ~p found when trying to get the vCard of ~s@~s "
           "in ~p:~n ~p", [E1, U, S, ModVcard, E2]),
      ""
    end.

get_rosteritem_name_vcard([]) ->
    "";
get_rosteritem_name_vcard([Vcard]) ->
    case xml:get_path_s(Vcard, [{elem, "NICKNAME"}, cdata]) of
  "" -> xml:get_path_s(Vcard, [{elem, "FN"}, cdata]);
  Nickname -> Nickname
    end.

%% This function rewrites the roster entries when moving or renaming
%% them in the user contact list.
process_item(RosterItem, Host) ->
    USFrom = {UserFrom, ServerFrom} = RosterItem#roster.us,
    {UserTo, ServerTo, ResourceTo} = RosterItem#roster.jid,
    NameTo = RosterItem#roster.name,
    USTo = {UserTo, ServerTo},
    DisplayedGroups = get_user_displayed_groups(USFrom),
    CommonGroups = lists:filter(fun(Group) ->
          is_user_in_group(USTo, Group, Host)
        end, DisplayedGroups),
    case CommonGroups of
  [] -> RosterItem;
  %% Roster item cannot be removed: We simply reset the original groups:
  _ when RosterItem#roster.subscription == remove ->
      GroupNames = lists:map(fun(Group) ->
             get_group_name(Host, Group)
           end, CommonGroups),
      RosterItem#roster{subscription = both, ask = none,
            groups=[GroupNames]};
  %% Both users have at least a common shared group,
  %% So each user can see the other
  _ ->
      %% Check if the list of groups of the new roster item
      %% include at least a new one
      case lists:subtract(RosterItem#roster.groups, CommonGroups) of
                %% If it doesn't, then remove this user from any
                %% existing roster groups.
    [] ->
                    %% Remove pending subscription by setting it
                    %% unsubscribed.

                    %% Remove pending out subscription
                    mod_roster:out_subscription(UserTo, ServerTo,
                                         jlib:make_jid(UserFrom, ServerFrom, ""),
                                         unsubscribe),

                    %% Remove pending in subscription
                    mod_roster:in_subscription(aaaa, UserFrom, ServerFrom,
                                        jlib:make_jid(UserTo, ServerTo, ""),
                                        unsubscribe, ""),

                    %% But we're still subscribed, so respond as such.
        RosterItem#roster{subscription = both, ask = none};
    %% If so, it means the user wants to add that contact
    %% to his personal roster
    PersonalGroups ->
        %% Store roster items in From and To rosters
        set_new_rosteritems(UserFrom, ServerFrom,
          UserTo, ServerTo, ResourceTo, NameTo,
          PersonalGroups)
      end
    end.

build_roster_record(User1, Server1, User2, Server2, Name2, Groups) ->
    USR2 = {User2, Server2, ""},
    #roster{usj = {User1, Server1, USR2},
      us = {User1, Server1},
      jid = USR2,
      name = Name2,
      subscription = both,
      ask = none,
      groups = Groups
     }.

set_new_rosteritems(UserFrom, ServerFrom,
        UserTo, ServerTo, ResourceTo, NameTo, GroupsFrom) ->
    RIFrom = build_roster_record(UserFrom, ServerFrom,
         UserTo, ServerTo, NameTo, GroupsFrom),
    set_item(UserFrom, ServerFrom, ResourceTo, RIFrom),
    JIDTo = jlib:make_jid(UserTo, ServerTo, ""),

    JIDFrom = jlib:make_jid(UserFrom, ServerFrom, ""),
    RITo = build_roster_record(UserTo, ServerTo,
             UserFrom, ServerFrom, UserFrom,[]),
    set_item(UserTo, ServerTo, "", RITo),

    %% From requests
    mod_roster:out_subscription(UserFrom, ServerFrom, JIDTo, subscribe),
    mod_roster:in_subscription(aaa, UserTo, ServerTo, JIDFrom, subscribe, ""),

    %% To accepts
    mod_roster:out_subscription(UserTo, ServerTo, JIDFrom, subscribed),
    mod_roster:in_subscription(aaa, UserFrom, ServerFrom, JIDTo, subscribed, ""),

    %% To requests
    mod_roster:out_subscription(UserTo, ServerTo, JIDFrom, subscribe),
    mod_roster:in_subscription(aaa, UserFrom, ServerFrom, JIDTo, subscribe, ""),

    %% From accepts
    mod_roster:out_subscription(UserFrom, ServerFrom, JIDTo, subscribed),
    mod_roster:in_subscription(aaa, UserTo, ServerTo, JIDFrom, subscribed, ""),

    RIFrom.

set_item(User, Server, Resource, Item) ->
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
    id = "push" ++ randoms:get_string(),
    sub_el = [{xmlelement, "query",
         [{"xmlns", ?NS_ROSTER}],
         [mod_roster:item_to_xml(Item)]}]},
    ejabberd_router:route(
      jlib:make_jid(User, Server, Resource),
      jlib:make_jid("", Server, ""),
      jlib:iq_to_xml(ResIQ)).


get_subscription_lists({F, T}, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers =
  lists:usort(
    lists:flatmap(
      fun(Group) ->
        get_group_users(LServer, Group)
      end, DisplayedGroups)),
    SRJIDs = [{U1, S1, ""} || {U1, S1} <- SRUsers],
    {lists:usort(SRJIDs ++ F), lists:usort(SRJIDs ++ T)}.

get_jid_info({Subscription, Groups}, User, Server, JID) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} = jlib:jid_tolower(JID),
    US1 = {U1, S1},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers =
  lists:foldl(
    fun(Group, Acc1) ->
      lists:foldl(
        fun(User1, Acc2) ->
          dict:append(
            User1, get_group_name(LServer, Group), Acc2)
        end, Acc1, get_group_users(LServer, Group))
    end, dict:new(), DisplayedGroups),
    case dict:find(US1, SRUsers) of
  {ok, GroupNames} ->
      NewGroups = if
          Groups == [] -> GroupNames;
          true -> Groups
      end,
      {both, NewGroups};
  error ->
      {Subscription, Groups}
    end.

in_subscription(Acc, User, Server, JID, Type, _Reason) ->
    process_subscription(in, User, Server, JID, Type, Acc).

out_subscription(UserFrom, ServerFrom, JIDTo, unsubscribed) ->
    %% Remove pending out subscription
    #jid{luser = UserTo, lserver = ServerTo} = JIDTo,
    JIDFrom = jlib:make_jid(UserFrom, UserTo, ""),
    mod_roster:out_subscription(UserTo, ServerTo, JIDFrom, unsubscribe),

    %% Remove pending in subscription
    mod_roster:in_subscription(aaaa, UserFrom, ServerFrom, JIDTo, unsubscribe, ""),

    process_subscription(out, UserFrom, ServerFrom, JIDTo, unsubscribed, false);
out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type, false).

process_subscription(Direction, User, Server, JID, _Type, Acc) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    US1 = {U1, S1},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers =
  lists:usort(
    lists:flatmap(
      fun(Group) ->
        get_group_users(LServer, Group)
      end, DisplayedGroups)),
    case lists:member(US1, SRUsers) of
  true ->
      case Direction of
    in ->
        {stop, false};
    out ->
        stop
      end;
  false ->
      Acc
    end.

list_groups(Host) ->
    list_groups(Host, gen_mod:db_type(Host, ?MODULE)).

list_groups(Host, mnesia) ->
    mnesia:dirty_select(
      sr_group,
      [{#sr_group{group_host = {'$1', '$2'},
      _ = '_'},
  [{'==', '$2', Host}],
  ['$1']}]);
list_groups(Host, odbc) ->
    case ejabberd_odbc:sql_query(
           Host, ["select name from sr_group;"]) of
        {selected, ["name"], Rs} ->
            [G || {G} <- Rs];
        _ ->
            []
    end.

groups_with_opts(Host) ->
    groups_with_opts(Host, gen_mod:db_type(Host, ?MODULE)).

groups_with_opts(Host, mnesia) ->
    Gs = mnesia:dirty_select(
     sr_group,
     [{#sr_group{group_host={'$1', Host}, opts='$2', _='_'},
       [],
       [['$1','$2']] }]),
    lists:map(fun([G,O]) -> {G, O} end, Gs);
groups_with_opts(Host, odbc) ->
    case ejabberd_odbc:sql_query(
           Host, ["select name, opts from sr_group;"]) of
        {selected, ["name", "opts"], Rs} ->
            [{G, ejabberd_odbc:decode_term(Opts)} || {G, Opts} <- Rs];
        _ ->
            []
    end.

create_group(Host, Group) ->
    create_group(Host, Group, []).

create_group(Host, Group, Opts) ->
    create_group(Host, Group, Opts, gen_mod:db_type(Host, ?MODULE)).

create_group(Host, Group, Opts, mnesia) ->
    R = #sr_group{group_host = {Group, Host}, opts = Opts},
    F = fun() ->
    mnesia:write(R)
  end,
    mnesia:transaction(F);
create_group(Host, Group, Opts, odbc) ->
    SGroup = ejabberd_odbc:escape(Group),
    SOpts = ejabberd_odbc:encode_term(Opts),
    F = fun() ->
                odbc_queries:update_t("sr_group",
                                      ["name", "opts"],
                                      [SGroup, SOpts],
                                      ["name='", SGroup, "'"])
        end,
    ejabberd_odbc:sql_transaction(Host, F).

delete_group(Host, Group) ->
    delete_group(Host, Group, gen_mod:db_type(Host, ?MODULE)).

delete_group(Host, Group, mnesia) ->
    GroupHost = {Group, Host},
    F = fun() ->
    %% Delete the group ...
    mnesia:delete({sr_group, GroupHost}),
    %% ... and its users
    Users = mnesia:index_read(sr_user, GroupHost, #sr_user.group_host),
    lists:foreach(fun(UserEntry) ->
              mnesia:delete_object(UserEntry)
            end, Users)
  end,
    mnesia:transaction(F);
delete_group(Host, Group, odbc) ->
    SGroup = ejabberd_odbc:escape(Group),
    F = fun() ->
                ejabberd_odbc:sql_query_t(
                  ["delete from sr_group where name='", SGroup, "';"]),
                ejabberd_odbc:sql_query_t(
                  ["delete from sr_user where grp='", SGroup, "';"])
        end,
    ejabberd_odbc:sql_transaction(Host, F).

get_group_opts(Host, Group) ->
    get_group_opts(Host, Group, gen_mod:db_type(Host, ?MODULE)).

get_group_opts(Host, Group, mnesia) ->
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
  [#sr_group{opts = Opts}] ->
      Opts;
  _ ->
      error
    end;
get_group_opts(Host, Group, odbc) ->
    SGroup = ejabberd_odbc:escape(Group),
    case catch ejabberd_odbc:sql_query(
                 Host, ["select opts from sr_group "
                        "where name='", SGroup, "';"]) of
        {selected, ["opts"], [{SOpts}]} ->
            ejabberd_odbc:decode_term(SOpts);
        _ ->
            error
    end.

set_group_opts(Host, Group, Opts) ->
    set_group_opts(Host, Group, Opts, gen_mod:db_type(Host, ?MODULE)).

set_group_opts(Host, Group, Opts, mnesia) ->
    R = #sr_group{group_host = {Group, Host}, opts = Opts},
    F = fun() ->
    mnesia:write(R)
  end,
    mnesia:transaction(F);
set_group_opts(Host, Group, Opts, odbc) ->
    SGroup = ejabberd_odbc:escape(Group),
    SOpts = ejabberd_odbc:encode_term(Opts),
    F = fun() ->
                odbc_queries:update_t("sr_group",
                                      ["name", "opts"],
                                      [SGroup, SOpts],
                                      ["name='", SGroup, "'"])
        end,
    ejabberd_odbc:sql_transaction(Host, F).

get_user_groups(US) ->
    Host = element(2, US),
    DBType = gen_mod:db_type(Host, ?MODULE),
    get_user_groups(US, Host, DBType) ++ get_special_users_groups(Host).

get_user_groups(US, Host, mnesia) ->
    case catch mnesia:dirty_read(sr_user, US) of
  Rs when is_list(Rs) ->
      [Group || #sr_user{group_host = {Group, H}} <- Rs, H == Host];
  _ ->
      []
    end;
get_user_groups(US, Host, odbc) ->
    SJID = make_jid_s(US),
    case catch ejabberd_odbc:sql_query(
                 Host, ["select grp from sr_user "
                        "where jid='", SJID, "';"]) of
        {selected, ["grp"], Rs} ->
            [G || {G} <- Rs];
        _ ->
            []
    end.

is_group_enabled(Host1, Group1) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    case get_group_opts(Host, Group) of
        error ->
            false;
        Opts ->
            not lists:member(disabled, Opts)
    end.

%% @spec (Host::string(), Group::string(), Opt::atom(), Default) -> OptValue | Default
get_group_opt(Host, Group, Opt, Default) ->
    case get_group_opts(Host, Group) of
        error ->
            Default;
        Opts ->
      case lists:keysearch(Opt, 1, Opts) of
    {value, {_, Val}} ->
        Val;
    false ->
        Default
      end
    end.

get_online_users(Host) ->
    lists:usort([{U, S} || {U, S, _} <- ejabberd_sm:get_vh_session_list(Host)]).

get_group_users(Host1, Group1) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    case get_group_opt(Host, Group, all_users, false) of
  true ->
      ejabberd_auth:get_vh_registered_users(Host);
  false ->
      []
    end ++
    case get_group_opt(Host, Group, online_users, false) of
  true ->
      get_online_users(Host);
  false ->
      []
    end ++
    get_group_explicit_users(Host, Group).

%% @spec (Host::string(), Group::string()) -> [{User::string(), Server::string()}]
get_group_explicit_users(Host, Group) ->
    get_group_explicit_users(Host, Group, gen_mod:db_type(Host, ?MODULE)).

get_group_explicit_users(Host, Group, mnesia) ->
    Read = (catch mnesia:dirty_index_read(
        sr_user,
        {Group, Host},
        #sr_user.group_host)),
    case Read of
  Rs when is_list(Rs) ->
      [R#sr_user.us || R <- Rs];
  _ ->
      []
    end;
get_group_explicit_users(Host, Group, odbc) ->
    SGroup = ejabberd_odbc:escape(Group),
    case catch ejabberd_odbc:sql_query(
                 Host, ["select jid from sr_user "
                        "where grp='", SGroup, "';"]) of
        {selected, ["jid"], Rs} ->
            lists:map(
              fun({JID}) ->
                      {U, S, _} = jlib:jid_tolower(
                                    jlib:string_to_jid(JID)),
                      {U, S}
              end, Rs);
        _ ->
            []
    end.

get_group_name(Host1, Group1) ->
    {Host, Group} = split_grouphost(Host1, Group1),
    get_group_opt(Host, Group, name, Group).

%% Get list of names of groups that have @all@/@online@/etc in the memberlist
get_special_users_groups(Host) ->
    lists:filter(
      fun(Group) ->
        get_group_opt(Host, Group, all_users, false)
      orelse get_group_opt(Host, Group, online_users, false)
      end,
      list_groups(Host)).

%% Get list of names of groups that have @online@ in the memberlist
get_special_users_groups_online(Host) ->
    lists:filter(
      fun(Group) ->
        get_group_opt(Host, Group, online_users, false)
      end,
      list_groups(Host)).

%% @doc Get the list of groups that are displayed to this user
get_user_displayed_groups(US) ->
    Host = element(2, US),
    DisplayedGroups1 =
  lists:usort(
    lists:flatmap(
      fun(Group) ->
        case is_group_enabled(Host, Group) of
      true ->
          get_group_opt(Host, Group, displayed_groups, []);
      false ->
          []
        end
      end, get_user_groups(US))),
    [Group || Group <- DisplayedGroups1, is_group_enabled(Host, Group)].

is_user_in_group(US, Group, Host) ->
    is_user_in_group(US, Group, Host, gen_mod:db_type(Host, ?MODULE)).

is_user_in_group(US, Group, Host, mnesia) ->
    case catch mnesia:dirty_match_object(
     #sr_user{us=US, group_host={Group, Host}}) of
        [] -> lists:member(US, get_group_users(Host, Group));
  _  -> true
    end;
is_user_in_group(US, Group, Host, odbc) ->
    SJID = make_jid_s(US),
    SGroup = ejabberd_odbc:escape(Group),
    case catch ejabberd_odbc:sql_query(
                 Host, ["select * from sr_user where "
                        "jid='", SJID, "' and grp='", SGroup, "';"]) of
        {selected, _, []} ->
            lists:member(US, get_group_users(Host, Group));
        _ ->
            true
    end.

%% @spec (Host::string(), {User::string(), Server::string()}, Group::string()) -> {atomic, ok}
add_user_to_group(Host, US, Group) ->
    {LUser, LServer} = US,
    case ejabberd_regexp:run(LUser, "^@.+@$") of
  match ->
      GroupOpts = ?MODULE:get_group_opts(Host, Group),
      MoreGroupOpts =
    case LUser of
        "@all@" -> [{all_users, true}];
        "@online@" -> [{online_users, true}];
        _ -> []
    end,
            ?MODULE:set_group_opts(
        Host, Group,
        GroupOpts ++ MoreGroupOpts);
  nomatch ->
      %% Push this new user to members of groups where this group is displayed
      push_user_to_displayed(LUser, LServer, Group, Host, both),
      %% Push members of groups that are displayed to this group
      push_displayed_to_user(LUser, LServer, Group, Host, both),
            add_user_to_group(Host, US, Group, gen_mod:db_type(Host, ?MODULE))
    end.

add_user_to_group(Host, US, Group, mnesia) ->
    R = #sr_user{us = US, group_host = {Group, Host}},
    F = fun() ->
                mnesia:write(R)
        end,
    mnesia:transaction(F);
add_user_to_group(Host, US, Group, odbc) ->
    SJID = make_jid_s(US),
    SGroup = ejabberd_odbc:escape(Group),
    F = fun() ->
                odbc_queries:update_t(
                  "sr_user",
                  ["jid", "grp"],
                  [SJID, SGroup],
                  ["jid='", SJID, "' and grp='", SGroup, "'"])
        end,
    ejabberd_odbc:sql_transaction(Host, F).

push_displayed_to_user(LUser, LServer, Group, Host, Subscription) ->
    GroupsOpts = groups_with_opts(LServer),
    GroupOpts = proplists:get_value(Group, GroupsOpts, []),
    DisplayedGroups = proplists:get_value(displayed_groups, GroupOpts, []),
    [push_members_to_user(LUser, LServer, DGroup, Host, Subscription) || DGroup <- DisplayedGroups].

remove_user_from_group(Host, US, Group) ->
    {LUser, LServer} = US,
    case ejabberd_regexp:run(LUser, "^@.+@$") of
  match ->
      GroupOpts = ?MODULE:get_group_opts(Host, Group),
      NewGroupOpts =
    case LUser of
        "@all@" ->
      lists:filter(fun(X) -> X/={all_users,true} end, GroupOpts);
        "@online@" ->
      lists:filter(fun(X) -> X/={online_users,true} end, GroupOpts)
    end,
      ?MODULE:set_group_opts(Host, Group, NewGroupOpts);
  nomatch ->
            Result = remove_user_from_group(Host, US, Group,
                                            gen_mod:db_type(Host, ?MODULE)),
      %% Push removal of the old user to members of groups where the group that this user was members was displayed
      push_user_to_displayed(LUser, LServer, Group, Host, remove),
      %% Push removal of members of groups that where displayed to the group which this user has left
      push_displayed_to_user(LUser, LServer, Group, Host, remove),
      Result
    end.

remove_user_from_group(Host, US, Group, mnesia) ->
    R = #sr_user{us = US, group_host = {Group, Host}},
    F = fun() ->
                mnesia:delete_object(R)
        end,
    mnesia:transaction(F);
remove_user_from_group(Host, US, Group, odbc) ->
    SJID = make_jid_s(US),
    SGroup = ejabberd_odbc:escape(Group),
    F = fun() ->
                ejabberd_odbc:sql_query_t(
                  ["delete from sr_user where jid='",
                   SJID, "' and grp='", SGroup, "';"]),
                ok
        end,
    ejabberd_odbc:sql_transaction(Host, F).

push_members_to_user(LUser, LServer, Group, Host, Subscription) ->
    GroupsOpts = groups_with_opts(LServer),
    GroupOpts = proplists:get_value(Group, GroupsOpts, []),
    GroupName = proplists:get_value(name, GroupOpts, Group),
    Members = get_group_users(Host, Group),
    lists:foreach(
      fun({U, S}) ->
        push_roster_item(LUser, LServer, U, S, GroupName, Subscription)
      end, Members).

push_user_to_displayed(LUser, LServer, Group, Host, Subscription) ->
    GroupsOpts = groups_with_opts(Host),
    GroupOpts = proplists:get_value(Group, GroupsOpts, []),
    GroupName = proplists:get_value(name, GroupOpts, Group),
    DisplayedToGroupsOpts = displayed_to_groups(Group, Host),
    [push_user_to_group(LUser, LServer, GroupD, Host, GroupName, Subscription) || {GroupD, _Opts} <- DisplayedToGroupsOpts].

push_user_to_group(LUser, LServer, Group, Host, GroupName, Subscription) ->
    lists:foreach(
      fun({U, S})  when (U == LUser) and (S == LServer) -> ok;
         ({U, S}) ->
        push_roster_item(U, S, LUser, LServer, GroupName, Subscription)
      end, get_group_users(Host, Group)).

%% Get list of groups to which this group is displayed
displayed_to_groups(GroupName, LServer) ->
    GroupsOpts = groups_with_opts(LServer),
    lists:filter(
      fun({_Group, Opts}) ->
        lists:member(GroupName, proplists:get_value(displayed_groups, Opts, []))
      end, GroupsOpts).

push_item(User, Server, From, Item) ->
    %% It was
    %%  ejabberd_sm:route(jlib:make_jid("", "", ""),
    %%                    jlib:make_jid(User, Server, "")
    %% why?
    ejabberd_sm:route(From, jlib:make_jid(User, Server, ""),
          {xmlelement, "broadcast", [],
           [{item,
       Item#roster.jid,
       Item#roster.subscription}]}),
    Stanza = jlib:iq_to_xml(
         #iq{type = set, xmlns = ?NS_ROSTER,
       id = "push" ++ randoms:get_string(),
       sub_el = [{xmlelement, "query",
            [{"xmlns", ?NS_ROSTER}],
            [item_to_xml(Item)]}]}),
    lists:foreach(
      fun(Resource) ->
        JID = jlib:make_jid(User, Server, Resource),
        ejabberd_router:route(JID, JID, Stanza)
      end, ejabberd_sm:get_user_resources(User, Server)).

push_roster_item(User, Server, ContactU, ContactS, GroupName, Subscription) ->
    Item = #roster{usj = {User, Server, {ContactU, ContactS, ""}},
       us = {User, Server},
       jid = {ContactU, ContactS, ""},
       name = "",
       subscription = Subscription,
       ask = none,
       groups = [GroupName]},
    push_item(User, Server, jlib:make_jid("", Server, ""), Item).

item_to_xml(Item) ->
    Attrs1 = [{"jid", jlib:jid_to_string(Item#roster.jid)}],
    Attrs2 = case Item#roster.name of
     "" ->
         Attrs1;
     Name ->
         [{"name", Name} | Attrs1]
       end,
    Attrs3 = case Item#roster.subscription of
     none ->
         [{"subscription", "none"} | Attrs2];
     from ->
         [{"subscription", "from"} | Attrs2];
     to ->
         [{"subscription", "to"} | Attrs2];
     both ->
         [{"subscription", "both"} | Attrs2];
     remove ->
         [{"subscription", "remove"} | Attrs2]
       end,
    Attrs4 = case ask_to_pending(Item#roster.ask) of
     out ->
         [{"ask", "subscribe"} | Attrs3];
     both ->
         [{"ask", "subscribe"} | Attrs3];
     _ ->
         Attrs3
       end,
    SubEls1 = lists:map(fun(G) ->
        {xmlelement, "group", [], [{xmlcdata, G}]}
      end, Item#roster.groups),
    SubEls = SubEls1 ++ Item#roster.xs,
    {xmlelement, "item", Attrs4, SubEls}.

ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.

user_available(New) ->
    LUser = New#jid.luser,
    LServer = New#jid.lserver,
    Resources = ejabberd_sm:get_user_resources(LUser, LServer),
    ?DEBUG("user_available for ~p @ ~p (~p resources)",
     [LUser, LServer, length(Resources)]),
    case length(Resources) of
  %% first session for this user
  1 ->
      %% This is a simplification - we ignore he 'display'
      %% property - @online@ is always reflective.
      OnlineGroups = get_special_users_groups_online(LServer),
      lists:foreach(
        fun(OG) ->
          ?DEBUG("user_available: pushing  ~p @ ~p grp ~p",
           [LUser, LServer, OG ]),
          push_user_to_displayed(LUser, LServer, OG, LServer, both)
        end, OnlineGroups);
  _ ->
      ok
    end.

unset_presence(LUser, LServer, Resource, Status) ->
    Resources = ejabberd_sm:get_user_resources(LUser, LServer),
    ?DEBUG("unset_presence for ~p @ ~p / ~p -> ~p (~p resources)",
     [LUser, LServer, Resource, Status, length(Resources)]),
    %% if user has no resources left...
    case length(Resources) of
  0 ->
      %% This is a simplification - we ignore he 'display'
      %% property - @online@ is always reflective.
      OnlineGroups = get_special_users_groups_online(LServer),
      %% for each of these groups...
      lists:foreach(
        fun(OG) ->
          %% Push removal of the old user to members of groups
          %% where the group that this uwas members was displayed
          push_user_to_displayed(LUser, LServer, OG, LServer, remove),
          %% Push removal of members of groups that where
          %% displayed to the group which thiuser has left
          push_displayed_to_user(LUser, LServer, OG, LServer,remove)
        end, OnlineGroups);
  _ ->
      ok
    end.

split_grouphost(Host, Group) ->
    case string:tokens(Group, "@") of
  [GroupName, HostName] ->
      {HostName, GroupName};
  [_] ->
      {Host, Group}
    end.

make_jid_s(U, S) ->
    ejabberd_odbc:escape(
      jlib:jid_to_string(
        jlib:jid_tolower(
          jlib:make_jid(U, S, "")))).

make_jid_s({U, S}) ->
    make_jid_s(U, S).
