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
  get_group_users/2,
  is_user_in_group/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").

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
    ?MODULE, process_item, 50).

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
    ?MODULE, process_item, 50).

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

get_group_users(Host1, Group1) ->
  {Host, Group} = split_grouphost(Host1, Group1),
  SGroup = ejabberd_odbc:escape(Group),
  case catch ejabberd_odbc:sql_query(
      Host, ["select username from user_organizations uo join users u "
        "on uo.user_id=u.id where organization_id=", SGroup, ";"]) of
    {selected, ["username"], Rs} ->
      lists:map(
        fun({Username}) ->
            {Username, Host}
        end, Rs);
    _ ->
      []
  end.

get_group_name(Host1, Group1) ->
  {Host, Group} = split_grouphost(Host1, Group1),
  SGroup = ejabberd_odbc:escape(Group),
  case catch ejabberd_odbc:sql_query(
      Host, ["select name from organizations "
        "where id='", SGroup, "';"]) of
    {selected, ["name"], [{Name}]} ->
      Name;
    _ ->
      Group
  end.

%% @doc Get the list of groups that are displayed to this user
get_user_displayed_groups(US) ->
  Host = element(2, US),
  Username = ejabberd_odbc:escape(element(1, US)),

  case catch ejabberd_odbc:sql_query(
      Host, ["select organization_id from user_organizations uo join users u "
        "ON uo.user_id=u.id where username='", Username, "';"]) of
    {selected, ["organization_id"], Rs} ->
      [G || {G} <- Rs];
    _ ->
      []
  end.

is_user_in_group(US, Group, Host) ->
  Username = ejabberd_odbc:escape(element(1, US)),
  SGroup = ejabberd_odbc:escape(Group),
  case catch ejabberd_odbc:sql_query(
      Host, ["select count(*) from user_organizations uo JOIN users u "
        "ON uo.user_id=u.id WHERE username='",Username,"' "
        "AND organization_id=",SGroup,";"]) of
    {selected, _, [{"1"}]} ->
      true;
    _ ->
      false
  end.

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

split_grouphost(Host, Group) ->
    case string:tokens(Group, "@") of
  [GroupName, HostName] ->
      {HostName, GroupName};
  [_] ->
      {Host, Group}
    end.
