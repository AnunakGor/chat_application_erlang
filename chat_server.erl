-module(chat_server).
-behaviour(gen_server).

-export([
    start_link/2,
    stop/0,
    connect/2,
    disconnect/1,
    send_message/2,
    list_clients/0,
    private_message/3,
    get_history/0,
    server_info/0,
    set_topic/2,
    get_topic/0,
    get_admins/0,
    kick/2,
    mute/3,
    unmute/2,
    promote/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    capacity,             %% maximum number of clients allowed
    history_count,        %% number of past messages to send to a new client
    clients = #{},        %% map: Username => ClientPid
    history = [],         %% list of messages (most recent first)
    topic = "Default Topic",   %% current chatroom topic
    admins = ["admin"],        %% list of admin usernames (strings)
    muted = #{},               %% map: Username -> UnmuteTime (timestamp in sec)
    offline = #{}              %% map: Username -> list of offline private messages
}).


start_link(Capacity, HistoryCount) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, {Capacity, HistoryCount}, []).

stop() ->
    gen_server:call({global, ?MODULE}, stop).

connect(Username, ClientPid) ->
    gen_server:call({global, ?MODULE}, {connect, Username, ClientPid}).

disconnect(Username) ->
    gen_server:call({global, ?MODULE}, {disconnect, Username}).

send_message(Username, Message) ->
    gen_server:cast({global, ?MODULE}, {send_message, Username, Message}).

list_clients() ->
    gen_server:call({global, ?MODULE}, list_clients).

private_message(From, To, Message) ->
    gen_server:cast({global, ?MODULE}, {private_message, From, To, Message}).

get_history() ->
    gen_server:call({global, ?MODULE}, get_history).

server_info() ->
    gen_server:call({global, ?MODULE}, server_info).

set_topic(Admin, NewTopic) ->
    gen_server:call({global, ?MODULE}, {set_topic, Admin, NewTopic}).

get_topic() ->
    gen_server:call({global, ?MODULE}, get_topic).

get_admins() ->
    gen_server:call({global, ?MODULE}, get_admins).

kick(Admin, Target) ->
    gen_server:call({global, ?MODULE}, {kick, Admin, Target}).

mute(Admin, Target, Duration) ->
    gen_server:call({global, ?MODULE}, {mute, Admin, Target, Duration}).

unmute(Admin, Target) ->
    gen_server:call({global, ?MODULE}, {unmute, Admin, Target}).

promote(Admin, Target) ->
    gen_server:call({global, ?MODULE}, {promote, Admin, Target}).

init({Capacity, HistoryCount}) ->
    {ok, #state{capacity = Capacity, history_count = HistoryCount}}.

handle_call({connect, Username, ClientPid}, _From, State = #state{
    clients = Clients, capacity = Capacity, history = History, admins = Admins, topic = Topic, offline = Offline
}) ->
    case maps:is_key(Username, Clients) of
        true ->
            {reply, {error, username_taken}, State};
        false ->
            Size = maps:size(Clients),
            if
                Size >= Capacity ->
                    {reply, {error, server_full}, State};
                true ->
                    NewAdmins = case Size of
                                  0 -> [Username];
                                  _ -> Admins
                                end,
                    NewClients = Clients#{Username => ClientPid},
                    broadcast({entry, Username}, NewClients),
                    (case Size of
                         0 -> broadcast({admin, Username, "You are now the admin by default."}, NewClients);
                         _ -> ok
                     end),
                    HistoryCount = State#state.history_count,
                    LastMessages = lists:sublist(lists:reverse(History), HistoryCount),
                    ClientPid ! {history, LastMessages},
                    ClientPid ! {topic, Topic},
                    OfflineMsgs = case maps:find(Username, Offline) of
                                    {ok, OffMsgs} -> OffMsgs;
                                    error -> []
                                  end,
                    ClientPid ! {offline, OfflineMsgs},
                    NewOffline = maps:remove(Username, Offline),
                    {reply, {ok, LastMessages}, State#state{
                        clients = NewClients,
                        admins = NewAdmins,
                        offline = NewOffline
                    }}
            end
    end;

handle_call({disconnect, Username}, _From, State = #state{clients = Clients}) ->
    case maps:find(Username, Clients) of
        error ->
            {reply, {error, not_connected}, State};
        {ok, _Pid} ->
            NewClients = maps:remove(Username, Clients),
            broadcast({exit, Username}, NewClients),
            {reply, ok, State#state{clients = NewClients}}
    end;

handle_call(list_clients, _From, State = #state{clients = Clients}) ->
    {reply, maps:keys(Clients), State};

handle_call(get_history, _From, State = #state{history = History}) ->
    {reply, lists:reverse(History), State};

handle_call(server_info, _From, State = #state{clients = Clients, history = History}) ->
    Info = #{clients => maps:keys(Clients), history => lists:reverse(History)},
    {reply, Info, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

% handle_call({set_topic, _User, NewTopic}, _From, State) ->
%     NewState = State#state{topic = NewTopic},
%     broadcast({topic, NewTopic}, State#state.clients),
%     {reply, ok, NewState};
handle_call({set_topic, Admin, NewTopic}, _From, State = #state{admins = Admins}) ->
    case lists:member(Admin, Admins) of
         false ->
              {reply, {error, "You are not allowed to change the topic."}, State};
         true ->
              NewState = State#state{topic = NewTopic},
              broadcast({topic, NewTopic}, State#state.clients),
              {reply, ok, NewState}
    end;                    
  

handle_call(get_topic, _From, State = #state{topic = Topic}) ->
    {reply, Topic, State};

handle_call(get_admins, _From, State = #state{admins = Admins}) ->
    {reply, Admins, State};

handle_call({kick, Admin, Target}, _From, State = #state{admins = Admins, clients = Clients}) ->
    case lists:member(Admin, Admins) of
         false ->
              {reply, {error, "You are not allowed to kick users."}, State};
         true ->
              case maps:find(Target, Clients) of
                  error ->
                      {reply, {error, "User not found."}, State};
                  {ok, Pid} ->
                      Pid ! {kick_notice, Target},
                      NewClients = maps:remove(Target, Clients),
                      broadcast({exit, Target}, NewClients),
                      {reply, ok, State#state{clients = NewClients}}
              end
    end;

handle_call({mute, Admin, Target, Duration}, _From, State = #state{admins = Admins, muted = Muted}) ->
    case lists:member(Admin, Admins) of
         false ->
              {reply, {error, "You are not allowed to mute users."}, State};
         true ->
              UnmuteTime = erlang:system_time(second) + Duration,
              NewMuted = maps:put(Target, UnmuteTime, Muted),
              broadcast({muted, Target, UnmuteTime}, State#state.clients),
              {reply, ok, State#state{muted = NewMuted}}
    end;

handle_call({unmute, Admin, Target}, _From, State = #state{admins = Admins, muted = Muted}) ->
    case lists:member(Admin, Admins) of
         false ->
              {reply, {error, "You are not allowed to unmute users."}, State};
         true ->
              NewMuted = maps:remove(Target, Muted),
              broadcast({unmuted, Target}, State#state.clients),
              {reply, ok, State#state{muted = NewMuted}}
    end;

handle_call({promote, Admin, Target}, _From, State = #state{admins = Admins}) ->
    case lists:member(Admin, Admins) of
         false ->
              {reply, {error, "You are not allowed to promote users."}, State};
         true ->
              NewAdmins = lists:usort([Target | Admins]),
              broadcast({promoted, Target, "has been promoted to admin."}, State#state.clients),
              {reply, ok, State#state{admins = NewAdmins}}
    end.

handle_cast({send_message, Username, Message}, State = #state{
    clients = Clients, history = History, muted = Muted
}) ->
    CurrentTime = erlang:system_time(second),
    case maps:find(Username, Muted) of
        {ok, UnmuteTime} when CurrentTime < UnmuteTime ->
             case maps:find(Username, Clients) of
                 {ok, FromPid} ->
                     FromPid ! {error, {muted_until, UnmuteTime}},
                     {noreply, State};
                 error ->
                     {noreply, State}
             end;
        _ ->
             Timestamp = erlang:system_time(second),
             Msg = {chat, Username, Message, Timestamp},
             NewHistory = [Msg | History],
             broadcast(Msg, Clients),
             {noreply, State#state{history = NewHistory}}
    end;

handle_cast({private_message, From, To, Message}, State = #state{
    clients = Clients, history = History, offline = Offline
}) ->
    Timestamp = erlang:system_time(second),
    Msg = {private, From, To, Message, Timestamp},
    case maps:find(To, Clients) of
        error ->
            NewOffline = case maps:find(To, Offline) of
                           error -> maps:put(To, [Msg], Offline);
                           {ok, Msgs} -> maps:put(To, Msgs ++ [Msg], Offline)
                         end,
            case maps:find(From, Clients) of
                {ok, FromPid} ->
                    FromPid ! {error, {user_offline, To}},
                    % FromPid ! {error, {"User is not yet online on this server", To}},
                    ok;
                error ->
                    ok
            end,
            {noreply, State#state{offline = NewOffline}};
        {ok, ToPid} ->
            ToPid ! Msg,
            case maps:find(From, Clients) of
                {ok, FromPid} ->
                    FromPid ! {private_sent, To, Message},
                    ok;
                error ->
                    ok
            end,
            NewHistory = [Msg | History],
            {noreply, State#state{history = NewHistory}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

broadcast(Message, Clients) ->
    io:format("Broadcasting message: ~p~n", [Message]),
    lists:foreach(fun({_Username, Pid}) ->
                      io:format("Sending message to: ~p~n", [Pid]),
                      Pid ! Message
                  end, maps:to_list(Clients)).
