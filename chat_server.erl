-module(chat_server).
-behaviour(gen_server).

%% Type Definitions
-type username()     :: string().
-type message()      :: string().
-type topic()        :: string().
-type admin()        :: username().
-type client_pid()   :: pid().
-type clients()      :: #{username() => client_pid()}.
-type error_reason() :: term().

%% Exported Functions
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

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Internal functions
-export([broadcast/2]).

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

-type state() :: #state{
    capacity      :: pos_integer(),
    history_count :: pos_integer(),
    clients       :: map(),
    history       :: [term()],
    topic         :: string(),
    admins        :: [string()],
    muted         :: map(),
    offline       :: map()
}.

%% @doc Starts the chat server process with a given capacity and history count.
-spec start_link(Capacity :: pos_integer(), HistoryCount :: pos_integer()) ->
          {ok, pid()} | {error, error_reason()}.
start_link(Capacity, HistoryCount) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, {Capacity, HistoryCount}, []).

%% @doc Stops the chat server.
-spec stop() -> ok.
stop() ->
    gen_server:call({global, ?MODULE}, stop).

%% @doc Connects a client to the chat server.
-spec connect(Username :: username(), ClientPid :: client_pid()) ->
          {ok, [term()]} | {error, error_reason()}.
connect(Username, ClientPid) ->
    gen_server:call({global, ?MODULE}, {connect, Username, ClientPid}).

%% @doc Disconnects a client from the chat server.
-spec disconnect(Username :: username()) -> ok | {error, not_connected}.
disconnect(Username) ->
    gen_server:call({global, ?MODULE}, {disconnect, Username}).

%% @doc Sends a chat message from the specified client.
-spec send_message(Username :: username(), Message :: message()) -> ok.
send_message(Username, Message) ->
    gen_server:cast({global, ?MODULE}, {send_message, Username, Message}).

%% @doc Returns the list of currently connected clients.
-spec list_clients() -> [username()].
list_clients() ->
    gen_server:call({global, ?MODULE}, list_clients).

%% @doc Sends a private message from one client to another.
-spec private_message(From :: username(), To :: username(), Message :: message()) -> ok.
private_message(From, To, Message) ->
    gen_server:cast({global, ?MODULE}, {private_message, From, To, Message}).

%% @doc Retrieves the chat history.
-spec get_history() -> [term()].
get_history() ->
    gen_server:call({global, ?MODULE}, get_history).

%% @doc Retrieves information about the server (clients and chat history).
-spec server_info() -> map().
server_info() ->
    gen_server:call({global, ?MODULE}, server_info).

%% @doc Sets the chat room topic if the requester is an admin.
-spec set_topic(Admin :: admin(), NewTopic :: topic()) ->
          ok | {error, string()}.
set_topic(Admin, NewTopic) ->
    gen_server:call({global, ?MODULE}, {set_topic, Admin, NewTopic}).

%% @doc Gets the current chat room topic.
-spec get_topic() -> topic().
get_topic() ->
    gen_server:call({global, ?MODULE}, get_topic).

%% @doc Returns the list of admin users.
-spec get_admins() -> [admin()].
get_admins() ->
    gen_server:call({global, ?MODULE}, get_admins).

%% @doc Kicks a user from the chat server if the requester is an admin.
-spec kick(Admin :: admin(), Target :: username()) ->
          ok | {error, string()}.
kick(Admin, Target) ->
    gen_server:call({global, ?MODULE}, {kick, Admin, Target}).

%% @doc Mutes a user for a given duration (in seconds) if the requester is an admin.
-spec mute(Admin :: admin(), Target :: username(), Duration :: pos_integer()) ->
          ok | {error, string()}.
mute(Admin, Target, Duration) ->
    gen_server:call({global, ?MODULE}, {mute, Admin, Target, Duration}).

%% @doc Unmutes a user if the requester is an admin.
-spec unmute(Admin :: admin(), Target :: username()) ->
          ok | {error, string()}.
unmute(Admin, Target) ->
    gen_server:call({global, ?MODULE}, {unmute, Admin, Target}).

%% @doc Promotes a user to admin if the requester is an admin.
-spec promote(Admin :: admin(), Target :: username()) ->
          ok | {error, string()}.
promote(Admin, Target) ->
    gen_server:call({global, ?MODULE}, {promote, Admin, Target}).

%% gen_server Callback Functions

%% @doc Initializes the chat server state.
-spec init({Capacity :: pos_integer(), HistoryCount :: pos_integer()}) ->
          {ok, state()}.
init({Capacity, HistoryCount}) ->
    {ok, #state{capacity = Capacity, history_count = HistoryCount}}.

%% @doc Handles synchronous calls to the server.
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: state()) ->
          {reply, term(), state()} |
          {stop, term(), term(), state()}.
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

%% @doc Handles asynchronous cast messages.
-spec handle_cast(Msg :: term(), State :: state()) -> {noreply, state()}.
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
             MsgEntry = {chat, Username, Message, Timestamp},
             NewHistory = [MsgEntry | History],
             broadcast(MsgEntry, Clients),
             {noreply, State#state{history = NewHistory}}
    end;

handle_cast({private_message, From, To, Message}, State = #state{
    clients = Clients, history = History, offline = Offline
}) ->
    Timestamp = erlang:system_time(second),
    MsgEntry = {private, From, To, Message, Timestamp},
    case maps:find(To, Clients) of
        error ->
            NewOffline = case maps:find(To, Offline) of
                           error -> maps:put(To, [MsgEntry], Offline);
                           {ok, Msgs} -> maps:put(To, Msgs ++ [MsgEntry], Offline)
                         end,
            case maps:find(From, Clients) of
                {ok, FromPid} ->
                    FromPid ! {error, {user_offline, To}},
                    ok;
                error ->
                    ok
            end,
            {noreply, State#state{offline = NewOffline}};
        {ok, ToPid} ->
            ToPid ! MsgEntry,
            case maps:find(From, Clients) of
                {ok, FromPid} ->
                    FromPid ! {private_sent, To, Message},
                    ok;
                error ->
                    ok
            end,
            NewHistory = [MsgEntry | History],
            {noreply, State#state{history = NewHistory}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handles all other messages.
-spec handle_info(Info :: term(), State :: state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Called when the server is terminated.
-spec terminate(Reason :: term(), State :: state()) -> any().
terminate(_Reason, _State) ->
    ok.

%% @doc Handles code upgrades.
-spec code_change(OldVsn :: term(), State :: state(), Extra :: term()) ->
          {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Broadcasts a message to all connected clients.
-spec broadcast(Message :: term(), Clients :: clients()) -> ok.
broadcast(Message, Clients) ->
    io:format("Broadcasting message: ~p~n", [Message]),
    logger:info("Broadcasting message: ~p", [Message]),
    lists:foreach(
      fun({_Username, Pid}) ->
              io:format("Sending message to: ~p~n", [Pid]),
              logger:info("Sending message to: ~p", [Pid]),
              Pid ! Message
      end,
      maps:to_list(Clients)
    ),
    ok.
