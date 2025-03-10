-module(chat_client).
-behaviour(gen_server).

-export([
    start/1,
    send/1,
    send_private/2,
    list_clients/0,
    disconnect/0,
    get_server_node/1,
    set_topic/1,
    kick/1,
    mute/2,
    unmute/1,
    promote/1,
    get_topic/0,
    get_admins/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {username, server_node}).


%% @doc Starts the chat client process with the given username.
-spec start(string()) -> {ok, pid()} | {error, already_running}.
start(Username) ->
    case whereis(chat_client) of
        undefined ->
            gen_server:start_link({local, chat_client}, ?MODULE, Username, []);
        _ ->
            io:format("A chat client is already running on this node. Cannot start another.~n"),
            {error, already_running}
    end.

%% @doc Sends a public message via the chat client.
-spec send(string()) -> ok | {error, not_started}.
send(Message) ->
    case whereis(chat_client) of
        undefined ->
            io:format("No chat client running on this node. Please start one first.~n"),
            {error, not_started};
        _ ->
            gen_server:cast(chat_client, {send, Message}),
            ok
    end.

%% @doc Sends a private message to the specified recipient.
-spec send_private(string(), string()) -> ok | {error, not_started}.
send_private(To, Message) ->
    case whereis(chat_client) of
        undefined ->
            io:format("No chat client running on this node. Please start one first.~n"),
            {error, not_started};
        _ ->
            gen_server:cast(chat_client, {send_private, To, Message}),
            ok
    end.

%% @doc Lists all connected clients by querying the server.
-spec list_clients() -> [string()].
list_clients() ->
    chat_server:list_clients().

%% @doc Disconnects the chat client from the server.
-spec disconnect() -> ok | {error, not_started}.
disconnect() ->
    case whereis(chat_client) of
        undefined ->
            io:format("No chat client running on this node.~n"),
            {error, not_started};
        _ ->
            gen_server:cast(chat_client, disconnect),
            ok
    end.

%% @doc Constructs the server node name from a given short name.
-spec get_server_node(string()) -> atom().
get_server_node(SName) ->
    {ok, Hostname} = inet:gethostname(),
    FullName = SName ++ "@" ++ Hostname,
    list_to_atom(FullName).

%% @doc Sends a request to change the chat topic.
-spec set_topic(string()) -> ok.
set_topic(NewTopic) ->
    gen_server:cast(chat_client, {set_topic, NewTopic}),
    ok.

%% @doc Sends a kick request for a specified user.
-spec kick(string()) -> ok.
kick(Target) ->
    gen_server:cast(chat_client, {kick, Target}),
    ok.

%% @doc Sends a mute request for a specified user for a given duration (in seconds).
-spec mute(string(), pos_integer()) -> ok.
mute(Target, Duration) ->
    gen_server:cast(chat_client, {mute, Target, Duration}),
    ok.

%% @doc Sends an unmute request for a specified user.
-spec unmute(string()) -> ok.
unmute(Target) ->
    gen_server:cast(chat_client, {unmute, Target}),
    ok.

%% @doc Sends a promote request for a specified user.
-spec promote(string()) -> ok.
promote(Target) ->
    gen_server:cast(chat_client, {promote, Target}),
    ok.

%% @doc Retrieves the current chat topic from the server.
-spec get_topic() -> string().
get_topic() ->
    chat_server:get_topic().

%% @doc Retrieves the list of admin users from the server.
-spec get_admins() -> [string()].
get_admins() ->
    chat_server:get_admins().

%% gen_server Callback Functions

%% @doc Initializes the chat client by connecting to the chat server.
-spec init(string()) -> {ok, #state{}} | {stop, term()}.
init(Username) ->
    ServerNode = get_server_node("server"),
    case net_adm:ping(ServerNode) of
        pong ->
            io:format("Successfully connected to server node ~p~n", [ServerNode]);
        pang ->
            io:format("WARNING: Could not connect to server node ~p. Check that the server is running and reachable.~n", [ServerNode])
    end,
    timer:sleep(1000),
    case chat_server:connect(Username, self()) of
        {ok, History} ->
            io:format("Connected as ~s.~nPrevious chat history: ~p~n", [Username, History]),
            {ok, #state{username = Username, server_node = ServerNode}};
        {error, Reason} ->
            io:format("Failed to connect as ~s: ~p~n", [Username, Reason]),
            {stop, Reason}
    end.

%% @doc Handles synchronous calls (not used extensively in this client).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc Handles asynchronous cast messages.
handle_cast({send, Message}, State = #state{username = Username}) ->
    chat_server:send_message(Username, Message),
    {noreply, State};
handle_cast({send_private, To, Message}, State = #state{username = Username}) ->
    chat_server:private_message(Username, To, Message),
    {noreply, State};
handle_cast(disconnect, State = #state{username = Username}) ->
    chat_server:disconnect(Username),
    io:format("Disconnected from chat server.~n", []),
    {stop, normal, State};
handle_cast({set_topic, NewTopic}, State = #state{username = Username}) ->
    Reply = chat_server:set_topic(Username, NewTopic),
    io:format("Set topic reply: ~p~n", [Reply]),
    {noreply, State};
handle_cast({kick, Target}, State = #state{username = Username}) ->
    Reply = chat_server:kick(Username, Target),
    io:format("Kick reply: ~p~n", [Reply]),
    {noreply, State};
handle_cast({mute, Target, Duration}, State = #state{username = Username}) ->
    Reply = chat_server:mute(Username, Target, Duration),
    io:format("Mute reply: ~p~n", [Reply]),
    {noreply, State};
handle_cast({unmute, Target}, State = #state{username = Username}) ->
    Reply = chat_server:unmute(Username, Target),
    io:format("Unmute reply: ~p~n", [Reply]),
    {noreply, State};
handle_cast({promote, Target}, State = #state{username = Username}) ->
    Reply = chat_server:promote(Username, Target),
    io:format("Promote reply: ~p~n", [Reply]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handles incoming messages from the chat server.
handle_info({history, History}, State) ->
    io:format("Received chat history: ~p~n", [History]),
    {noreply, State};
handle_info({offline, OffMsgs}, State) ->
    io:format("You have offline private messages: ~p~n", [OffMsgs]),
    {noreply, State};
handle_info({topic, NewTopic}, State) ->
    io:format("Chat topic is now: ~s~n", [NewTopic]),
    {noreply, State};
handle_info({chat, From, Message, Timestamp}, State) ->
    io:format("[~p] ~s: ~s~n", [Timestamp, From, Message]),
    {noreply, State};
handle_info({entry, User}, State) ->
    io:format("User ~s has joined the chat.~n", [User]),
    {noreply, State};
handle_info({exit, User}, State) ->
    io:format("User ~s has left the chat.~n", [User]),
    {noreply, State};
handle_info({private, From, To, Message, Timestamp}, State) ->
    io:format("[PRIVATE][~p] ~s -> ~s: ~s~n", [Timestamp, From, To, Message]),
    {noreply, State};
handle_info({private_sent, To, Message}, State) ->
    io:format("Private message sent to ~s: ~s~n", [To, Message]),
    {noreply, State};
handle_info({error, Reason}, State) ->
    io:format("Error: ~p~n", [Reason]),
    {noreply, State};
handle_info({kick_notice, Target}, State = #state{username = Username}) ->
    if
        Target =:= Username ->
            io:format("You have been kicked from the chat.~n", []),
            {stop, normal, State};
        true ->
            {noreply, State}
    end;
handle_info(Other, State) ->
    io:format("Unknown message: ~p~n", [Other]),
    {noreply, State}.

%% @doc Called when the server is terminating.
terminate(_Reason, _State) ->
    ok.

%% @doc Handles code upgrades.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
