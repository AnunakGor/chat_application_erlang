-module(chat_client).
-behaviour(gen_server).

%% Type Definitions
-type username()     :: string().
-type message()      :: string().

%% The state record holds the username (of type username()) and the server node (an atom).
-record(state, {username :: username(), server_node :: atom()}).

%% Exported Functions
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

%% @doc Starts the chat client process with the given username.
-spec start(Username :: username()) -> {ok, pid()} | {error, already_running}.
start(Username) ->
    case whereis(chat_client) of
        undefined ->
            gen_server:start_link({local, chat_client}, ?MODULE, Username, []);
        _ ->
            io:format("A chat client is already running on this node. Cannot start another.~n"),
            logger:info("A chat client is already running on this node. Cannot start another."),
            {error, already_running}
    end.

%% @doc Sends a public message via the chat client.
-spec send(Message :: message()) -> ok | {error, not_started}.
send(Message) ->
    case whereis(chat_client) of
        undefined ->
            io:format("No chat client running on this node. Please start one first.~n"),
            logger:info("No chat client running on this node. Please start one first."),
            {error, not_started};
        _ ->
            gen_server:cast(chat_client, {send, Message}),
            ok
    end.

%% @doc Sends a private message to the specified recipient.
-spec send_private(To :: username(), Message :: message()) -> ok | {error, not_started}.
send_private(To, Message) ->
    case whereis(chat_client) of
        undefined ->
            io:format("No chat client running on this node. Please start one first.~n"),
            logger:info("No chat client running on this node. Please start one first."),
            {error, not_started};
        _ ->
            gen_server:cast(chat_client, {send_private, To, Message}),
            ok
    end.

%% @doc Lists all connected clients by querying the server.
-spec list_clients() -> [username()].
list_clients() ->
    chat_server:list_clients().

%% @doc Disconnects the chat client from the server.
-spec disconnect() -> ok | {error, not_started}.
disconnect() ->
    case whereis(chat_client) of
        undefined ->
            io:format("No chat client running on this node.~n"),
            logger:info("No chat client running on this node."),
            {error, not_started};
        _ ->
            gen_server:cast(chat_client, disconnect),
            ok
    end.

%% @doc Constructs the server node name from a given short name.
-spec get_server_node(SName :: string()) -> atom().
get_server_node(SName) ->
    {ok, Hostname} = inet:gethostname(),
    FullName = SName ++ "@" ++ Hostname,
    list_to_atom(FullName).

%% @doc Sends a request to change the chat topic.
-spec set_topic(NewTopic :: string()) -> ok.
set_topic(NewTopic) ->
    gen_server:cast(chat_client, {set_topic, NewTopic}),
    ok.

%% @doc Sends a kick request for a specified user.
-spec kick(Target :: username()) -> ok.
kick(Target) ->
    gen_server:cast(chat_client, {kick, Target}),
    ok.

%% @doc Sends a mute request for a specified user for a given duration (in seconds).
-spec mute(Target :: username(), Duration :: pos_integer()) -> ok.
mute(Target, Duration) ->
    gen_server:cast(chat_client, {mute, Target, Duration}),
    ok.

%% @doc Sends an unmute request for a specified user.
-spec unmute(Target :: username()) -> ok.
unmute(Target) ->
    gen_server:cast(chat_client, {unmute, Target}),
    ok.

%% @doc Sends a promote request for a specified user.
-spec promote(Target :: username()) -> ok.
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
-spec init(Username :: username()) -> {ok, #state{}} | {stop, term()}.
init(Username) ->
    ServerNode = get_server_node("server"),
    case net_adm:ping(ServerNode) of
        pong ->
            io:format("Successfully connected to server node ~p~n", [ServerNode]),
            logger:info("Successfully connected to server node ~p", [ServerNode]);
        pang ->
            io:format("WARNING: Could not connect to server node ~p. Check that the server is running and reachable.~n", [ServerNode]),
            logger:info("WARNING: Could not connect to server node ~p. Check that the server is running and reachable.", [ServerNode])
    end,
    timer:sleep(1000),
    case chat_server:connect(Username, self()) of
        {ok, History} ->
            io:format("Connected as ~s.~nPrevious chat history: ~p~n", [Username, History]),
            logger:info("Connected as ~s. Previous chat history: ~p", [Username, History]),
            {ok, #state{username = Username, server_node = ServerNode}};
        {error, Reason} ->
            io:format("Failed to connect as ~s: ~p~n", [Username, Reason]),
            logger:info("Failed to connect as ~s: ~p", [Username, Reason]),
            {stop, Reason}
    end.

%% @doc Handles synchronous calls (not used extensively in this client).
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: #state{}) ->
          {reply, term(), #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc Handles asynchronous cast messages.
-spec handle_cast(Msg :: term(), State :: #state{}) -> {noreply, #state{}}.
handle_cast({send, Message}, State = #state{username = Username}) ->
    chat_server:send_message(Username, Message),
    {noreply, State};
handle_cast({send_private, To, Message}, State = #state{username = Username}) ->
    chat_server:private_message(Username, To, Message),
    {noreply, State};
handle_cast(disconnect, State = #state{username = Username}) ->
    chat_server:disconnect(Username),
    io:format("Disconnected from chat server.~n", []),
    logger:info("Disconnected from chat server."),
    {stop, normal, State};
handle_cast({set_topic, NewTopic}, State = #state{username = Username}) ->
    Reply = chat_server:set_topic(Username, NewTopic),
    io:format("Set topic reply: ~p~n", [Reply]),
    logger:info("Set topic reply: ~p", [Reply]),
    {noreply, State};
handle_cast({kick, Target}, State = #state{username = Username}) ->
    Reply = chat_server:kick(Username, Target),
    io:format("Kick reply: ~p~n", [Reply]),
    logger:info("Kick reply: ~p", [Reply]),
    {noreply, State};
handle_cast({mute, Target, Duration}, State = #state{username = Username}) ->
    Reply = chat_server:mute(Username, Target, Duration),
    io:format("Mute reply: ~p~n", [Reply]),
    logger:info("Mute reply: ~p", [Reply]),
    {noreply, State};
handle_cast({unmute, Target}, State = #state{username = Username}) ->
    Reply = chat_server:unmute(Username, Target),
    io:format("Unmute reply: ~p~n", [Reply]),
    logger:info("Unmute reply: ~p", [Reply]),
    {noreply, State};
handle_cast({promote, Target}, State = #state{username = Username}) ->
    Reply = chat_server:promote(Username, Target),
    io:format("Promote reply: ~p~n", [Reply]),
    logger:info("Promote reply: ~p", [Reply]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handles incoming messages from the chat server.
-spec handle_info(Info :: term(), State :: #state{}) -> {noreply, #state{}}.
handle_info({history, History}, State) ->
    io:format("Received chat history: ~p~n", [History]),
    logger:info("Received chat history: ~p", [History]),
    {noreply, State};
handle_info({offline, OffMsgs}, State) ->
    io:format("You have offline private messages: ~p~n", [OffMsgs]),
    logger:info("You have offline private messages: ~p", [OffMsgs]),
    {noreply, State};
handle_info({topic, NewTopic}, State) ->
    io:format("Chat topic is now: ~s~n", [NewTopic]),
    logger:info("Chat topic is now: ~s", [NewTopic]),
    {noreply, State};
handle_info({chat, From, Message, Timestamp}, State) ->
    io:format("[~p] ~s: ~s~n", [Timestamp, From, Message]),
    logger:info("[~p] ~s: ~s", [Timestamp, From, Message]),
    {noreply, State};
handle_info({entry, User}, State) ->
    io:format("User ~s has joined the chat.~n", [User]),
    logger:info("User ~s has joined the chat.", [User]),
    {noreply, State};
handle_info({exit, User}, State) ->
    io:format("User ~s has left the chat.~n", [User]),
    logger:info("User ~s has left the chat.", [User]),
    {noreply, State};
handle_info({private, From, To, Message, Timestamp}, State) ->
    io:format("[PRIVATE][~p] ~s -> ~s: ~s~n", [Timestamp, From, To, Message]),
    logger:info("[PRIVATE][~p] ~s -> ~s: ~s", [Timestamp, From, To, Message]),
    {noreply, State};
handle_info({private_sent, To, Message}, State) ->
    io:format("Private message sent to ~s: ~s~n", [To, Message]),
    logger:info("Private message sent to ~s: ~s", [To, Message]),
    {noreply, State};
handle_info({error, Reason}, State) ->
    io:format("Error: ~p~n", [Reason]),
    logger:info("Error: ~p", [Reason]),
    {noreply, State};
handle_info({kick_notice, Target}, State = #state{username = Username}) ->
    if
        Target =:= Username ->
            io:format("You have been kicked from the chat.~n", []),
            logger:info("You have been kicked from the chat."),
            {stop, normal, State};
        true ->
            {noreply, State}
    end;
handle_info(Other, State) ->
    io:format("Unknown message: ~p~n", [Other]),
    logger:info("Unknown message: ~p", [Other]),
    {noreply, State}.

%% @doc Called when the client is terminating.
-spec terminate(Reason :: term(), State :: #state{}) -> any().
terminate(_Reason, _State) ->
    ok.

%% @doc Handles code upgrades.
-spec code_change(OldVsn :: term(), State :: #state{}, Extra :: term()) ->
          {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
