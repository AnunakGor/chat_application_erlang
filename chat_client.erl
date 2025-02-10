-module(chat_client).

-export([
    start/1,
    send/1,
    send_private/2,
    list_clients/0,
    disconnect/0,
    loop/1,
    get_server_node/1,
    set_topic/1,
    kick/1,
    mute/2,
    unmute/1,
    promote/1,
    get_topic/0,
    get_admins/0
]).

%% @doc Starts the chat client with a given username.
-spec start(string()) -> {ok, pid()} | {error, already_running}.
start(Username) ->
    case whereis(chat_client) of
        undefined ->
            Pid = spawn(fun() -> init_client(Username) end),
            register(chat_client, Pid),
            {ok, Pid};
        _ ->
            io:format("A chat client is already running on this node. Cannot start another.~n"),
            {error, already_running}
    end.

%% @doc Constructs the server node name from a given server short name.
-spec get_server_node(string()) -> atom().
get_server_node(SName) ->
    {ok, Hostname} = inet:gethostname(),
    FullName = SName ++ "@" ++ Hostname,
    list_to_atom(FullName).

%% @doc Initializes the chat client process and connects to the server.
-spec init_client(string()) -> no_return().
init_client(Username) ->
    ServerNode = get_server_node("server"),
    case net_adm:ping(ServerNode) of
        pong ->
            io:format("Successfully connected to server node ~p~n", [ServerNode]);
        pang ->
            io:format("WARNING: Could not connect to server node ~p. Check that the server is running and reachable.~n", [ServerNode])
    end,
    timer:sleep(1000),
    Self = self(),
    case chat_server:connect(Username, Self) of
        {ok, History} ->
            io:format("Connected as ~s.~nPrevious chat history: ~p~n", [Username, History]),
            loop(Username);
        {error, Reason} ->
            io:format("Failed to connect as ~s: ~p~n", [Username, Reason]),
            loop(Username)
    end.

%% @doc Sends a public message via the chat client.
-spec send(string()) -> ok | {error, not_started}.
send(Message) ->
    case whereis(chat_client) of
        undefined ->
            io:format("No chat client running on this node. Please start one first.~n"),
            {error, not_started};
        _ ->
            chat_client ! {send, Message},
            ok
    end.

%% @doc Sends a private message to a specified user.
-spec send_private(string(), string()) -> ok | {error, not_started}.
send_private(To, Message) ->
    case whereis(chat_client) of
        undefined ->
            io:format("No chat client running on this node. Please start one first.~n"),
            {error, not_started};
        _ ->
            chat_client ! {send_private, To, Message},
            ok
    end.

%% @doc Lists all connected clients by querying the chat server.
-spec list_clients() -> [string()].
list_clients() ->
    Clients = chat_server:list_clients(),
    io:format("Connected clients: ~p~n", [Clients]),
    Clients.

%% @doc Disconnects the chat client from the server.
-spec disconnect() -> ok | {error, not_started}.
disconnect() ->
    case whereis(chat_client) of
        undefined ->
            io:format("No chat client running on this node.~n"),
            {error, not_started};
        _ ->
            chat_client ! disconnect,
            ok
    end.

%% @doc Sends a request to change the chat topic.
-spec set_topic(string()) -> ok.
set_topic(NewTopic) ->
    chat_client ! {set_topic, NewTopic},
    ok.

%% @doc Sends a kick request for a specified user.
-spec kick(string()) -> ok.
kick(Target) ->
    chat_client ! {kick, Target},
    ok.

%% @doc Sends a mute request for a specified user for a given duration (in seconds).
-spec mute(string(), pos_integer()) -> ok.
mute(Target, Duration) ->
    chat_client ! {mute, Target, Duration},
    ok.

%% @doc Sends an unmute request for a specified user.
-spec unmute(string()) -> ok.
unmute(Target) ->
    chat_client ! {unmute, Target},
    ok.

%% @doc Sends a request to promote a specified user to admin.
-spec promote(string()) -> ok.
promote(Target) ->
    chat_client ! {promote, Target},
    ok.

%% @doc Retrieves the current chat topic from the server.
-spec get_topic() -> string().
get_topic() ->
    Topic = chat_server:get_topic(),
    io:format("Current topic: ~s~n", [Topic]),
    Topic.

%% @doc Retrieves the list of admin users from the server.
-spec get_admins() -> [string()].
get_admins() ->
    Admins = chat_server:get_admins(),
    io:format("Admin users: ~p~n", [Admins]),
    Admins.

%% @doc The main loop for handling incoming messages for the chat client.
-spec loop(string()) -> no_return().
loop(Username) ->
    receive
        {history, History} ->
            io:format("Received chat history: ~p~n", [History]),
            loop(Username);
        {offline, OffMsgs} ->
            io:format("You have offline private messages: ~p~n", [OffMsgs]),
            loop(Username);
        {topic, NewTopic} ->
            io:format("Chat topic is now: ~s~n", [NewTopic]),
            loop(Username);
        {chat, From, Message, Timestamp} ->
            io:format("[~p] ~s: ~s~n", [Timestamp, From, Message]),
            loop(Username);
        {entry, User} ->
            io:format("User ~s has joined the chat.~n", [User]),
            loop(Username);
        {exit, User} ->
            io:format("User ~s has left the chat.~n", [User]),
            loop(Username);
        {private, From, To, Message, Timestamp} ->
            io:format("[PRIVATE][~p] ~s -> ~s: ~s~n", [Timestamp, From, To, Message]),
            loop(Username);
        {private_sent, To, Message} ->
            io:format("Private message sent to ~s: ~s~n", [To, Message]),
            loop(Username);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            loop(Username);
        {send, Message} ->
            chat_server:send_message(Username, Message),
            loop(Username);
        {send_private, To, Message} ->
            chat_server:private_message(Username, To, Message),
            loop(Username);
        disconnect ->
            chat_server:disconnect(Username),
            io:format("Disconnected from chat server.~n", []),
            unregister(chat_client),
            ok;
        {set_topic, NewTopic} ->
            Reply = chat_server:set_topic(Username, NewTopic),
            io:format("Set topic reply: ~p~n", [Reply]),
            loop(Username);
        {kick, Target} ->
            Reply = chat_server:kick(Username, Target),
            io:format("Kick reply: ~p~n", [Reply]),
            loop(Username);
        {mute, Target, Duration} ->
            Reply = chat_server:mute(Username, Target, Duration),
            io:format("Mute reply: ~p~n", [Reply]),
            loop(Username);
        {unmute, Target} ->
            Reply = chat_server:unmute(Username, Target),
            io:format("Unmute reply: ~p~n", [Reply]),
            loop(Username);
        {promote, Target} ->
            Reply = chat_server:promote(Username, Target),
            io:format("Promote reply: ~p~n", [Reply]),
            loop(Username);
        {kick_notice, Target} ->
            if Target =:= Username ->
                io:format("You have been kicked from the chat.~n", []),
                unregister(chat_client),
                exit(normal);
               true ->
                loop(Username)
            end;
        Other ->
            io:format("Unknown message: ~p~n", [Other]),
            loop(Username)
    end.


