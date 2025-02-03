-module(chat_client).                    
  
-export([start/1, send/1, send_private/2, list_clients/0, disconnect/0, loop/1]).

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

init_client(Username) ->
    Self = self(),
    case chat_server:connect(Username, Self) of
        {ok, History} ->
            io:format("Connected as ~s.~nPrevious chat history: ~p~n", [Username, History]),
            loop(Username);
        {error, Reason} ->
            io:format("Failed to connect as ~s: ~p~n", [Username, Reason])
    end.

send(Message) ->
    case whereis(chat_client) of
        undefined ->
            io:format("No chat client running on this node. Please start one first.~n"),
            {error, not_started};
        _ ->
            chat_client ! {send, Message},
            ok
    end.

send_private(To, Message) ->
    case whereis(chat_client) of
        undefined ->
            io:format("No chat client running on this node. Please start one first.~n"),
            {error, not_started};
        _ ->
            chat_client ! {send_private, To, Message},
            ok
    end.

list_clients() ->
    Clients = chat_server:list_clients(),
    io:format("Connected clients: ~p~n", [Clients]),
    Clients.

disconnect() ->
    case whereis(chat_client) of
        undefined ->
            io:format("No chat client running on this node.~n"),
            {error, not_started};
        _ ->
            chat_client ! disconnect,
            ok
    end.

loop(Username) ->
    receive
        {history, History} ->
            io:format("Received chat history: ~p~n", [History]),
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
            io:format("Disconnected from chat server.~n"),
            unregister(chat_client),
            ok;
        Other ->
            io:format("Unknown message: ~p~n", [Other]),
            loop(Username)
    end.
