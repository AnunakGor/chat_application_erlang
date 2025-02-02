-module(chat_client).

-export([start/1, send/2, send_private/3, list_clients/0, disconnect/1, loop/1]).

start(Username) ->
    spawn(fun() -> init_client(Username) end).

init_client(Username) ->
    Self = self(),
    case chat_server:connect(Username, Self) of
        {ok, History} ->
            io:format("Connected as ~s.~nPrevious chat history: ~p~n", [Username, History]),
            loop(Username);
        {error, Reason} ->
            io:format("Failed to connect as ~s: ~p~n", [Username, Reason])
    end.

send(Username, Message) ->
    chat_server:send_message(Username, Message).

send_private(From, To, Message) ->
    chat_server:private_message(From, To, Message).

list_clients() ->
    Clients = chat_server:list_clients(),
    io:format("Connected clients: ~p~n", [Clients]),
    Clients.

disconnect(Username) ->
    chat_server:disconnect(Username).

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
        Other ->
            io:format("Unknown message: ~p~n", [Other]),
            loop(Username)
    end.