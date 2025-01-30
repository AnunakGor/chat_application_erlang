%% File: chat_client.erl
-module(chat_client).

-export([start/2, stop/1, send_msg/2, send_private/3, list_clients/1, init/1]).

-record(client_state, {
    socket,
    username
}).

start(Host, Port) ->
    %% Connect to the server
    case gen_tcp:connect(Host, Port, [binary, {packet, line}, {active, true}]) of
        {ok, Socket} ->
            ClientPid = spawn_link(?MODULE, init, [Socket]),
            {ok, ClientPid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(ClientPid) ->
    ClientPid ! stop.

send_msg(ClientPid, Message) ->
    ClientPid ! {send, Message}.

send_private(ClientPid, ToUser, Message) ->
    ClientPid ! {send_private, ToUser, Message}.

list_clients(ClientPid) ->
    ClientPid ! list.

%%% Internal functions %%%
init(Socket) ->
    State = #client_state{socket = Socket},
    loop(State).

loop(State=#client_state{socket = Socket, username=Username}) ->
    receive
        stop ->
            gen_tcp:close(Socket),
            exit(normal);

        {send, Message} ->
            %% Send public message
            gen_tcp:send(Socket, Message ++ "\n"),
            loop(State);

        {send_private, ToUser, Message} ->
            %% Send private message
            Line = io_lib:format("/msg ~s ~s~n", [ToUser, Message]),
            gen_tcp:send(Socket, lists:flatten(Line)),
            loop(State);

        list ->
            %% Send the "list" command
            gen_tcp:send(Socket, "list\n"),
            loop(State);

        {tcp, Socket, Data} ->
            %% Server/broadcast data arrives here
            io:format("~s", [Data]),
            loop(State);

        {broadcast, Timestamp, From, Msg} ->
            io:format("~p ~s: ~s~n", [Timestamp, From, Msg]),
            loop(State);

        {private_msg, Timestamp, From, Msg} ->
            io:format("Private from ~s at ~p: ~s~n", [From, Timestamp, Msg]),
            loop(State);

        {tcp_closed, Socket} ->
            io:format("Connection closed.~n", []),
            exit(normal);

        {tcp_error, Socket, Reason} ->
            io:format("TCP error: ~p~n", [Reason]),
            exit(Reason);
            
        Other ->
            io:format("Unknown message: ~p~n", [Other]),
            loop(State)
    end.