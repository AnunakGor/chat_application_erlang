
-module(chat_server).
-behaviour(gen_server).

%% -include_lib("kernel/include/logger.hrl").

-define(INFO_MSG(Format, Args), io:format(Format, Args)).
-define(ERROR_MSG(Format, Args), io:format(Format, Args)).

-export([
    start_link/3,
    stop/0,
    get_clients/0,
    get_chat_history/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    listener_socket,       
    max_clients   = 3,     
    history_size  = 10,    
    clients       = [],    
    chat_history  = []     
}).

start_link(Port, MaxClients, HistorySize) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, 
                          {Port, MaxClients, HistorySize}, []).

stop() ->
    gen_server:call(?MODULE, stop).

get_clients() ->
    gen_server:call(?MODULE, get_clients).

get_chat_history() ->
    gen_server:call(?MODULE, get_chat_history).

init({Port, MaxClients, HistorySize}) ->
    process_flag(trap_exit, true),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, 
                                        {active, false}, {reuseaddr, true}]),
    ?INFO_MSG("Server listening on port ~p~n", [Port]),
    
    spawn_link(fun() -> accept_loop(LSock) end),
    {ok, #state{
            listener_socket = LSock,
            max_clients = MaxClients,
            history_size = HistorySize
         }
    }.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(get_clients, _From, State=#state{clients=Clients}) ->
    %% returning usernames
    Usernames = [Username || {Username, _Pid} <- Clients],
    {reply, Usernames, State};

handle_call(get_chat_history, _From, State=#state{chat_history=History}) ->
    {reply, History, State};

handle_call({try_add_client, Username, Pid}, _From, 
            State=#state{clients=Clients, max_clients=MaxClients}) ->
    %% checking if username is already taken
    % case lists:any(fun({Name, _}) -> Name =:= Username end, Clients) of
    case lists:any(fun({Name, _}) -> Name =:= Username end, Clients) of
        true ->
            {reply, {error, "Username already taken"}, State};
        false ->
            %% checking max capacity
            case length(Clients) >= MaxClients of
                true ->
                    {reply, {error, "Server is at max capacity. Try later."}, State};
                false ->
                    {reply, ok, State#state{clients = [{Username, Pid} | Clients]}}
            end
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({remove_client, Username}, State=#state{clients=Clients}) ->
    NewClients = lists:filter(fun({Name, _}) -> Name =/= Username end, Clients),
    {noreply, State#state{clients = NewClients}};
handle_cast({send_history, Socket}, State=#state{chat_history=History, history_size=N}) ->
    %% last N messages to new client
    LastN = lists:sublist(lists:reverse(History), 1, N),
    ReversedBack = lists:reverse(LastN),
    lists:foreach(
      fun({Ts, From, Text}) ->
              S = io_lib:format("~p ~s: ~s~n", [Ts, From, Text]),
              gen_tcp:send(Socket, S)
      end,
      ReversedBack
    ),
    {noreply, State};

handle_cast({store_broadcast, From, Msg}, State=#state{clients=Clients, chat_history=Hist}) ->
    Timestamp = erlang:localtime(),
    NewHist = [{Timestamp, From, Msg} | Hist],
    lists:foreach(
      fun({_Name, ClientPid}) ->
        ClientPid ! {broadcast, Timestamp, From, Msg}
      end,
      Clients
    ),
    {noreply, State#state{chat_history = NewHist}};

handle_cast({private_message, From, To, Msg}, State=#state{clients=Clients}) ->
    case lists:keyfind(To, 1, Clients) of
        false ->
            case lists:keyfind(From, 1, Clients) of
                {From, SenderPid} ->
                    SenderPid ! {private_error, To, "User not found"};
                _ ->
                    ok
            end,
            {noreply, State};
        {To, TargetPid} ->
            Timestamp = erlang:localtime(),
            TargetPid ! {private_msg, Timestamp, From, Msg},
            
            {noreply, State}
    end;

handle_cast(_Other, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


accept_loop(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            spawn_link(fun() -> client_session(Socket) end),
            accept_loop(LSock);
        {error, Reason} ->
            ?ERROR_MSG("Accept error: ~p~n", [Reason]),
            ok
    end.

client_session(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    
    gen_tcp:send(Socket, <<"Enter your username:\n">>), %% asking client for username
    case recv_line(Socket) of
        {ok, Username} ->
            CleanName = string:trim(Username),
            case try_add_client(CleanName, self()) of
                ok ->
                    %% sending last N messages to new Client
                    send_history(Socket),
                    broadcast_join(CleanName),
                    loop(Socket, CleanName);
                {error, Reason} ->
                    gen_tcp:send(Socket, io_lib:format("~s~n", [Reason])),
                    gen_tcp:close(Socket)
            end;
        {error, _} ->
            gen_tcp:close(Socket)
    end.
    % return.

loop(Socket, Username) ->
    receive
        {tcp, Socket, Data} ->
            inet:setopts(Socket, [{active, once}]),
            Line = string:trim(binary_to_list(Data)),
            handle_client_message(Line, Username, Socket),
            loop(Socket, Username);

        {tcp_closed, Socket} ->
            remove_client(Username),
            broadcast_exit(Username);

        {tcp_error, Socket, Reason} ->
            ?ERROR_MSG("Socket error ~p for ~p~n", [Reason, Username]),
            remove_client(Username),
            broadcast_exit(Username)
    end.

recv_line(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {ok, binary_to_list(Data)};
        {error, Reason} ->
            {error, Reason}
    end.

handle_client_message(Line, Username, Socket) ->
    case parse_command(Line) of
        {msg, To, MsgText} ->
            private_message(Username, To, MsgText);
        
        %% list of connected users
        list ->
            ListOfUsers = get_clients(),
            gen_tcp:send(Socket, io_lib:format("Connected users: ~p~n", [ListOfUsers]));
        
        {public, Text} ->
            store_and_broadcast(Username, Text)
    end.

parse_command(Line) ->
    case string:tokens(Line, " ") of
        ["/msg", ToUser | Rest] ->
            Msg = string:join(Rest, " "),
            {msg, ToUser, Msg};
        ["list"] ->
            list;
        _ ->
            {public, Line}
    end.

try_add_client(Username, Pid) ->
    gen_server:call(?MODULE, {try_add_client, Username, Pid}).

remove_client(Username) ->
    gen_server:cast(?MODULE, {remove_client, Username}).

broadcast_join(Username) ->
    BroadcastMsg = io_lib:format("~s has joined the chat~n", [Username]),
    store_and_broadcast("SERVER", lists:flatten(BroadcastMsg)).

broadcast_exit(Username) ->
    BroadcastMsg = io_lib:format("~s has left the chat~n", [Username]),
    store_and_broadcast("SERVER", lists:flatten(BroadcastMsg)).

private_message(From, To, Msg) ->
    gen_server:cast(?MODULE, {private_message, From, To, Msg}).

store_and_broadcast(From, Msg) ->
    gen_server:cast(?MODULE, {store_broadcast, From, Msg}).

send_history(Socket) ->
    gen_server:cast(?MODULE, {send_history, Socket}).