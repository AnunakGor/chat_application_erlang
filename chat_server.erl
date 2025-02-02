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
    server_info/0
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
    capacity,        % maximum number of clients allowed
    history_count,   % number of past messages to send to a new client
    clients = #{},   % map: Username => ClientPid
    history = []     % list of messages (most recent first)
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

init({Capacity, HistoryCount}) ->
    {ok, #state{capacity = Capacity, history_count = HistoryCount}}.

handle_call({connect, Username, ClientPid}, _From, State = #state{
    clients = Clients, capacity = Capacity, history = History
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
                    NewClients = Clients#{Username => ClientPid},
                    broadcast({entry, Username}, NewClients),
                    HistoryCount = State#state.history_count,
                    LastMessages = lists:sublist(lists:reverse(History), HistoryCount),
                    ClientPid ! {history, LastMessages},
                    {reply, {ok, LastMessages}, State#state{clients = NewClients}}
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

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({send_message, Username, Message}, State = #state{
    clients = Clients, history = History
}) ->
    Timestamp = erlang:system_time(second),
    Msg = {chat, Username, Message, Timestamp},
    NewHistory = [Msg | History],
    broadcast(Msg, Clients),
    {noreply, State#state{history = NewHistory}};

handle_cast({private_message, From, To, Message}, State = #state{
    clients = Clients, history = History
}) ->
    Timestamp = erlang:system_time(second),
    Msg = {private, From, To, Message, Timestamp},
    case maps:find(To, Clients) of
        error ->
            case maps:find(From, Clients) of
                {ok, FromPid} ->
                    FromPid ! {error, {user_not_found, To}},
                    ok;
                error ->
                    ok
            end,
            {noreply, State};
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
    lists:foreach(
      fun({_Username, Pid}) ->
              io:format("Sending message to: ~p~n", [Pid]),  
              Pid ! Message
      end,
      maps:to_list(Clients)
    ).