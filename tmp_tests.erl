-module(tmp_tests).
-include_lib("eunit/include/eunit.hrl").
-eunit([{parallel, false}]).

%% Helper Functions

% Trying to stop server in case its running
maybe_stop_server() ->
    catch chat_server:stop(),
    ok.

%% Helper for Bob's process: Loop until the expected private message is received, then forward it to the TestClient.
bob_loop(TestClient) ->
    receive
        {private, "Alice", "Bob", "Secret message", _Ts} = Msg ->
            TestClient ! Msg;
        _Other ->
            bob_loop(TestClient)
    after 2000 ->
            ok
    end.

bob_receive_loop(TestClient) ->
    receive
        Msg ->
            TestClient ! {bob_msg, Msg},
            bob_receive_loop(TestClient)
    end.

bob_wait_for_error(TestClient) ->
    receive
        {bob_msg, {error, {muted_until, _UnmuteTime}}} = Msg ->
            Msg;
        {bob_msg, _Other} ->
            bob_wait_for_error(TestClient)
    after 2000 ->
         timeout
    end.

member(Element, List) ->
    lists:member(Element, List).
%% Test: Start and Stop Server
start_stop_test() ->
    maybe_stop_server(),
    {ok, _Pid} = chat_server:start_link(3, 10),
    ok = chat_server:stop(),
    ok.

%% Test: Client Connect and Duplicate Username Rejection.
connect_and_duplicate_test() ->
    maybe_stop_server(),
    {ok, _Pid} = chat_server:start_link(3, 10),
    TestClient = self(),
    {ok, History1} = chat_server:connect("Alice", TestClient),
    ?assertEqual([], History1),
    {error, username_taken} = chat_server:connect("Alice", TestClient),
    ok = chat_server:disconnect("Alice"),
    ok = chat_server:stop().

%% Test: Broadcast of Public Messages
broadcast_test() ->
    maybe_stop_server(),
    {ok, _Pid} = chat_server:start_link(3, 10),
    TestClient1 = self(),
    TestClient2 = spawn(fun() ->
                              receive
                                  Msg -> TestClient1 ! Msg
                              end
                          end),
    {ok, _History1} = chat_server:connect("Alice", TestClient1),
    {ok, _History2} = chat_server:connect("Bob", TestClient2),
    chat_server:send_message("Alice", "Hello, World!"),
    receive
        {chat, "Alice", "Hello, World!", _Timestamp} -> ok
    after 1000 ->
        ?assert(false)
    end,
    ok = chat_server:disconnect("Alice"),
    ok = chat_server:disconnect("Bob"),
    ok = chat_server:stop().


%% Test: private Messaging (Online Recipient)
private_message_online_test() ->
    maybe_stop_server(),
    {ok, _Pid} = chat_server:start_link(3, 10),
    TestClient = self(),
    TestClient2 = spawn(fun() -> bob_loop(TestClient) end),
    {ok, _History1} = chat_server:connect("Alice", TestClient),
    {ok, _History2} = chat_server:connect("Bob", TestClient2),
    chat_server:private_message("Alice", "Bob", "Secret message"),
    receive
        {private, "Alice", "Bob", "Secret message", _Ts} -> ok
    after 1000 ->
        ?assert(false)
    end,
    ok = chat_server:disconnect("Alice"),
    ok = chat_server:disconnect("Bob"),
    ok = chat_server:stop().

%% Test: setting Topic by Admin.
set_topic_test() ->
    maybe_stop_server(),
    {ok, _Pid} = chat_server:start_link(3, 10),
    TestClient = self(),
    {ok, _History1} = chat_server:connect("AdminUser", TestClient),
    ok = chat_server:set_topic("AdminUser", "New Topic"),
    Topic = chat_server:get_topic(),
    ?assertEqual("New Topic", Topic),
    ok = chat_server:disconnect("AdminUser"),
    ok = chat_server:stop().

%% Test:Kick a User by Admin
kick_test() ->
    maybe_stop_server(),
    {ok, _Pid} = chat_server:start_link(3, 10),
    TestClient = self(),
    {ok, _History1} = chat_server:connect("AdminUser", TestClient),
    {ok, _History2} = chat_server:connect("Bob", TestClient),
    ok = chat_server:kick("AdminUser", "Bob"),
    Clients = chat_server:list_clients(),
    ?assertNot(member("Bob", Clients)),
    ok = chat_server:disconnect("AdminUser"),
    ok = chat_server:stop().

%% To ensure that non-admin users cannot perform admin tasks
unauthorized_admin_test() ->
    maybe_stop_server(),
    {ok, _Pid} = chat_server:start_link(3, 10),
    TestClient = self(),

    {ok, _History1} = chat_server:connect("Alice", TestClient),
    {ok, _History2} = chat_server:connect("Bob", TestClient),
    
    Res1 = chat_server:set_topic("Bob", "New Topic"),
    ?assertEqual({error, "You are not allowed to change the topic."}, Res1),
    Res2 = chat_server:kick("Bob", "Alice"),
    ?assertEqual({error, "You are not allowed to kick users."}, Res2),
    Res3 = chat_server:mute("Bob", "Alice", 5),
    ?assertEqual({error, "You are not allowed to mute users."}, Res3),
    Res4 = chat_server:promote("Bob", "Bob"),
    ?assertEqual({error, "You are not allowed to promote users."}, Res4),
    ok = chat_server:disconnect("Alice"),
    ok = chat_server:disconnect("Bob"),
    ok = chat_server:stop().

%% Test to check if double disconnects throw an error
double_disconnect_test() ->
    maybe_stop_server(),
    {ok, _Pid} = chat_server:start_link(3, 10),
    TestClient = self(),
    {ok, _History} = chat_server:connect("Alice", TestClient),
    ok = chat_server:disconnect("Alice"),
    Res = chat_server:disconnect("Alice"),
    ?assertEqual({error, not_connected}, Res),
    ok = chat_server:stop().

%% To check that if a client who is muted, reconnects after disconnecting in the mute period, is treated as muted
mute_persistence_test() ->
    maybe_stop_server(),
    {ok, _Pid} = chat_server:start_link(3, 10),
    TestClient = self(),  
  
    {ok, _HistoryAdmin} = chat_server:connect("AdminUser", TestClient),
    BobProcess = spawn(fun() -> bob_receive_loop(TestClient) end),
    {ok, _HistoryBob} = chat_server:connect("Bob", BobProcess),
    ok = chat_server:mute("AdminUser", "Bob", 5),
    timer:sleep(200), 

    chat_server:send_message("Bob", "Hello, world!"),
    timer:sleep(200), 
    ReceivedError = bob_wait_for_error(TestClient),
    ?assertNotEqual(timeout, ReceivedError),
    ok = chat_server:disconnect("Bob"),
    
    {ok, _HistoryBob2} = chat_server:connect("Bob", BobProcess),
    
    chat_server:send_message("Bob", "I'm back!"),
    timer:sleep(200),
    
    ReceivedError2 = bob_wait_for_error(TestClient),
    ?assertNotEqual(timeout, ReceivedError2),

    ok = chat_server:disconnect("AdminUser"),
    ok = chat_server:disconnect("Bob"),
    ok = chat_server:stop().


