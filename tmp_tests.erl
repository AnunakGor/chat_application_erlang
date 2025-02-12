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


