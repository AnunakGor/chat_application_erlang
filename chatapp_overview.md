
# Erlang Chat Application

This is a chat application made using erlang and gen_server. It supports a variety of features, including:

- **Client Connection Management:**
  - Configurable maximum simultaneous clients.
  - Duplicate username rejection.
  - Automatic admin assignment (the first client becomes admin by default).

- **Public Messaging:**
  - Broadcasting messages to all connected clients.
  - Maintaining a history of messages.

- **Private Messaging:**
  - Sending private messages.
  - Offline message storage (messages delivered when a user reconnects).

- **Chatroom Topic Management:**
  - Viewing and setting the chatroom topic.
  - *By default, only admins can change the topic.*

- **Administrative Commands:**
  - **Kick:** Admins can kick a user from the chat. Kicked users are forcibly disconnected.
  - **Mute/Unmute:** Admins can mute a user for a specified duration (muted users cannot send public messages but can still send private messages).
  - **Promote:** Admins can promote a normal user to admin.
  - **Get Admins:** Retrieve the list of current admin users.


## Steps of implementation  

- Open a terminal and start an Erlang shell with a node name and set the cookie:
  ```cmd
    erl -sname server -setcookie mycookie -pa ebin -pa src
    {ok, _Pid} = chat_server:start_link(5, 5).
- Open a new terminal and start an erlang shell for a client node
  ```cmd  
    erl -sname alice -setcookie mycookie -pa ebin -pa src
    chat_client:start("Alice").
In this way a server is created and a client is connected to that server. Multiple terminals can be used to simulate multiple clients.



## Functionalities

- **Public Messaging**: 
  In a client shell, 
    ```cmd
    chat_client:send("Hello, everyone!").
  
- **Private Messaging**: 
  Send a private message to a specific client
    ```cmd
    chat_client:send_private("Bob", "Hi Bob, this is a private message.").

- **Chatroom topic management**: By default, admins can only change the topic. All clients receive the updated topic. Any client can check what the current topic is.
  ```cmd
  chat_client:set_topic("New Chatroom Topic").

  chat_client:get_topic().

- **Admin Commands**: 
  - Kick a client
  - Mute a client
  - Unmute a client
  - Promote a User to Admin
  ```cmd
  chat_client:kick("Bob").
  chat_client:mute("Bob", 300).
  chat_client:unmute("Bob").
  chat_client:promote("Charlie").

  
- Disconnect a client
  ```cmd
  chat_client:disconnect().


