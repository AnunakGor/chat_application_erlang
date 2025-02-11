# Erlang Chat Application

This is a chat application made using Erlang and `gen_server`. It supports a variety of features, including the following:

### Client Connection Management
- **Maximum Clients:** The server is configured to allow a configurable maximum number of simultaneous clients.
- **Username Uniqueness:** Duplicate usernames are rejected to ensure that each connected client is uniquely identifiable.
- **Admin Assignment:** The first client to connect automatically becomes the admin. Additional clients join as regular users until promoted.

  **Commands:**
  - **Start a Client:**
    ```erlang
    chat_client:start("Alice").
    ```

### Public Messaging
- **Broadcasting:** Public messages are broadcast to all connected clients.
- **Message History:** The server maintains a history of recent public messages. When a new client connects, it receives a configurable number of previous messages, so it can catch up on the conversation.

  **Commands:**
  - **Sending a Public Message:**  
    ```erlang
    chat_client:send("Hello, everyone!").
    ```

### Private Messaging
- **Direct Messages:** Clients can send private messages directly to another user.
- **Offline Message Storage:** If a client is offline, any private messages sent to them are stored on the server. When the client reconnects, these offline messages are delivered automatically.

  **Commands:**
  - **Sending a Private Message:**  
    ```erlang
    chat_client:send_private("Bob", "Hi Bob, this is a private message.").
    ```

### Chatroom Topic Management
- **Viewing and Updating the Topic:** Clients can view the current chatroom topic at any time.
- **Admin-Only Changes:** By default, only admin users have the permission to change the topic. When updated, all connected clients receive the new topic.

  **Commands:**
  - **Setting the Topic:**  
    ```erlang
    chat_client:set_topic("New Chatroom Topic").
    ```
  - **Getting the Current Topic:**  
    ```erlang
    chat_client:get_topic().
    ```

### Administrative Commands
- **Kick:** Admins can forcibly disconnect a user from the chat. The kicked user gets disconnected from the server.

  **Command:**
  - **Kick a Client:**  
    ```erlang
    chat_client:kick("Bob").
    ```

- **Mute/Unmute:** Admins can mute a user for a specified duration(in seconds). Muted users cannot send public messages but can still send private messages.

  **Commands:**
  - **Mute a Client:**  
    ```erlang
    chat_client:mute("Bob", 300).
    ```
  - **Unmute a Client:**  
    ```erlang
    chat_client:unmute("Bob").
    ```

- **Promote:** Admins can promote a regular user to admin status.

  **Command:**
  - **Promote a User to Admin:**  
    ```erlang
    chat_client:promote("Charlie").
    ```

- **Retrieve Admin List:** The server can provide a list of all current admin users.

  **Command:**
  - **Retrieve Admin List:**  
    ```erlang
    chat_client:get_admins().
    ```

### Disconnecting
- **Disconnecting from the Server:**  
  ```erlang
  chat_client:disconnect().

### Compilation

1. Compile the modules:
   ```bash
   erlc -o ebin chat_server.erl chat_client.erl
   ```

### Running the Server and Clients
1. **Start the Server:**
   - Open a terminal and start an Erlang shell with a node name and a cookie:
     ```bash
     erl -sname server -setcookie mycookie -pa ebin -pa src
     ```
   - Start the chat server with desired parameters (e.g., maximum 5 clients and a history of 5 messages):
     ```erlang
     {ok, _Pid} = chat_server:start_link(5, 5).
     ```

2. **Start a Client:**
   - Open a new terminal for each client node:
     ```bash
     erl -sname alice -setcookie mycookie -pa ebin -pa src
     ```
   - Connect as a client (replace `"Alice"` with the desired username):
     ```erlang
     chat_client:start("Alice").
     ```
   - You can open additional terminals to simulate multiple clients (e.g., Bob, Charlie).

---
## Internal Functionalities

- **Message History Storage:**  
  The server maintains a history (with a configurable limit) of the most recent public messages. When a new client connects, it automatically receives this history to help it catch up with the ongoing conversation.

- **Offline Private Messages:**  
  Private messages sent to offline users are stored on the server. Once the offline user reconnects, these messages are delivered immediately along with any connection-related notifications.
