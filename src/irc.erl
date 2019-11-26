%%% @private

-module(irc).

-include_lib("gen_ircbot.hrl").

-define(SOCKET_OPTS, [{active, true}, {packet, 0}, {send_timeout, 5000}, {send_timeout_close, true}]).

%% API exports
-export([connect/1, send/2, ping/2, pong/2, nick/2, join/2, part/3, privmsg/3]).
-export_type([server/0, connection/0]).

%% API types and records
-type server() :: #{
    host := nonempty_string(),
    port := pos_integer(),
    ssl => boolean(),
    username := nonempty_string(),
    realname := nonempty_string(),
    nickname := nonempty_string(),
    password => nonempty_string(),
    channels => [nonempty_string()],
    ping_interval => pos_integer()
}.

-opaque connection() :: {SocketMod :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:socket()}.

%% Internal types and records

%%====================================================================
%% API functions
%%====================================================================

-spec connect(Server :: server()) -> {ok, connection()} | {error, Reason :: term()}.
connect(Server) ->
    {ok, Connection} = connect_socket(Server),
    ok = register_connection(Server, Connection),
    {ok, Connection}.

-spec send(Connection :: connection(), Data :: nonempty_string()) -> ok.
send({SocketMod, Socket}, Data) ->
    SocketMod:send(Socket, Data ++ ?IRC_CRLF).

-spec ping(Connection :: connection(), Server :: nonempty_string()) ->ok.
ping(Connection, Server) ->
    send(Connection, format_cmd(ping, ":~s", [Server])).

-spec pong(Connection :: connection(), Server :: nonempty_string()) ->ok.
pong(Connection, Server) ->
    send(Connection, format_cmd(pong, ":~s", [Server])).

-spec nick(Connection :: connection(), Nickname :: nonempty_string()) -> ok.
nick(Connection, Nickname) -> 
    send(Connection, format_cmd(nick, "~s", [Nickname])).

-spec join(Connection :: connection(), Channel :: nonempty_string()) -> ok.
join(Connection, Channel) ->
    send(Connection, format_cmd(join, "~s", [Channel])).

-spec part(Connection :: connection(), Channel :: nonempty_string(), Reason :: nonempty_string()) -> ok.
part(Connection, Channel, Reason) ->
    send(Connection, format_cmd(part, "~s :~s", [Channel, Reason])).

-spec privmsg(Connection :: connection(), Target :: nonempty_string(), Message :: nonempty_string()) -> ok.
privmsg(Connection, Target, Message) ->
    send(Connection, format_cmd(privmsg, "~s :~s", [Target, Message])).

%%====================================================================
%% Internal functions
%%====================================================================
connect_socket(#{host := Host, port := Port} = Server) ->
    SocketMod = case maps:get(ssl, Server, false) of
        true -> ssl;
        false -> gen_tcp
    end,

    case SocketMod:connect(Host, Port, ?SOCKET_OPTS) of
        {ok, Socket} ->
            {ok, {gen_tcp, Socket}};
        {error, _Reason} -> % @todo Implement better backoff strategy with a max. retry count
            timer:sleep(15000),
            connect_socket(Server)
    end.

register_connection(#{username := Username, realname := Realname, nickname := Nickname} = Server, Connection) ->
    case maps:get(password, Server, undefined) of
        undefined -> ok;
        P -> send(Connection, format_cmd(pass, "~s", [P]))
    end,
    send(Connection, format_cmd(user, "~s 0 @ :~s", [Username, Realname])),
    nick(Connection, Nickname),

    lists:foreach(
        fun (Channel) -> join(Connection, Channel) end,
        maps:get(channels, Server, [])
    ),

    case maps:get(ping_interval, Server, false) of
        false -> ok;
        I -> timer:apply_interval(I * 1000, ?MODULE, ping, [Connection, Realname])
    end,
    
    ok.

format_cmd(Command, Format, Args) when is_atom(Command) ->
    format_cmd(atom_to_list(Command), Format, Args);
format_cmd(Command, Format, Args) when is_list(Command) ->
    io_lib:format("~s " ++ Format, [string:uppercase(Command)|Args]).
