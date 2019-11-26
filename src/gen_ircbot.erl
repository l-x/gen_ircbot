-module(gen_ircbot).
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2,  handle_info/2]).

%% API exports
-export([start_link/3, nick/1, nick/2, join/2, part/3, channels/1, privmsg/3, reply/3]).

-type mod_state() :: term().
-record(state, {
    server :: irc:server(),
    connection :: irc:connection(),
    mod :: module(),
    mod_state :: mod_state()
}).

%%====================================================================
%% callback functions
%%====================================================================
-callback init(Args :: term()) -> {ok, mod_state()}.

-callback handle_message(
    Command :: nonempty_string(), 
    Params :: [nonempty_string()], 
    Trail :: undefined | nonempty_string(), 
    Prefix :: {Nickname :: undefined | nonempty_string(), Username :: undefined | nonempty_string(), Host :: undefined | nonempty_string},
    State :: mod_state()
) -> {ok, mod_state()}.

%callback handle_command() -> {ok, mod_state()}

%%====================================================================
%% gen_server functions
%%====================================================================
-spec init(Args :: [Server :: irc:server()]) -> {ok, #state{}}.
%% @private
init([Module, Server, Args]) ->
    {ok, ModState} = Module:init(Args),
    {ok, Connection} = irc:connect(Server),

    {ok, #state{server = Server, connection = Connection, mod = Module, mod_state = ModState}}.

%% @private
handle_call(nick, _From, State) ->
    {reply, maps:get(nickname, State#state.server), State};
handle_call(channels, _From, State) ->
    {reply, maps:get(channels, State#state.server), State};
handle_call(_Message, _From, State) ->
    {reply, ignore, State}.

%% @private
handle_cast({nick, Nickname}, State) ->
    irc:nick(State#state.connection, Nickname),
    {noreply, State};
handle_cast({join, Channel}, State) ->
    irc:join(State#state.connection, Channel),
    {noreply, State};
handle_cast({part, Channel, Reason}, State) ->
    irc:part(State#state.connection, Channel, Reason),
    {noreply, State};
handle_cast({privmsg, Target, Message}, State) ->
    irc:privmsg(State#state.connection, Target, Message),
    {noreply, State};
handle_cast({reply, {_Command, [Target|_], _Trail, {Sender, _, _}}, Reply}, State) ->
    case maps:get(nickname, State#state.server) of
        Target -> irc:privmsg(State#state.connection, Sender, Reply);
        _ -> irc:privmsg(State#state.connection, Target, io_lib:format("~s: ~s", [Sender, Reply]))
    end,
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info({Event, _Socket, Message}, State) when Event == tcp; Event == ssl ->
    NewState = dispatch_messages(irc_message:parse(Message), State),
    {noreply, NewState};
handle_info({Event, _Socket}, _State) when Event == tcp_closed; Event == ssl_closed ->
    erlang:error(Event);
handle_info(_Info, State) ->
    {noreply, State}.

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Module :: module(), Server :: irc:server(), Args :: term()) -> {ok, Pid :: pid()} | {error, {already_started, Pid :: pid()}} | {error, Reason :: term()}.
start_link(Module, Server, Args) ->
    gen_server:start_link(?MODULE, [Module, Server, Args], []).

nick(Server) ->
    gen_server:call(Server, nick).

nick(Server, Nickname) ->
    gen_server:cast(Server, {nick, Nickname}).

join(Server, Channel) ->
    gen_server:cast(Server, {join, Channel}).

part(Server, Channel, Reason) ->
    gen_server:cast(Server, {part, Channel, Reason}).

channels(Server) ->
    gen_server:call(Server, channels).

privmsg(Server, Target, Message) ->
    gen_server:call(Server, {privmsg, Target, Message}).

reply(Server, {"PRIVMSG", _Params, _, _Prefix} = Message, Reply) ->
    gen_server:cast(Server, {reply, Message, Reply});
reply(_, _, _) ->
    {error, badmessage}.
%%====================================================================
%% Internal functions
%%====================================================================

dispatch_messages([Message|Messages], State) ->
    dispatch_messages(
        Messages, 
        dispatch_message(Message, State)
    );
dispatch_messages([], State) ->
    State.

dispatch_message(Message, State) ->
    try_handle_botcommand(
        bot_command:parse(Message),
        Message,
        try_handle_message(
            Message,
            handle_vital_messages(Message, State)
        )
    ).

handle_vital_messages({"PING", Params, Trail, _Prefix}, State) ->
    irc:pong(State#state.connection, case Trail of undefined -> hd(Params); T -> T end),
    State;
handle_vital_messages({"001", [Nickname|_], _Trail, _Prefix}, #state{server = Server} = State) ->
    State#state{server = Server#{nickname => Nickname}};
handle_vital_messages({"NICK", _Params, Nickname, {_, Username, _}}, #state{server = #{username := Username} = Server} = State) ->
    State#state{server = Server#{nickname => Nickname}};
handle_vital_messages({"JOIN", _Params, Channel, {_, Username, _}}, #state{server = #{username := Username} = Server} = State) ->
    ChannelSet = sets:from_list(maps:get(channels, Server, [])),
    Channels = sets:to_list(sets:add_element(Channel, ChannelSet)),
    State#state{server = Server#{channels => Channels}};
handle_vital_messages({"PART", _Params, Channel, {_, Username, _}}, #state{server = #{username := Username} = Server} = State) ->
    ChannelSet = sets:from_list(maps:get(channels, Server, [])),
    Channels = sets:to_list(sets:del_element(Channel, ChannelSet)),
    State#state{server = Server#{channels => Channels}};

handle_vital_messages(_Message, State) ->
    State.

try_handle_message({Command, Params, Trail, Prefix}, #state{mod = Mod, mod_state = ModState} = State) ->
    {ok, NewModState} = Mod:handle_message(Command, Params, Trail, Prefix, ModState),
    State#state{mod_state = NewModState}.

try_handle_botcommand({Command, Params}, Message, #state{mod = Mod, mod_state = ModState} = State) ->
    case erlang:function_exported(Mod, handle_command, 4) of
        false -> 
            State;
        true ->
            {ok, NewModState} = Mod:handle_command(Command, Params, Message, ModState),
            State#state{mod_state = NewModState}
    end;
try_handle_botcommand(_, _Message, State) ->
    State.
