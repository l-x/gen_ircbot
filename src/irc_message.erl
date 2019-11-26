%%% @private
-module(irc_message).

-include_lib("gen_ircbot.hrl").

-export([parse/1]).

parse(RawMessage) ->
    parse(string:split(RawMessage, ?IRC_CRLF, all), []).

parse([RawMessage|Rest], Messages) ->
    case re:run(RawMessage, "^(:((?P<nickname>\\S+)!\~)?((?P<username>\\S+)@)?(?P<host>\\S+)? )?(?P<command>\\S+)( (?!:)(?P<params>.+?))?( :(?P<trail>.+))?$", [{capture, ["nickname", "username", "host", "command", "params", "trail"], list}]) of
        nomatch -> 
            parse(Rest, Messages);
        {match, [Nickname, Username, Hostname, Command, Params, Trail]} ->
            Message = {Command, params(Params), trail(Trail), prefix(Nickname, Username, Hostname)},
            parse(Rest, [Message|Messages])
    end;
parse([], Messages) ->
    lists:reverse(Messages).

params(Params) ->
    case Params of
        [] -> [];
        P -> string:tokens(P, " ")
    end.

trail(Trail) ->
    case Trail of
        [] -> undefined;
        T -> T
    end.

prefix(Nickname, Username, Host) ->
    {
        case Nickname of [] -> undefined; _ -> Nickname end,
        case Username of [] -> undefined; _ -> Username end,
        case Host of [] -> undefined; _ -> Host end
    }.
