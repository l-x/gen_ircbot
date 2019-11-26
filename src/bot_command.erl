%%% @private

-module(bot_command).
-export([parse/2]).

parse({"PRIVMSG", [Target], Privmsg, {Sender, _, _}}, OwnNick) ->
    case re:run(Privmsg, "^(!(?P<command>\\w+))(?P<params>.*)?$", [{capture, ["command", "params"], list}]) of
        nomatch ->
            nomatch;
        {match, [Command, Params]} ->
            {
                string:lowercase(Command), 
                case Params of [] -> 
                    undefined; 
                    P -> string:trim(P) 
                end,
                case Target of
                    OwnNick -> {direct, OwnNick, Sender};
                    Channel -> {channel, Channel, Sender}
                end
            }
    end;
parse(_Message, _OwnNick) ->
    nomatch.
