%%% @private

-module(bot_command).
-export([parse/1]).

parse({"PRIVMSG", _Params, Privmsg, _Prefix}) ->
    case re:run(Privmsg, "^(!(?P<command>\\w+))(?P<params>.*)?$", [{capture, ["command", "params"], list}]) of
        nomatch ->
            nomatch;
        {match, [Command, Params]} ->
            {
                string:lowercase(Command), 
                case Params of [] -> 
                    undefined; 
                    P -> string:trim(P) 
                end
            }
    end;
parse(_Message) ->
    nomatch.
