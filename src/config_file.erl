-module(config_file).
-include("common.hrl").


-export([parse_config_file/0]).

-define(CONFIG_FILE, "etc/rotor.conf").

%%%% ---------------------------------------------------------------------------

parse_config_file() ->
    {ok, Binary} = file:read_file(?CONFIG_FILE),
    {ok, Tokens, _} = config_lexer:string(binary_to_list(Binary)),
    config_parser:parse(Tokens).
