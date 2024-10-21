-module(hello_world_root).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Flags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    Children = [],
    {ok, {Flags, Children}}.
