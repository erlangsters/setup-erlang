-module(hello_handler).
-export([init/2]).

init(Request1, State) ->
    {ok, Name, Request2} = cowboy_req:read_body(Request1),
    Response = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello ", Name/binary, "!\n">>,
        Request2
    ),

    {ok, Response, State}.
