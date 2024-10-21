-module(hello_world).
-export([main/1]).

main([Name]) ->
    application:ensure_all_started(gun),

    {ok, Connection} = gun:open("localhost", 8080),
    {ok, _Protocol} = gun:await_up(Connection),

    BodyIn = erlang:list_to_binary(Name),
    HeadersIn = [
        {<<"content-length">>, integer_to_binary(size(BodyIn))},
        {<<"content-type">>, "application/json"}
    ],
    Stream = gun:post(Connection, "/hello", HeadersIn, BodyIn),

    {response, nofin, 200, _HeadersOut} = gun:await(Connection, Stream),
    {ok, Body} = gun:await_body(Connection, Stream),
    io:format("~s~n", [erlang:binary_to_list(Body)]),

    ok = gun:close(Connection),

    erlang:halt(0).
