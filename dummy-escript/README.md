# Dummy project - An escript

A dummy Erlang project using rebar3. It's used to verify that Erlang projects
compiles successfully and can be executed just fine.

It implements the "hello_world" program (an "escript") that uses
[gun](https://ninenines.eu/docs/en/gun/2.1/guide/) library to make a HTTP
request (to a HTTP server running locally).

It's meant to be used in pair with the
[other dummy Erlang project](../dummy-release/) which implements the server
side.
