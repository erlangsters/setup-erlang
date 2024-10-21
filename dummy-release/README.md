# Dummy project - An Erlang release (xrel)

A dummy Erlang project using rebar3. It's used to verify that Erlang projects
compiles successfully and can be executed just fine.

It implements the "hello_world" program (a "release" created with "xrel") that
uses [cowboy](https://ninenines.eu/docs/en/cowboy/2.12/guide/) library to
expose a "hello" HTTP endpoint.

It's meant to be used in pair with the
[other dummy Erlang project](../dummy-escript/) which implements the client
side.
