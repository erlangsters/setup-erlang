# Setup Erlang

This repository contains a Github JavaScript action to setup Erlang in your
Github Actions workflows.

```yaml
- uses: actions/setup-erlang@v1
  with:
    erlang-version: '27'
    install-rebar3: true
```

The pre-built Erlang binaries used are from the community S3 bucket maintained
by the community and build with this [Erlang builder](https://github.com/erlangsters/build-erlang).

Works on Linux, macOS and Windows.

## Basic usage

The most basic usage of this action is the following.

```
```

It will detect the platform and install the pre-built Erlang binaries of the 
latest supported Erlang version.

You may specify an Erlang version with the `erlang-version` field.

```
```


Additionally, if you need the Rebar3 build system installed, you can

To be written.

## Advanced usage

There is not really any advanced use. However, here is how.

```
```

## Supported platforms

By default, the Erlang builds that are used by this Github action comes from the Erlangsters build registry.

And therefore depends on the availability.
See https://

```
steps:
- uses: actions/checkout@v4
- uses: actions/setup-erlang@v1
  with:
    erlang-version: '27'
- run: escript my_script.erl
```

## Dummy applications

What's with the `dummy-release/` and `dummy-escript/` folders in this
repository? Well, they're real-life Erlang applications that are used by the
Github Actions workflow to test the `setup-erlang` Github action against.

The dummy-release/ folder contains an OTP release which implements a "HTTP echo server" and depends on `cowboy`.

The dummy-escript/ folder contains an escript that makes an HTTP request to a
local HTTP server, and depnds on `gun`.

They're meant to be used together to test if the built applications work
flawlessly.


The Github Actions workflow testing the Github action used them to ensure that
the produced.

They're minimal by nature and both have a dependency on common Erlang
applications.