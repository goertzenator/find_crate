# find_crate

Erlang functions to find the Rust libraries and executes placed in the `priv/` directory by Rust plugins for erlang.mk and rebar3.

These functions abstract over various platform naming differences for executables and libraries in Rust and Erlang.

# NIF module example
``` erlang
-module(mymodule).
-on_load(init/0).
init() ->
	{ok, Lib} = find_crate:find_library(my_app, "nifcrate", "nifs"),
    ok = erlang:load_nif(Lib, 0).
    
...
```

# Port program example
``` erlang
f() ->
    {ok, Exe} = find_crate:find_executable(my_app, "portcrate", "port"),
    open_port({spawn, Exe}, [{packet, 2}]),
```

