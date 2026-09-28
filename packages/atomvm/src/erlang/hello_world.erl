-module(hello_world).
-export([start/0]).

%% Uses only BIFs so it runs on a bare AtomVM without the estdlib library.
start() ->
    erlang:display({atomvm_module_alive, hello_world}),
    ok.
