-module(read_priv).
-export([start/0]).

%% atomvm:read_priv/2 needs the file packed as <app>/priv/<path> with a u32be
%% length prefix (src/avm-packer.mjs does this for {file: true} entries).
start() ->
    Hello = atomvm:read_priv(read_priv, "hello.txt"),
    Blob = atomvm:read_priv(read_priv, "sub/blob.bin"),
    Missing = atomvm:read_priv(read_priv, "nope.txt"),
    erlang:display({priv, byte_size(Hello), Hello, byte_size(Blob), Blob, Missing}),
    case {Hello, Blob, Missing} of
        {<<"hello priv\n">>, <<0, 1, 2, 255>>, undefined} -> ok;
        _ -> error
    end.
