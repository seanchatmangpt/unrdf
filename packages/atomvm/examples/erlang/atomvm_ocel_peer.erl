%% SPDX-License-Identifier: MIT
%% Browser peer endpoint for the UNRDF AtomVM OCEL v2 explorer.
%%
%% The browser transport never manufactures the runtime proof: every domain
%% message is passed through this registered AtomVM process on both peers.
%% The returned checksum is computed inside AtomVM and is used by the browser
%% bridge to prove that source and target AtomVM instances observed identical
%% message bytes.
-module(atomvm_ocel_peer).
-export([start/0]).

start() ->
    register(peer, self()),
    loop(0).

loop(Sequence) ->
    receive
        {emscripten, {call, Promise, Message}} ->
            Next = Sequence + 1,
            Digest = checksum(Message),
            %% Emscripten's promise integer bridge uses a C int. Return the
            %% monotonic sequence and full checksum as a string so no receipt
            %% information is truncated at the WASM/JS boundary.
            Receipt = <<(integer_to_binary(Next))/binary, $:, (integer_to_binary(Digest))/binary>>,
            emscripten:promise_resolve(Promise, Receipt),
            loop(Next);
        {emscripten, {cast, _Message}} ->
            loop(Sequence + 1)
    end.

checksum(Binary) when is_binary(Binary) ->
    checksum(binary_to_list(Binary), 5381).

checksum([], Acc) ->
    Acc rem 1000000000;
checksum([Byte | Rest], Acc) ->
    checksum(Rest, ((Acc * 33) + Byte) rem 1000000000).
