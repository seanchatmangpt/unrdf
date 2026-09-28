%% Tier witness: a real BEAM program each continuum tier (browser, edge, fog,
%% cloud) executes on AtomVM. Compiled once per tier with -DTIER=<tier>.
%%
%% It proves three things a tier must be able to do: pass and selectively
%% receive messages, compute over binaries with small-integer arithmetic, and
%% report an identity a caller can check byte-for-byte. Only BIFs are used, so
%% it runs on a bare AtomVM (no estdlib); integers stay below 2^27 because
%% 32-bit WASM AtomVM cannot box larger ones; and it never calls spawn because
%% spawn hangs in the shipped wasm build (see bin/atomvm-wasm.mjs).
-module(tier_witness).
-export([start/0]).

-ifndef(TIER).
-error("compile with -DTIER=browser|edge|fog|cloud").
-endif.

-define(CORPUS, <<"unrdf-atomvm-continuum">>).

level(browser) -> 0;
level(edge) -> 1;
level(fog) -> 2;
level(cloud) -> 3.

start() ->
    self() ! {witness, other, 99},
    self() ! {witness, self(), level(?TIER)},
    Me = self(),
    Level =
        receive
            {witness, Me, L} -> L
        after 1000 -> erlang:error(witness_timeout)
        end,
    {A, B, N} = adler(?CORPUS, 1, 0, 0),
    erlang:display({atomvm_tier_alive, ?TIER, Level, N, A, B}),
    ok.

adler(<<>>, A, B, N) -> {A, B, N};
adler(<<X, Rest/binary>>, A, B, N) ->
    A1 = (A + X) rem 65521,
    B1 = (B + A1) rem 65521,
    adler(Rest, A1, B1, N + 1).
