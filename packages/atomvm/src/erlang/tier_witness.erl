%% Tier witness: a real BEAM program each continuum tier (browser, edge, fog,
%% cloud) executes on AtomVM. Compiled once per tier with -DTIER=<tier>.
%%
%% It proves three things a tier must be able to do: run a real second process
%% and exchange messages with it, compute over binaries with small-integer
%% arithmetic, and report an identity a caller can check byte-for-byte.
%% spawn/1 is an Erlang wrapper in AtomVM's estdlib erlang.beam, so the tier
%% packs (scripts/build-tier-fixtures.mjs) ship that module.
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
    Me = self(),
    Child = spawn(fun() -> Me ! {witness, self(), level(?TIER)} end),
    Level =
        receive
            {witness, Child, L} -> L
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
