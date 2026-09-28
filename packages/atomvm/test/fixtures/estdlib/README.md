erlang.beam is AtomVM's `libs/estdlib/src/erlang.erl` (tag v0.6.6, Apache-2.0)
compiled with erlc (OTP 25). In AtomVM 0.6.x `spawn/1,3`, `md5`-style helpers and
other functions are Erlang wrappers in this module, so a program that uses them
must ship it inside its .avm. Without it a lookup for `erlang.beam` misses and the
VM reports `undef` (with a correctly terminated pack) - see src/avm-packer.mjs.
