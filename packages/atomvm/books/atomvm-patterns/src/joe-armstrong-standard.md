# Appendix: The Joe Armstrong Standard

The thirty patterns build an OTP-shaped application. This appendix states the harder bar: whether the runtime preserves the failure model that made Erlang useful.

The standard is not “the API looks like actors.” It is eight observable laws.

## 1. A process owns its state

A process receives immutable values and is the only authority that can replace its state. Callers cannot retain a mutable reference into the process heap. State changes are visible only through messages, replies, snapshots, or receipts.

## 2. Receive is selective

A mailbox is not merely a callback queue. A process may wait for a message matching its current receive clause while retaining unmatched messages in their original mailbox positions.

A handler can install the next receive clause with `ProcDirective.receive(state, selector)`.

## 3. Replies are not state

A request reply is a protocol value. It is not implicitly the process's entire internal state. `ProcDirective.reply(reply, state)` keeps the public protocol smaller than the private implementation.

## 4. Scheduling is preemptive in spirit

The JavaScript event loop is not the BEAM scheduler, but a single process must not drain an unbounded mailbox in one turn. Each `Proc` has a reduction budget. When the budget is exhausted, it yields before continuing.

The runtime exposes `reductions`, `yields`, and `reductionLimit` through `ProcSys.statistics`.

## 5. Failure is data

Termination has an explicit class: `normal`, `shutdown`, `error`, `kill`, or `noproc`. Normal linked exits do not kill peers. Abnormal exits propagate across links. A trapping process receives an immutable `ExitSignal`. A monitor can deliver an immutable `DownSignal` without sharing fate.

## 6. Late messages are harmless

A `ProcAlias` is a revocable delivery capability. Once revoked, late replies are recorded as dropped rather than entering a process that no longer expects them.

## 7. Ordering has evidence

Every admitted envelope records `from` and a monotonically increasing `senderSequence`. Selective receive may defer an unmatched message, but the mailbox retains sender order and transition receipts preserve the observed sequence.

## 8. Crashes explain themselves

A process crash produces a bounded report containing process identity, exit class, current message digest, state digest, pending mailbox depth, reductions consumed, and timestamp. The report is diagnostic evidence. Recovery remains the supervisor's responsibility.

## Two runtimes, one doctrine

The JavaScript Armstrong kernel is executed by Node.js. The corresponding BEAM laws are independently executed inside real AtomVM by `armstrong_kernel_probe.erl`: isolated state, sender order, selective receive, crash isolation, links and trapped exits, monitor `DOWN`, restart after failure, and tail-recursive servers.

JavaScript does not become Erlang by adopting method names. Standing comes from observing the same bounded laws in each runtime.

## What is not claimed

`@unrdf/atomvm` does not claim to reproduce the BEAM instruction scheduler, per-process garbage collector, distributed Erlang protocol, EPMD, hot-code loading semantics, or transparent remote PIDs in JavaScript. Those remain separate capabilities and must be proven independently before being named.
