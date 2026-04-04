# How-To Guides — @unrdf/federation

How-to guides are goal-oriented. Each guide solves a specific task. They assume you already
understand the basics; if you are new to the package, start with the
[Tutorials](../tutorials/README.md).

## Index

| #   | Guide                                                              | When to use                                                                      |
| --- | ------------------------------------------------------------------ | -------------------------------------------------------------------------------- |
| 01  | [Add a remote endpoint](./01-add-remote-endpoint.md)               | Register a new peer at runtime, with metadata and a connectivity check           |
| 02  | [Handle federation timeout](./02-handle-federation-timeout.md)     | Set per-query and per-peer timeouts, inspect partial results after a timeout     |
| 03  | [Query with source selection](./03-query-with-source-selection.md) | Route a query to one peer, or switch between broadcast/selective/first-available |

## Key points to keep in mind

**Peer status is `'healthy'` by default.** When you call `registerPeer` or construct a
coordinator with an initial `peers` array, each peer starts `'healthy'` without a real
connectivity test. Status changes to `'degraded'` or `'unreachable'` only after a failed
ping or failed query response. Call `coordinator.healthCheck()` or enable periodic health
checks with `coordinator.startHealthChecks()` before issuing queries if you need to validate
reachability upfront.

**`coordinator.query()` never throws on partial failure.** If some peers succeed and others
fail, `result.success` is `true` (success when at least one peer responded) and
`result.failureCount` records the number of peers that failed. Check `result.peerResults`
to distinguish individual outcomes.

**`coordinator.query()` returns `false` for `success` only when zero peers succeed.** The
return value always has the full envelope; never assume an exception path.

**Deduplication is JSON-key based.** `aggregateResults` stringifies each binding with
`JSON.stringify` and deduplicates by that key. Two bindings are considered equal only if
their serialisations are identical.
