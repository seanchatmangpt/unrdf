# Tutorials — @unrdf/federation

Tutorials guide you through learning-oriented tasks. Follow them in order if you are new to
the federation package. Each tutorial produces a working result by the end.

## Prerequisites

```bash
pnpm add @unrdf/federation
```

Node.js >= 18 is required. You will need at least two SPARQL endpoints (or a local mock) for
the hands-on steps.

## Index

| #   | Tutorial                                                         | What you will learn                                           |
| --- | ---------------------------------------------------------------- | ------------------------------------------------------------- |
| 01  | [Federate across two stores](./01-federate-across-two-stores.md) | `createCoordinator`, `addPeer`, `query`, result deduplication |

## After the tutorials

- Read the [How-To Guides](../how-to/README.md) for specific tasks such as adding a remote
  endpoint, handling timeouts, or narrowing queries to a specific source.
- Consult the [Reference](../reference/README.md) for exact API signatures and config fields.
- Read the [Explanation](../explanation/README.md) to understand how distributed execution and
  source selection actually work.
