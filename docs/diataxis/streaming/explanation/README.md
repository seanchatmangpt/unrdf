# Explanation

Conceptual deep-dives that help you understand why `@unrdf/streaming` is designed the way it is. Read these after you have used the package and want to understand the reasoning.

- [Change Feed Architecture](./01-change-feed-architecture.md) — why EventTarget + ring buffer, how the layers compose
- [Backpressure and Flow Control](./02-backpressure-and-flow-control.md) — Node.js stream mechanics and how the package manages flow
