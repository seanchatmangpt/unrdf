[< Back to Full Thesis](../FULL_THESIS.md)

# Chapter 3: Formalizing the KGC-4D Temporal Substrate

## 3.1 Defining the O-t-V-G Space

We formally define the UNRDF core, KGC-4D, not as a graph of triples, but as a continuous space spanning four dimensions: Observable data ($O$), precise Time ($t$), Vector Causality ($V$), and Git-based cryptographic state ($G$).

Let the global system state be an Event Log $L$, consisting of a set of events $E$.
Unlike standard RDF stores where `INSERT DATA` overwrites previous assertions, KGC-4D is strictly append-only.
An event $e \in E$ is defined as the tuple:
$$ e = (id, \Delta, \tau, V_c, \mathcal{A}) $$
Where:

- $id = \mathcal{H}(\Delta || \tau || V_c)$ is the BLAKE3 hash of the payload.
- $\Delta$ is the semantic mutation (RDF addition/retraction).
- $\tau$ is the wall-clock timestamp with nanosecond precision.
- $V_c$ is the vector clock mapping the causal history of the originating agent.
- $\mathcal{A}$ represents the object-centric bindings (mapping to OCEL).

## 3.2 Deterministic Projection vs. Hallucinated Reality

A primary critique of CRDT-backed event logs is the loss of execution reality. In KGC-4D, the event log $L$ is immutable. The active state of the graph, the Projection $\mathcal{P}(L, t)$, is a deterministic function applied to $L$ up to time $t$.

When the `unrdf telco splice` command merges divergent forks, it does not overwrite the past. It introduces $e_{splice}$, which records the vector resolution. Conformance checking algorithms in `wasm4pm` operate over $L$, not $\mathcal{P}$, ensuring that token-based replay evaluates the literal historical truth experienced by the actors.
