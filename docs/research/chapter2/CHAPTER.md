[< Back to Full Thesis](../FULL_THESIS.md)

# Chapter 2: Background and Literature Review

## 2.1 Formal Foundations of Process Mining

Process mining seeks to discover, monitor, and improve real processes by extracting knowledge from event logs (van der Aalst, 2016). Traditional conformance checking compares an event log $L$ against a Petri net model $N = (P, T, F)$ to compute fitness and precision. Real-time application of these algorithms is mathematically constrained by the state-space explosion problem, particularly when the underlying graph is continuously mutated by multiple concurrent actors.

## 2.2 Object-Centric Process Mining (OCPM)

The transition from traditional XES to OCEL 2.0 (Fahland et al., 2011; van der Aalst, 2019) acknowledges that events do not belong to a single "case" but to a complex web of interconnected objects. RDF graphs are inherently object-centric. However, prior attempts to bridge RDF and OCEL have treated the RDF graph as an export format, not a live execution environment.

## 2.3 Temporal Knowledge Graphs and CRDTs

To maintain consistency in distributed systems without centralized locks, Conflict-free Replicated Data Types (CRDTs) utilize logical clocks (Lamport, 1978). While CRDTs are common in collaborative text editing, their application to formal semantic graphs introduces challenges. As noted in critical process mining literature, naive CRDT state-merging destroys the linear historical truth required for token-based replay.
