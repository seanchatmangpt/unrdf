[< Back to Full Thesis](../FULL_THESIS.md)

# Chapter 5: Concurrency, Soundness, and Stochastic Agents

## 5.1 The Danger of Stochastic Mutation

Allowing Reinforcement Learning (RL) agents, such as `LinUCBAgent`, to autonomously route workflows introduces severe risk. Standard Petri nets can easily reach deadlocks if an agent makes an unconstrained mutation.

## 5.2 PoWL v2: Soundness by Construction

To bypass the NP-hard problem of verifying Soundness in real-time, `wasm4pm` constraints the mutation space. Agents operate on Partially Ordered Workflow Language (PoWL) constructs rather than arbitrary graphs.
Let a process model $M$ be inductively defined by operators: $seq(M_1, M_2)$, $xor(M_1, M_2)$, $and(M_1, M_2)$, and $loop(M_1, M_2)$.
**Proof of Soundness:** Because $M$ is strictly block-structured and built inductively from sound primitives, any mutation applying these operators yields a model that maps to a live, bounded Workflow Net (WF-net). State-space explosion is circumvented architecturally.

## 5.3 Bounding Agents with LTL (Declare) Envelopes

To enforce business logic, UNRDF utilizes the **Declare** framework. The RL agent's policy space is bounded by Linear Temporal Logic ($LTL_f$) constraints (e.g., `Response(A, B)`, `Not Co-Existence(A, B)`). The `@unrdf/core` Admission Engine runs a trace automaton verification; if the stochastic agent suggests a non-compliant route, the operation is blocked in $< 1\mu s$.
