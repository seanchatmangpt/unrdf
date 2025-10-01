# 4. The Substrate: An Autonomic RDF Framework

unrdf is presented as the world's first autonomic RDF framework, designed to transform static knowledge graphs into intelligent, reactive, self-governing systems. It serves as the canonical implementation of the Knowledge Hook paradigm, mapping the abstract concepts of fields, collisions, and runtime dynamics onto the robust and standardized technologies of the Semantic Web.

The strategic choice to build this system upon the Resource Description Framework (RDF) stack is fundamental to its design and purpose. RDF provides a common, standardized framework for representing information as a graph of statements, or "triples," where each statement consists of a subject, a predicate, and an object. This representation makes knowledge machine-readable and independent of any single application's internal data structures. The abstract "information field" of the theoretical model is realized as a concrete RDF knowledge graph.

To interact with and govern this field, unrdf leverages two other core Semantic Web standards:
*   **SPARQL (SPARQL Protocol and RDF Query Language):** This is the standard query language for RDF graphs. In the unrdf architecture, SPARQL queries are used to evaluate the state of the information field—to measure the "value" of a Knowledge Hook at a particular point in time.
*   **SHACL (Shapes Constraint Language):** This is a W3C standard for validating RDF data by defining a set of conditions, or "shapes," that the graph must conform to. In unrdf, SHACL shapes serve as a primary mechanism for declaring policies and governance rules that the knowledge graph must obey.

Within this architecture, the abstract "Knowledge Hook" is implemented as a concrete, enterprise-grade trigger. The various predicate types defined in unrdf—such as ASK (boolean SPARQL queries), SHACL (shape conformance), DELTA(change detection), and THRESHOLD (numeric comparisons)—represent the specific, measurable dimensions of the information field. The "collision" of hooks is the logical composition of the results of these evaluations.

This architectural choice is a strategic move to make knowledge computable and governance declarative. By building on the Semantic Web stack, unrdf inherits a world of standardized, verifiable, and interoperable tooling. Governance logic is not buried in thousands of lines of brittle, imperative application code. Instead, it is declared as a set of SHACL shapes and SPARQL queries that exist as first-class citizens within the knowledge layer itself. This separation of concerns is the essential foundation for creating a truly autonomic system—one that can understand, reason about, and manage its own rules.

| Component | Function | Maps to Autonomic Principle |
| :--- | :--- | :--- |
| Knowledge Hooks | The autonomic reflex arc; a declarative trigger based on the state of the knowledge graph. | Self-Optimization / Self-Healing |
| Policy Pack Manager | Manages the lifecycle of versioned, portable governance units (collections of hooks and shapes). | Self-Configuration |
| Multi-Agent Resolution Layer | Resolves conflicting change proposals from multiple autonomous agents using defined strategies. | System-wide Self-Optimization |
| Lockchain Writer | Creates cryptographic, Git-anchored receipts for every transaction, providing a verifiable audit trail. | Self-Protection / Audit |
| Effect Sandbox | Provides an isolated, secure execution environment (e.g., VM2/worker thread) for hook effects. | Self-Healing / Fault Isolation |
| Transaction Manager | Ensures the atomic application of state changes (deltas) to the knowledge graph. | Foundational Integrity |
