# Section 2: Related Work

## 2.1 RDF and Knowledge Graphs

The Resource Description Framework (RDF) [Lassila & Swick, 1999] provides the foundation for representing structured knowledge as subject-predicate-object triples. SPARQL [Harris & Seaborne, 2013] enables declarative querying of RDF graphs, while SHACL [Knublauch & Kontokostas, 2017] provides constraint validation.

Recent work on RDF processing includes:
- **RDF Stream Processing (RSP)**: C-SPARQL [Barbieri et al., 2010], CQELS [Le-Phuoc et al., 2011]
- **Incremental Reasoning**: DynamicDL [Volz et al., 2005], DRed [Gupta et al., 1993]
- **Change Detection**: RDF Patch [Dodds, 2008], LD Patch [Ceres & Steyskal, 2015]

However, these approaches focus on stream processing or reasoning, not on providing a unified calculus for reactive behavior with cryptographic provenance.

## 2.2 Reactive Systems and Event-Driven Architectures

Reactive systems [Bainomugisha et al., 2013] propagate changes through dataflow graphs. Functional Reactive Programming (FRP) [Elliott & Hudak, 1997] provides declarative event handling. Event sourcing [Fowler, 2005] maintains audit trails through event logs.

Distributed reactive systems include:
- **Actor Model**: Akka [Haller & Odersky, 2009], Orleans [Bykov et al., 2011]
- **Reactive Streams**: Reactor, RxJava [Meijer, 2012]
- **CQRS/Event Sourcing**: Axon Framework, EventStore

KGC differs by integrating reactivity directly into RDF semantics rather than requiring external frameworks, while providing cryptographic provenance absent from traditional event systems.

## 2.3 Policy and Governance Frameworks

Policy-based management systems include:
- **Ponder**: Damianou et al., 2001
- **Rei**: Kagal et al., 2003
- **ODRL**: Iannella & Villata, 2018

Access control and authorization:
- **XACML**: OASIS eXtensible Access Control Markup Language
- **N3 Logic**: Berners-Lee et al., 2008

KGC extends these by integrating policy governance with graph operations and providing versioned policy packs as first-class entities.

## 2.4 Cryptographic Auditability

Blockchain and distributed ledger technology [Nakamoto, 2008; Wood, 2014] provide tamper-proof audit trails. RDF canonicalization (URDNA2015) [Longley & Sporny, 2017] enables deterministic graph serialization for cryptographic hashing.

Git's content-addressed storage [Chacon & Straub, 2014] provides immutable history. Merkle trees [Merkle, 1980] enable efficient integrity verification.

KGC combines URDNA2015 canonicalization with Git-anchored lockchain, providing RDF-native cryptographic auditability without requiring blockchain infrastructure.
