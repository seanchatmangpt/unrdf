# Chatman Equation: Complete Documentation Reference

**S(t) = ⟨O, t_ns, V, G⟩**

This document contains the complete Diataxis documentation structure for the Chatman Equation, generated from TOML configuration files and Tera templates.

---

## Table of Contents

- [Overview](#overview)
- [The Equation Explained](#the-equation-explained)
- [Tutorial 1: Understanding the Chatman Equation](#tutorial-1-understanding-the-chatman-equation)
- [Tutorial 2: Implementing 4D State](#tutorial-2-implementing-4d-state)
- [Tutorial 3: Advanced Time-Travel Queries](#tutorial-3-advanced-time-travel-queries)
- [How-To Guides](#how-to-guides)
- [API Reference](#api-reference)
- [Explanations](#explanations)

---

## Overview

The Chatman Equation represents a paradigm shift from discrete-state computation to continuous field-theoretic reasoning for knowledge graphs. It defines state as a 4-tuple integrating:

1. **Observable State (O)**: The current RDF graph
2. **Nanosecond Time (t_ns)**: Precise temporal ordering
3. **Vector Clock (V)**: Causal dependencies
4. **Git Dimension (G)**: Cryptographic proof via content-addressed storage

---

## The Equation Explained

```math
S(t) = \langle O, t_{ns}, \vec{V}, G \rangle
```

### Mathematical Formulation

**Definition**: A 4D state is a smooth section S: M → E where:
- M = T × N (time × node manifold)
- E = O × V × G (total state bundle)

### Dimensions

#### O: Observable State
- **Type**: RDF Dataset (Named Graphs)
- **Example**: `{ <http://ex.org/alice> foaf:name "Alice" }`
- **Description**: The current RDF graph state - what you see when you query

#### t_ns: Nanosecond Timestamp
- **Type**: BigInt (nanoseconds since epoch)
- **Example**: `1704067200000000000`
- **Description**: When this state existed - precision temporal ordering

#### V: Vector Clock
- **Type**: Map<NodeID, Counter>
- **Example**: `{ node1: 42, node2: 17 }`
- **Description**: Causal relationships - what caused this state

#### G: Git Dimension
- **Type**: SHA-256 Hash
- **Example**: `a7f3e1c9d42b...`
- **Description**: Content-addressed proof - cryptographic immutability

---

