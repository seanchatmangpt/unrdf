# KGC-4D Tutorials

**Learning-oriented guides: Build working examples step-by-step**

These tutorials teach you KGC-4D concepts by building complete, working examples. Each tutorial is designed to be completed in 5-20 minutes and teaches one clear concept.

---

## Tutorial Catalog

### 1. [Getting Started with Nanosecond Timestamps](./01-nanosecond-timestamps.md)
**Learn:** Work with BigInt nanosecond precision for event timestamps  
**Time:** 5 minutes  
**Prerequisites:** Basic JavaScript knowledge  
**Outcome:** Understand why nanoseconds matter and how to use them

---

### 2. [Create and Freeze Your First Universe](./02-create-freeze-universe.md)
**Learn:** Create a KGC store, add RDF data, and freeze it for snapshots  
**Time:** 10 minutes  
**Prerequisites:** Tutorial 01  
**Outcome:** Working universe with Git-backed snapshot

---

### 3. [Time Travel to Past States](./03-time-travel.md)
**Learn:** Reconstruct historical state using snapshots and event replay  
**Time:** 20 minutes  
**Prerequisites:** Tutorial 02  
**Outcome:** Time-travel system with rollback capability

---

### 4. [Query Event Logs with SPARQL](./04-query-event-logs.md)
**Learn:** Query immutable event history using SPARQL  
**Time:** 15 minutes  
**Prerequisites:** Tutorial 02, Basic SPARQL  
**Outcome:** Query system for audit trails

---

### 5. [Use Vector Clocks for Causality](./05-vector-clocks.md)
**Learn:** Track causal relationships in distributed systems  
**Time:** 20 minutes  
**Prerequisites:** Tutorial 01  
**Outcome:** Distributed event ordering with vector clocks

---

## Recommended Learning Path

```
Start → Tutorial 01 → Tutorial 02 → Tutorial 03 → Tutorial 04
                                  ↓
                           Tutorial 05 (advanced)
```

**Beginner Path:** 01 → 02 → 04 (30 minutes)  
**Advanced Path:** 01 → 02 → 03 → 05 (55 minutes)  
**Complete Path:** All 5 tutorials (70 minutes)

---

## Tutorial Format

Each tutorial follows this structure:

1. **Objective** - What you'll learn (1 sentence)
2. **Prerequisites** - Required knowledge and capabilities
3. **Setup** - Code scaffold to start with
4. **Steps** - Numbered walkthrough with explanations
5. **Code Example** - Complete working example
6. **Verification** - How to test it works
7. **Evidence** - Link to source code and tests
8. **Next Steps** - Where to go next

---

## Evidence Base

All tutorials are backed by:
- Working code in `/home/user/unrdf/packages/kgc-4d/examples/`
- Passing tests in `/home/user/unrdf/packages/kgc-4d/test/`
- Source implementations in `/home/user/unrdf/packages/kgc-4d/src/`

---

**Navigate:** [Main Diataxis](../README.md) | [How-To Guides](../how-to/) | [Reference](../reference/) | [Explanation](../explanation/)
