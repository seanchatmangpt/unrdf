# Key Concepts: @unrdf/dark-matter

Fundamental ideas and mental models.

---

## Core Concepts

### Concept 1: The Pipeline Model

Think of data moving through a series of processing stages from input to output.

Each stage:
- Receives input
- Transforms data
- Passes to next stage
- Handles errors

### Concept 2: Configuration Control

All behavior is controlled through configuration, enabling:
- Testing with different settings
- Production customization
- Clear documentation

### Concept 3: Async Operations

All I/O is asynchronous because:
- Scales better under load
- Doesn't block execution
- Enables concurrency

---

## Mental Models

### Model 1: Stream of Data

Visualize continuous data flow through the system from input to output.

### Model 2: Configuration Tree

Picture nested configuration controlling every aspect.

### Model 3: Event Stream

Understand operations as producing events over time.

---

## Learning Path

1. Understand the pipeline model
2. Learn configuration options
3. Master async/await patterns
4. Explore advanced techniques
5. Contribute improvements

---

## When to Use

Use @unrdf/dark-matter when:
- You need its specific capabilities
- Performance matters
- You have large datasets
- You need reliability

---

## See Also

- [01-architecture.md](01-architecture.md) - System design
- [04-advanced.md](04-advanced.md) - Deep dives
- [../reference/01-api.md](../reference/01-api.md) - API details
