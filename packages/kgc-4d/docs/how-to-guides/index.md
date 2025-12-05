# How-To Guides

How-To Guides are **problem-oriented** instructions for accomplishing specific tasks with KGC 4D. Unlike tutorials, these assume you already understand the fundamentals and want to solve a particular problem.

## Available Guides

1. **[Reconstruct State at a Point in Time](./01-time-travel.md)**
   - Travel back to any moment in your knowledge graph's history
   - Find the nearest snapshot and replay events
   - Handle edge cases like missing snapshots

2. **[Verify Snapshots Cryptographically](./02-verification.md)**
   - Ensure integrity and authenticity of frozen universes
   - Use BLAKE3 hashing to detect tampering
   - Build verification into your workflow

3. **[Query Your Knowledge Graph](./03-querying.md)**
   - Use SPARQL to explore events and state
   - Query both Universe and EventLog graphs
   - Build complex analytical queries

4. **[Integrate with Git](./04-git-integration.md)**
   - Store snapshots and verify history with Git
   - Manage Git credentials and repositories
   - Build audit trails with Git

5. **[Distribute Across Node.js and Browser](./05-isomorphic-deployment.md)**
   - Deploy KGC 4D in both server and client environments
   - Use isomorphic-git for Git operations in browsers
   - Handle time differences between runtimes

---

## Quick Problem Solver

| I want to... | Read this... |
|--------------|--------------|
| Go back in time | [Time Travel Guide](./01-time-travel.md) |
| Check data wasn't modified | [Verification Guide](./02-verification.md) |
| Find specific facts | [Querying Guide](./03-querying.md) |
| Use Git for backups | [Git Integration Guide](./04-git-integration.md) |
| Run in a browser | [Isomorphic Deployment Guide](./05-isomorphic-deployment.md) |
| Detect anomalies | [Querying Guide](./03-querying.md) - Analytical Queries |
| Audit all changes | [Git Integration Guide](./04-git-integration.md) - Audit Trails |
| Sync between Node and Browser | [Isomorphic Deployment Guide](./05-isomorphic-deployment.md) |

---

## Prerequisites

All how-to guides assume:
- ✓ You've completed the [Tutorials](../tutorials/index.md)
- ✓ You understand KGC 4D's basic concepts (events, snapshots, queries)
- ✓ You're comfortable with async/await syntax

## Structure

Each guide follows this pattern:
1. **Problem statement** - What you're trying to accomplish
2. **Prerequisites** - What you need before starting
3. **Step-by-step instructions** - Concrete steps with code
4. **Examples** - Real-world usage patterns
5. **Troubleshooting** - Common issues and solutions
