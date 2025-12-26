# Reference: Subagent Types

Complete reference of available agent types and their capabilities.

## Core Agents

### coder
Implementation specialist for writing clean, efficient code.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | Code generation, implementation |
| Output | Working code with explanations |

### tester
Comprehensive testing and quality assurance specialist.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | Test writing, coverage analysis |
| Output | Test files, coverage reports |

### reviewer
Code review and quality assurance specialist.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | Code review, quality assessment |
| Output | Review comments, improvement suggestions |

### planner
Strategic planning and task orchestration agent.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | Task breakdown, project planning |
| Output | Action plans, task lists |

### researcher
Deep research and information gathering specialist.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | Codebase exploration, documentation research |
| Output | Research findings, summaries |

## Exploration Agents

### Explore
Fast agent for codebase exploration.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | Quick file searches, codebase questions |
| Parameters | Thoroughness: quick, medium, very thorough |

Usage:
```
Task("Find auth code",
     "Find authentication implementation...",
     "Explore")
```

## Architecture Agents

### system-architect
Expert agent for system architecture design.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | Architecture review, design patterns |
| Output | Architecture diagrams, design recommendations |

### backend-dev
Specialized for backend API development.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | REST/GraphQL endpoints, database design |
| Output | API implementations, schema designs |

## Analysis Agents

### code-analyzer
Advanced code quality analysis.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | Static analysis, quality metrics |
| Output | Analysis reports, quality scores |

### production-validator
Ensures applications are deployment-ready.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | Production readiness checks |
| Output | Validation reports, blockers list |

## Coordination Agents

### task-orchestrator
Central coordination for task decomposition.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | Multi-step task coordination |
| Output | Execution plans, synthesized results |

### adaptive-coordinator
Dynamic topology switching coordinator.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | Self-organizing swarm patterns |
| Output | Topology recommendations, coordination signals |

## Specialized Agents

### ml-developer
Machine learning model development.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | ML training, model deployment |
| Output | Model code, training scripts |

### cicd-engineer
GitHub Actions CI/CD pipeline specialist.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | Workflow creation, pipeline optimization |
| Output | GitHub Actions YAML, CI/CD configs |

### api-docs
OpenAPI/Swagger documentation expert.

| Property | Value |
|----------|-------|
| Tools | All tools |
| Best For | API documentation |
| Output | OpenAPI specs, documentation |

## Agent Selection Guide

```
┌─────────────────────┬─────────────────────────────────┐
│ Task Type           │ Recommended Agent               │
├─────────────────────┼─────────────────────────────────┤
│ Write new code      │ coder, backend-dev              │
│ Write tests         │ tester                          │
│ Review code         │ reviewer, code-analyzer         │
│ Research codebase   │ researcher, Explore             │
│ Plan work           │ planner, task-orchestrator      │
│ Architecture        │ system-architect                │
│ Production check    │ production-validator            │
│ CI/CD setup         │ cicd-engineer                   │
│ API documentation   │ api-docs                        │
│ ML development      │ ml-developer                    │
└─────────────────────┴─────────────────────────────────┘
```

## Usage Pattern

```javascript
Task("description",           // Short description (3-5 words)
     "detailed prompt...",     // Full task instructions
     "agent-type")            // One of the types above
```

## Related

- [Tutorial: Your First Subagent](../tutorials/01-first-subagent.md)
- [How-to: Parallel Execution](../how-to/parallel-execution.md)
- [Explanation: Delegation Patterns](../explanations/delegation-patterns.md)
