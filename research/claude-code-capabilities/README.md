# Claude Code Capability Research Framework

## Overview

This research framework implements a systematic 10-agent swarm for exploring Claude Code's capability surface, discovering compositional combinations, and producing actionable documentation.

## Research Methodology

### Technique A: Capability Graph + Composition Closure
- Define primitive capabilities as nodes
- Compute edges representing compositional relationships
- Explore pairs/triples that yield new operational patterns

### Technique B: Diataxis-Driven Exploration
- **Tutorials**: Minimal hello-world for each primitive
- **How-to Guides**: Solve one workflow problem per primitive
- **Reference**: Enumerate flags/commands/config with runnable examples
- **Explanations**: Document why combinations change operator economics

### Technique C: Ten-Agent Research Swarm
Each agent explores a "primitive cluster" with explicit deliverables.

### Technique D: Measurable Novelty Filter
Only count "new capability" when it changes:
- Operator steps reduced
- Policy strength increased
- Recovery time reduced
- Parallel throughput increased
- Reproducibility increased

## Agent Assignment

| Agent | Focus Area | Deliverable |
|-------|------------|-------------|
| 1 | Subagents & Delegation | Prove 10-way decomposition works |
| 2 | Hooks & Tool Governance | Policy catalog (allow/deny/ask) |
| 3 | Plugins | Reusable plugin skeleton |
| 4 | Slash Commands | Command suite for research workflows |
| 5 | MCP (Model Context Protocol) | Permission + discovery map |
| 6 | Programmatic Execution | JSON/stream patterns + session resume |
| 7 | Checkpointing | Quantify rewind risk tolerance |
| 8 | IDE/VS Code Surface | Extension vs CLI capability map |
| 9 | Composition Hunter | Cross-composition tests |
| 10 | Librarian | Capability lattice + frontier report |

## Artifacts

1. `capability-lattice.json` - Graph of all capabilities and compositions
2. `composition-closure-report.md` - Analysis of productive combinations
3. `diataxis/` - Four-track documentation set
4. `novelty-metrics.json` - Quantified capability measurements

## Execution

```bash
# Spawn all 10 agents in parallel
node research/claude-code-capabilities/spawn-swarm.mjs

# Or use slash command
/claude-code-research
```
