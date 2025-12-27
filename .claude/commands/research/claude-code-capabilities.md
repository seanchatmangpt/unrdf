---
description: Launch 10-agent swarm for Claude Code capability research
arguments:
  - name: mode
    description: 'Execution mode: full, quick, or specific agent number (1-10)'
    required: false
    default: 'full'
---

# Claude Code Capability Research Swarm

## Mode: $mode

Launch the capability research swarm to systematically explore Claude Code's capability surface.

## Agent Assignments

| Agent | Focus Area              | Deliverable                   |
| ----- | ----------------------- | ----------------------------- |
| 1     | Subagents & Delegation  | 10-way decomposition proof    |
| 2     | Hooks & Tool Governance | Policy catalog                |
| 3     | Plugins                 | Reusable plugin skeleton      |
| 4     | Slash Commands          | Research command suite        |
| 5     | MCP Protocol            | Permission + discovery map    |
| 6     | Programmatic Execution  | Pipeline patterns             |
| 7     | Checkpointing           | Risk tolerance quantification |
| 8     | IDE/VS Code Surface     | CLI vs Extension parity       |
| 9     | Composition Hunter      | Cross-composition tests       |
| 10    | Librarian               | Final lattice + frontier      |

## Execution Protocol

Based on mode "$mode":

### If mode is "full":

Spawn all 10 agents in parallel using Task tool. Each agent should:

1. Read their agent definition from `.claude/agents/research/cc-agent-XX-*.md`
2. Execute their research protocol
3. Store findings in coordination memory
4. Report back with structured deliverables

### If mode is "quick":

Spawn agents 1-3 only for core primitives:

- Agent 1: Subagents (essential for parallel work)
- Agent 2: Hooks (essential for policy)
- Agent 3: Plugins (essential for distribution)

### If mode is a number (1-10):

Spawn only the specified agent for focused research.

## Output Requirements

All agents must produce:

1. Structured JSON findings
2. Evidence of tests run (commands, outputs)
3. Novelty metrics if applicable
4. Recommendations for further exploration

## Storage

Store all findings at:

- `swarm/cc-research/agent-{NN}/findings` - Individual agent findings
- `swarm/cc-research/capability-lattice-updates` - Lattice updates
- `swarm/cc-research/frontier` - Unexplored combinations

## Validation

Before declaring complete, verify:

- [ ] All spawned agents returned findings
- [ ] Findings include actual evidence (not just theory)
- [ ] Novelty metrics calculated where applicable
- [ ] Results stored in coordination memory
