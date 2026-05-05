# Agent Archive - 2026-01-11

**Archived**: January 11, 2026
**Reason**: Agent consolidation and simplification
**Total Agents Archived**: 84 (89.4% of total)
**Agents Retained**: 10 (10.6% of total)

---

## Executive Summary

This archive contains 84 agent definitions removed from active use to simplify the UNRDF agent system from 94 agents down to 10 core, high-value agents.

**Archival completed with zero data loss** - All agents preserved with full git history and can be restored if needed.

---

## Archived Agents by Category

### 1. Research Completed (20 agents)

**Reason**: Research projects completed, deliverables extracted

**Path**: `research/`

**Agents**:
- **Numbered Research Agents** (10 files):
  - `1-capability-cartographer.md`
  - `2-package-archeologist.md`
  - `3-runtime-integrator.md`
  - `4-beam-wasm-specialist.md`
  - `5-receipts-auditor.md`
  - `6-hooks-policy-specialist.md`
  - `7-docs-diataxis-architect.md`
  - `8-poka-yoke-engineer.md`
  - `9-performance-proxy.md`
  - `10-synthesis-editor.md`

- **Claude Code Research** (10 files in `claude-code-research/`):
  - `cc-agent-01-subagents.md` - Subagents & Delegation research
  - `cc-agent-02-hooks.md` - Hooks & Tool Governance research
  - `cc-agent-03-plugins.md` - Plugins research
  - `cc-agent-04-slash-commands.md` - Slash Commands research
  - `cc-agent-05-mcp.md` - MCP (Model Context Protocol) research
  - `cc-agent-06-programmatic.md` - Programmatic/Headless Execution research
  - `cc-agent-07-checkpointing.md` - Checkpointing & Rewind research
  - `cc-agent-08-ide.md` - IDE/VS Code Surface research
  - `cc-agent-09-composition.md` - Composition Hunter research
  - `cc-agent-10-librarian.md` - Librarian synthesis research

**Impact**: Low - Research complete, findings documented in deliverables

---

### 2. Flow Nexus Cloud Platform (9 agents)

**Reason**: Flow Nexus platform not in active use

**Path**: `flow-nexus/`

**Agents**:
- `app-store.md` - Application marketplace specialist
- `authentication.md` - Auth and user management specialist
- `challenges.md` - Coding challenges and gamification specialist
- `neural-network.md` - Neural network training specialist
- `payments.md` - Credit management and billing specialist
- `sandbox.md` - E2B sandbox deployment specialist
- `swarm.md` - AI swarm orchestration specialist
- `user-tools.md` - User management and system utilities specialist
- `workflow.md` - Event-driven workflow automation specialist

**Impact**: Low - Not currently using Flow Nexus platform

**Restoration**: If Flow Nexus is adopted, restore entire directory

---

### 3. Advanced Coordination (20 agents)

**Reason**: Too specialized, overlapping functionality

**Path**: `advanced-coordination/`

**Categories**:

#### Consensus (7 agents in `consensus/`)
- `byzantine-coordinator.md` - Byzantine fault tolerance
- `crdt-synchronizer.md` - Conflict-free Replicated Data Types
- `gossip-coordinator.md` - Gossip protocol coordination
- `performance-benchmarker.md` - Consensus performance benchmarking
- `quorum-manager.md` - Dynamic quorum management
- `raft-manager.md` - Raft consensus algorithm
- `security-manager.md` - Consensus security mechanisms

#### Hive Mind (5 agents in `hive-mind/`)
- `collective-intelligence-coordinator.md` - Distributed cognition
- `queen-coordinator.md` - Hierarchical orchestration
- `scout-explorer.md` - Information reconnaissance
- `swarm-memory-manager.md` - Distributed memory management
- `worker-specialist.md` - Task execution specialist

#### Swarm (3 agents in `swarm/`)
- `adaptive-coordinator.md` - Dynamic topology switching
- `hierarchical-coordinator.md` - Queen-led hierarchical swarm
- `mesh-coordinator.md` - Peer-to-peer mesh network

#### Optimization (5 agents in `optimization/`)
- `benchmark-suite.md` - Comprehensive performance benchmarking
- `load-balancer.md` - Dynamic task distribution
- `performance-monitor.md` - Real-time metrics collection
- `resource-allocator.md` - Adaptive resource allocation
- `topology-optimizer.md` - Topology reconfiguration

**Impact**: Medium - Advanced coordination features available if needed

**Restoration**: Restore specific category or individual agent as needed

---

### 4. GitHub Extended (11 agents)

**Reason**: Overlapping with retained pr-manager and release-manager

**Path**: `github-extended/`

**Agents**:
- `code-review-swarm.md` - Code review swarm (overlaps with core/reviewer)
- `github-modes.md` - GitHub integration modes documentation
- `issue-tracker.md` - GitHub issue management
- `multi-repo-swarm.md` - Cross-repository orchestration
- `project-board-sync.md` - GitHub Projects integration
- `release-swarm.md` - Release swarm (overlaps with release-manager)
- `repo-architect.md` - Repository architecture (overlaps with arch-system-design)
- `swarm-issue.md` - Issue-based swarm coordination
- `swarm-pr.md` - PR-based swarm coordination
- `sync-coordinator.md` - Multi-repo synchronization
- `workflow-automation.md` - GitHub Actions integration

**Impact**: Low - Core GitHub workflows covered by retained pr-manager and release-manager

**Retained GitHub Agents**:
- `.claude/agents/github/pr-manager.md` - PR automation
- `.claude/agents/github/release-manager.md` - Release automation

**Restoration**: Restore specific agent if advanced GitHub feature needed

---

### 5. Methodologies (6 agents)

**Reason**: Too specialized, methodology-specific

**Path**: `methodologies/`

**Categories**:

#### SPARC (4 agents in `sparc/`)
- `architecture.md` - SPARC architecture phase
- `pseudocode.md` - SPARC pseudocode phase
- `refinement.md` - SPARC refinement phase
- `specification.md` - SPARC specification phase

#### GOAP (2 agents in `goal/`)
- `code-goal-planner.md` - Code-centric Goal-Oriented Action Planning
- `goal-planner.md` - GOAP specialist for complex objectives

**Impact**: Low - core/planner handles general planning needs

**Restoration**: Restore if specific SPARC or GOAP methodology required

---

### 6. Templates (9 agents)

**Reason**: Template/pattern agents, not operational

**Path**: `templates/`

**Agents**:
- `automation-smart-agent.md` - Smart agent coordinator template
- `coordinator-swarm-init.md` - Swarm initializer template
- `github-pr-manager.md` - PR manager template (superseded by github/pr-manager)
- `implementer-sparc-coder.md` - SPARC implementation template
- `memory-coordinator.md` - Memory coordination template
- `migration-plan.md` - Migration plan template
- `orchestrator-task.md` - Task orchestrator template
- `performance-analyzer.md` - Performance analyzer template
- `sparc-coordinator.md` - SPARC orchestrator template

**Impact**: None - Templates only, not operational agents

**Restoration**: Templates available for reference if creating new agents

---

### 7. Specialized Domain (9 agents)

**Reason**: Too specialized, infrequently used

**Path**: `specialized/`

**Agents**:
- `base-template-generator.md` - Template generation (root-level)
- `code-review/analyze-code-quality.md` - Code quality analyzer (overlaps with code-analyzer)
- `data/ml/data-ml-model.md` - Machine learning model developer
- `development/backend/dev-backend-api.md` - Backend API developer (overlaps with coder)
- `devops/ci-cd/ops-cicd-github.md` - GitHub CI/CD engineer (overlaps with release-manager)
- `documentation/api-docs/docs-api-openapi.md` - OpenAPI documentation specialist
- `domain-specific/mobile/spec-mobile-react-native.md` - React Native developer
- `neural/safla-neural.md` - Self-aware neural specialist
- `unit/tdd-london-swarm.md` - TDD London School swarm

**Impact**: Low - Specialized use cases, recall if needed

**Restoration**: Restore individual agent for specific domain needs

---

## Retained Agents (10 Total)

### Active Agent Structure

```
.claude/agents/
├── core/
│   ├── coder.md              # Code implementation specialist
│   ├── planner.md            # Strategic planning
│   ├── researcher.md         # Research & investigation
│   ├── reviewer.md           # Code review specialist
│   └── tester.md             # Testing specialist
├── testing/validation/
│   └── production-validator.md  # Production readiness validation
├── analysis/
│   └── code-analyzer.md      # Deep code analysis
├── github/
│   ├── pr-manager.md         # PR automation
│   └── release-manager.md    # Release automation
└── architecture/system-design/
    └── arch-system-design.md # System architecture design
```

### Why These 10?

1. **core/*** (5 agents) - Essential to daily development workflow
2. **production-validator** - UNIQUE production readiness validation capability
3. **code-analyzer** - Deep analysis beyond basic code review
4. **pr-manager** - Core GitHub workflow automation
5. **release-manager** - Release process automation
6. **arch-system-design** - System-level architectural guidance

---

## Archive Statistics

| Category | Agents | Reason |
|----------|--------|--------|
| Research | 20 | Completed projects |
| Flow Nexus | 9 | Platform not in use |
| Advanced Coordination | 20 | Too specialized |
| GitHub Extended | 11 | Overlapping functionality |
| Methodologies | 6 | Methodology-specific |
| Templates | 9 | Not operational |
| Specialized | 9 | Domain-specific, infrequent |
| **TOTAL** | **84** | **89.4% of agents** |

---

## Restoration Process

### Restore Individual Agent

```bash
# List archived agents
find archive/2026-01-11/agents -name "*.md"

# Restore specific agent
git mv archive/2026-01-11/agents/specialized/data/ml/data-ml-model.md .claude/agents/data/ml/
```

### Restore Entire Category

```bash
# Restore all Flow Nexus agents
git mv archive/2026-01-11/agents/flow-nexus/ .claude/agents/
```

### Restore Multiple Agents

```bash
# Restore consensus agents
mkdir -p .claude/agents/consensus
git mv archive/2026-01-11/agents/advanced-coordination/consensus/* .claude/agents/consensus/
```

---

## Success Metrics

### Archival Success (Immediate)
- ✅ 84 agents archived (89.4%)
- ✅ 10 agents retained (10.6%)
- ✅ Zero broken references
- ✅ All core workflows functional

### Post-Archival Success (30-day measure)
- **0 restorations** = Perfect selection
- **1-3 restorations** = Good selection
- **4+ restorations** = Re-evaluate criteria

Track restorations to validate agent selection criteria.

---

## Full Agent Manifest

### Research (20 files)
```
archive/2026-01-11/agents/research/
├── 1-capability-cartographer.md
├── 2-package-archeologist.md
├── 3-runtime-integrator.md
├── 4-beam-wasm-specialist.md
├── 5-receipts-auditor.md
├── 6-hooks-policy-specialist.md
├── 7-docs-diataxis-architect.md
├── 8-poka-yoke-engineer.md
├── 9-performance-proxy.md
├── 10-synthesis-editor.md
└── claude-code-research/
    ├── cc-agent-01-subagents.md
    ├── cc-agent-02-hooks.md
    ├── cc-agent-03-plugins.md
    ├── cc-agent-04-slash-commands.md
    ├── cc-agent-05-mcp.md
    ├── cc-agent-06-programmatic.md
    ├── cc-agent-07-checkpointing.md
    ├── cc-agent-08-ide.md
    ├── cc-agent-09-composition.md
    └── cc-agent-10-librarian.md
```

### Flow Nexus (9 files)
```
archive/2026-01-11/agents/flow-nexus/
├── app-store.md
├── authentication.md
├── challenges.md
├── neural-network.md
├── payments.md
├── sandbox.md
├── swarm.md
├── user-tools.md
└── workflow.md
```

### Advanced Coordination (20 files)
```
archive/2026-01-11/agents/advanced-coordination/
├── consensus/
│   ├── byzantine-coordinator.md
│   ├── crdt-synchronizer.md
│   ├── gossip-coordinator.md
│   ├── performance-benchmarker.md
│   ├── quorum-manager.md
│   ├── raft-manager.md
│   └── security-manager.md
├── hive-mind/
│   ├── collective-intelligence-coordinator.md
│   ├── queen-coordinator.md
│   ├── scout-explorer.md
│   ├── swarm-memory-manager.md
│   └── worker-specialist.md
├── optimization/
│   ├── benchmark-suite.md
│   ├── load-balancer.md
│   ├── performance-monitor.md
│   ├── resource-allocator.md
│   └── topology-optimizer.md
└── swarm/
    ├── adaptive-coordinator.md
    ├── hierarchical-coordinator.md
    └── mesh-coordinator.md
```

### GitHub Extended (11 files)
```
archive/2026-01-11/agents/github-extended/
├── code-review-swarm.md
├── github-modes.md
├── issue-tracker.md
├── multi-repo-swarm.md
├── project-board-sync.md
├── release-swarm.md
├── repo-architect.md
├── swarm-issue.md
├── swarm-pr.md
├── sync-coordinator.md
└── workflow-automation.md
```

### Methodologies (6 files)
```
archive/2026-01-11/agents/methodologies/
├── goal/
│   ├── code-goal-planner.md
│   └── goal-planner.md
└── sparc/
    ├── architecture.md
    ├── pseudocode.md
    ├── refinement.md
    └── specification.md
```

### Templates (9 files)
```
archive/2026-01-11/agents/templates/
├── automation-smart-agent.md
├── coordinator-swarm-init.md
├── github-pr-manager.md
├── implementer-sparc-coder.md
├── memory-coordinator.md
├── migration-plan.md
├── orchestrator-task.md
├── performance-analyzer.md
└── sparc-coordinator.md
```

### Specialized (9 files)
```
archive/2026-01-11/agents/specialized/
├── base-template-generator.md
├── code-review/
│   └── analyze-code-quality.md
├── data/ml/
│   └── data-ml-model.md
├── development/backend/
│   └── dev-backend-api.md
├── devops/ci-cd/
│   └── ops-cicd-github.md
├── documentation/api-docs/
│   └── docs-api-openapi.md
├── domain-specific/mobile/
│   └── spec-mobile-react-native.md
├── neural/
│   └── safla-neural.md
└── unit/
    └── tdd-london-swarm.md
```

---

## Related Documentation

- **Agent Retention Analysis**: `AGENT-RETENTION-ANALYSIS.md` (root)
  - Detailed rationale for all archival decisions
  - Selection criteria and methodology
  - Impact assessment

- **Active Agents**: `.claude/agents/`
  - 10 retained agent definitions
  - Core, essential, non-overlapping capabilities

---

## Questions or Issues

If you need to:
- **Restore an agent**: Follow restoration process above
- **Understand archival decision**: See AGENT-RETENTION-ANALYSIS.md
- **Request new agent**: Consider if existing 10 can fulfill need first

All agents are preserved in git history and can be restored at any time.
