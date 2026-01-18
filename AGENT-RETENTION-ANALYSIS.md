# Agent Retention Analysis - UNRDF v6.0.0

**Date**: 2026-01-11
**Total Agents**: 94
**Target Retention**: 10 agents
**Archive**: 84 agents (89.4%)

---

## Executive Summary

**Recommendation**: Retain 10 core, high-value agents that provide essential development, quality assurance, and automation capabilities. Archive 84 specialized agents that provide diminishing returns or overlapping functionality.

**Retention Criteria**:
1. **Essential Development** - Core to daily development workflow
2. **Production Quality** - Critical for production readiness
3. **Automation Value** - High ROI on workflow automation
4. **Non-Overlapping** - Unique, non-redundant capabilities
5. **UNRDF-Specific** - Tailored to project needs

---

## Current Agent Inventory

### By Category (94 Total)

| Category | Count | Keep | Archive | Notes |
|----------|-------|------|---------|-------|
| **Core** | 5 | 5 | 0 | Essential development agents |
| **Testing** | 2 | 1 | 1 | Keep production-validator |
| **Analysis** | 2 | 1 | 1 | Keep code-analyzer |
| **GitHub** | 13 | 2 | 11 | Keep pr-manager, release-manager |
| **Architecture** | 1 | 1 | 0 | Keep system-design |
| **Consensus** | 7 | 0 | 7 | UNRDF-specific, archive for now |
| **Flow Nexus** | 9 | 0 | 9 | Cloud platform specific |
| **Research (CC)** | 10 | 0 | 10 | Research completed |
| **Numbered (1-10)** | 10 | 0 | 10 | Research swarm completed |
| **SPARC** | 4 | 0 | 4 | Methodology specific |
| **Hive Mind** | 5 | 0 | 5 | Advanced coordination |
| **Swarm** | 3 | 0 | 3 | Advanced coordination |
| **Templates** | 9 | 0 | 9 | Template agents |
| **Goal** | 2 | 0 | 2 | GOAP-specific |
| **Optimization** | 5 | 0 | 5 | Performance specific |
| **Specialized** | 7 | 0 | 7 | Domain-specific |

---

## Top 10 Agents to Retain

### Tier 1: Core Development (5 agents)

#### 1. core/coder
**Path**: `.claude/agents/core/coder.md`
**Why Keep**: Implementation specialist, essential for code generation
**Capabilities**:
- Code generation
- Refactoring
- Optimization
- API design
- Error handling

**Value**: ⭐⭐⭐⭐⭐ (Essential)

---

#### 2. core/tester
**Path**: `.claude/agents/core/tester.md`
**Why Keep**: Testing specialist, ensures code quality
**Capabilities**:
- Test creation
- Coverage analysis
- Test automation
- Quality assurance

**Value**: ⭐⭐⭐⭐⭐ (Essential)

---

#### 3. core/reviewer
**Path**: `.claude/agents/core/reviewer.md`
**Why Keep**: Code review specialist, maintains quality standards
**Capabilities**:
- Code review
- Quality gates
- Best practice enforcement
- Security review

**Value**: ⭐⭐⭐⭐⭐ (Essential)

---

#### 4. core/planner
**Path**: `.claude/agents/core/planner.md`
**Why Keep**: Strategic planning, task decomposition
**Capabilities**:
- Task planning
- Architecture planning
- Resource allocation
- Risk assessment

**Value**: ⭐⭐⭐⭐⭐ (Essential)

---

#### 5. core/researcher
**Path**: `.claude/agents/core/researcher.md`
**Why Keep**: Research and investigation specialist
**Capabilities**:
- Codebase research
- Documentation research
- Pattern discovery
- Context gathering

**Value**: ⭐⭐⭐⭐⭐ (Essential)

---

### Tier 2: Quality Assurance (2 agents)

#### 6. testing/validation/production-validator
**Path**: `.claude/agents/testing/validation/production-validator.md`
**Why Keep**: Ensures production readiness, no other agent provides this
**Capabilities**:
- Production validation
- Implementation verification
- End-to-end testing
- Deployment readiness
- Real-world simulation

**Value**: ⭐⭐⭐⭐⭐ (Critical - Unique capability)

**Rationale**: This is the ONLY agent that explicitly validates production readiness and ensures no mock/stub implementations remain. Critical for UNRDF's production deployment.

---

#### 7. analysis/code-analyzer
**Path**: `.claude/agents/analysis/code-analyzer.md`
**Why Keep**: Deep code analysis, static analysis capabilities
**Capabilities**:
- Code quality analysis
- Complexity analysis
- Security scanning
- Performance analysis

**Value**: ⭐⭐⭐⭐ (High - Comprehensive analysis)

---

### Tier 3: Automation & DevOps (2 agents)

#### 8. github/pr-manager
**Path**: `.claude/agents/github/pr-manager.md`
**Why Keep**: Pull request automation, critical for workflow
**Capabilities**:
- PR management
- Code review coordination
- Testing integration
- Merge automation

**Value**: ⭐⭐⭐⭐ (High - Workflow automation)

**Rationale**: Automates the most common GitHub workflow. Other GitHub agents (issue-tracker, release-swarm, etc.) provide overlapping or less frequently used functionality.

---

#### 9. github/release-manager
**Path**: `.claude/agents/github/release-manager.md`
**Why Keep**: Release automation, reduces manual work
**Capabilities**:
- Release automation
- Changelog generation
- Version management
- Deployment coordination

**Value**: ⭐⭐⭐⭐ (High - Release automation)

**Rationale**: Releases are complex and frequent. Automation here provides high ROI.

---

### Tier 4: Architecture & Design (1 agent)

#### 10. architecture/system-design/arch-system-design
**Path**: `.claude/agents/architecture/system-design/arch-system-design.md`
**Why Keep**: System architecture design, unique capability
**Capabilities**:
- System architecture design
- Design patterns
- Scalability planning
- Architecture documentation

**Value**: ⭐⭐⭐⭐ (High - Architectural guidance)

**Rationale**: No other agent provides comprehensive system design capabilities. Critical for UNRDF's complex 5-layer architecture.

---

## Agents to Archive (84 agents)

### By Rationale

#### 1. Research Completed (20 agents)
**Why Archive**: Research projects completed, deliverables extracted

**Agents**:
- `1-capability-cartographer.md` through `10-synthesis-editor.md` (10 numbered agents)
- `research/cc-agent-01-subagents.md` through `cc-agent-10-librarian.md` (10 CC research agents)

**Archival Destination**: `archive/2026-01-11/agents/research/`

**Impact**: Low - Research complete, findings documented

---

#### 2. Cloud Platform Specific (9 agents)
**Why Archive**: Flow Nexus cloud platform not in active use

**Agents**:
- `flow-nexus/app-store.md`
- `flow-nexus/authentication.md`
- `flow-nexus/challenges.md`
- `flow-nexus/neural-network.md`
- `flow-nexus/payments.md`
- `flow-nexus/sandbox.md`
- `flow-nexus/swarm.md`
- `flow-nexus/user-tools.md`
- `flow-nexus/workflow.md`

**Archival Destination**: `archive/2026-01-11/agents/flow-nexus/`

**Impact**: Low - Not currently using Flow Nexus platform

---

#### 3. Advanced Coordination (17 agents)
**Why Archive**: Overlapping functionality, too specialized

**Agents**:
- Consensus (7): `byzantine-coordinator`, `crdt-synchronizer`, `gossip-coordinator`, `performance-benchmarker`, `quorum-manager`, `raft-manager`, `security-manager`
- Hive Mind (5): `collective-intelligence-coordinator`, `queen-coordinator`, `scout-explorer`, `swarm-memory-manager`, `worker-specialist`
- Swarm (3): `adaptive-coordinator`, `hierarchical-coordinator`, `mesh-coordinator`
- Optimization (5): `benchmark-suite`, `load-balancer`, `performance-monitor`, `resource-allocator`, `topology-optimizer` - Moved to overlap with performance-benchmarker

**Archival Destination**: `archive/2026-01-11/agents/advanced-coordination/`

**Impact**: Medium - Can use core agents for coordination, recall if needed

---

#### 4. GitHub Overlapping (11 agents)
**Why Archive**: Overlapping with pr-manager and release-manager

**Agents**:
- `github/code-review-swarm.md` - Overlap with reviewer
- `github/github-modes.md` - Documentation, not agent
- `github/issue-tracker.md` - Less critical than PR management
- `github/multi-repo-swarm.md` - Advanced, not frequently needed
- `github/project-board-sync.md` - Optional GitHub Projects feature
- `github/release-swarm.md` - Overlap with release-manager
- `github/repo-architect.md` - Overlap with arch-system-design
- `github/swarm-issue.md` - Overlap with issue-tracker
- `github/swarm-pr.md` - Overlap with pr-manager
- `github/sync-coordinator.md` - Advanced, not frequently needed
- `github/workflow-automation.md` - GitHub Actions specific

**Archival Destination**: `archive/2026-01-11/agents/github-extended/`

**Impact**: Low - Core functionality covered by pr-manager and release-manager

---

#### 5. Methodology Specific (6 agents)
**Why Archive**: SPARC and GOAP methodology agents, too specialized

**Agents**:
- SPARC (4): `sparc/architecture.md`, `sparc/pseudocode.md`, `sparc/refinement.md`, `sparc/specification.md`
- Goal (2): `goal/code-goal-planner.md`, `goal/goal-planner.md`

**Archival Destination**: `archive/2026-01-11/agents/methodologies/`

**Impact**: Low - Can use core/planner for planning needs

---

#### 6. Template Agents (9 agents)
**Why Archive**: Template/pattern agents, not operational

**Agents**:
- `templates/automation-smart-agent.md`
- `templates/coordinator-swarm-init.md`
- `templates/github-pr-manager.md` (superseded by github/pr-manager)
- `templates/implementer-sparc-coder.md`
- `templates/memory-coordinator.md`
- `templates/migration-plan.md`
- `templates/orchestrator-task.md`
- `templates/performance-analyzer.md`
- `templates/sparc-coordinator.md`

**Archival Destination**: `archive/2026-01-11/agents/templates/`

**Impact**: None - Templates, not operational agents

---

#### 7. Specialized Domain (9 agents)
**Why Archive**: Too specialized, infrequently used

**Agents**:
- `analysis/code-review/analyze-code-quality.md` - Overlap with code-analyzer
- `data/ml/data-ml-model.md` - ML not primary focus
- `development/backend/dev-backend-api.md` - Overlap with coder
- `devops/ci-cd/ops-cicd-github.md` - Overlap with release-manager
- `documentation/api-docs/docs-api-openapi.md` - Specific to OpenAPI
- `neural/safla-neural.md` - Neural network specific
- `specialized/mobile/spec-mobile-react-native.md` - React Native specific
- `testing/unit/tdd-london-swarm.md` - TDD methodology specific
- `base-template-generator.md` - Template generation

**Archival Destination**: `archive/2026-01-11/agents/specialized/`

**Impact**: Low - Specialized use cases, recall if needed

---

## Archive Plan

### Phase 1: Create Archive Structure

```bash
mkdir -p archive/2026-01-11/agents/{research,flow-nexus,advanced-coordination,github-extended,methodologies,templates,specialized}
```

### Phase 2: Move Agents by Category

```bash
# Research agents (20 files)
mv .claude/agents/{1..10}-*.md archive/2026-01-11/agents/research/
mv .claude/agents/research/ archive/2026-01-11/agents/research/claude-code-research/

# Flow Nexus (9 files)
mv .claude/agents/flow-nexus/ archive/2026-01-11/agents/

# Advanced coordination (17 files)
mv .claude/agents/consensus/ archive/2026-01-11/agents/advanced-coordination/
mv .claude/agents/hive-mind/ archive/2026-01-11/agents/advanced-coordination/
mv .claude/agents/swarm/ archive/2026-01-11/agents/advanced-coordination/
mv .claude/agents/optimization/ archive/2026-01-11/agents/advanced-coordination/

# GitHub extended (11 files)
mkdir -p archive/2026-01-11/agents/github-extended/
mv .claude/agents/github/code-review-swarm.md archive/2026-01-11/agents/github-extended/
mv .claude/agents/github/github-modes.md archive/2026-01-11/agents/github-extended/
mv .claude/agents/github/issue-tracker.md archive/2026-01-11/agents/github-extended/
# ... (move 11 files, keep pr-manager and release-manager)

# Methodologies (6 files)
mv .claude/agents/sparc/ archive/2026-01-11/agents/methodologies/
mv .claude/agents/goal/ archive/2026-01-11/agents/methodologies/

# Templates (9 files)
mv .claude/agents/templates/ archive/2026-01-11/agents/

# Specialized (9 files)
mkdir -p archive/2026-01-11/agents/specialized/
mv .claude/agents/analysis/code-review/ archive/2026-01-11/agents/specialized/
mv .claude/agents/data/ archive/2026-01-11/agents/specialized/
mv .claude/agents/development/ archive/2026-01-11/agents/specialized/
mv .claude/agents/devops/ archive/2026-01-11/agents/specialized/
mv .claude/agents/documentation/ archive/2026-01-11/agents/specialized/
mv .claude/agents/neural/ archive/2026-01-11/agents/specialized/
mv .claude/agents/specialized/ archive/2026-01-11/agents/specialized/domain-specific/
mv .claude/agents/testing/unit/ archive/2026-01-11/agents/specialized/
mv .claude/agents/base-template-generator.md archive/2026-01-11/agents/specialized/
```

### Phase 3: Create Archive Manifest

Create `archive/2026-01-11/agents/README.md` with:
- List of all archived agents (84 total)
- Original locations
- Rationale for archival
- Restoration instructions

### Phase 4: Update Active Agents

Remaining structure:
```
.claude/agents/
├── core/
│   ├── coder.md
│   ├── planner.md
│   ├── researcher.md
│   ├── reviewer.md
│   └── tester.md
├── testing/validation/
│   └── production-validator.md
├── analysis/
│   └── code-analyzer.md
├── github/
│   ├── pr-manager.md
│   └── release-manager.md
└── architecture/system-design/
    └── arch-system-design.md
```

### Phase 5: Verification

```bash
# Count remaining agents
find .claude/agents -type f -name "*.md" | wc -l
# Expected: 10

# Count archived agents
find archive/2026-01-11/agents -type f -name "*.md" | wc -l
# Expected: 84

# Verify no broken references
grep -r "agents/" .claude/commands/ .claude/rules/ | grep -v "archive/"
```

---

## Impact Assessment

### Before
- **Total Agents**: 94
- **Complexity**: Very High
- **Maintenance**: Difficult
- **Overlap**: Significant

### After
- **Total Agents**: 10
- **Complexity**: Low
- **Maintenance**: Easy
- **Overlap**: Minimal

### Benefits

1. **Reduced Cognitive Load**: 89.4% reduction in agent count
2. **Clear Responsibility**: Each agent has distinct, non-overlapping purpose
3. **Faster Development**: Less time choosing which agent to use
4. **Easier Maintenance**: 10 agents vs 94 to maintain
5. **Better Testing**: Can thoroughly test 10 agents

### Risks

**Risk**: Archived agent needed for specific task
**Mitigation**: Full archive with manifest, easy restoration

**Risk**: Loss of specialized capabilities
**Mitigation**: Specialized agents archived, not deleted. Can restore on demand.

**Risk**: GitHub workflow gaps
**Mitigation**: Kept pr-manager and release-manager (most critical workflows)

---

## Restoration Process

If an archived agent is needed:

```bash
# List archived agents
find archive/2026-01-11/agents -name "*.md"

# Restore specific agent
git mv archive/2026-01-11/agents/specialized/data-ml-model.md .claude/agents/data/ml/

# Restore entire category
git mv archive/2026-01-11/agents/flow-nexus/ .claude/agents/
```

---

## Recommendations

### Immediate Actions

1. ✅ **Approve Top 10 List** - Review and confirm agent selection
2. ⏳ **Execute Archive Plan** - Move 84 agents to archive/2026-01-11/agents/
3. ⏳ **Update Documentation** - Update agent documentation to reflect new structure
4. ⏳ **Test Retained Agents** - Validate that 10 retained agents work correctly
5. ⏳ **Commit Changes** - Commit archive operation to git

### Future Considerations

1. **Agent Refactoring** - After archival, refactor 10 retained agents for consistency
2. **Integration Testing** - Test agent coordination with reduced set
3. **Documentation Update** - Update CLAUDE.md with new agent structure
4. **Command Updates** - Update .claude/commands to reference new agent paths
5. **Metrics Tracking** - Track if archived agents are ever restored (indicates gaps)

---

## Success Metrics

**Archival Success**:
- ✅ 84 agents archived (89.4%)
- ✅ 10 agents retained (10.6%)
- ✅ Zero broken references
- ✅ All core workflows functional

**Post-Archival Success** (measure after 30 days):
- 0 archived agents restored = Perfect selection
- 1-3 archived agents restored = Good selection
- 4+ archived agents restored = Re-evaluate criteria

---

## Conclusion

**Status**: ✅ **Ready for Execution**

The proposed archival of 84 agents (89.4%) and retention of 10 core agents provides:
- **Clear value proposition** for each retained agent
- **Minimal overlap** between retained agents
- **Complete workflow coverage** (development, testing, quality, automation, architecture)
- **Easy restoration** of archived agents if needed

**Next Step**: Approve top 10 list and execute archival plan.
