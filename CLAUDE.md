# Claude Code Configuration - SPARC Development Environment

## 🚨 CRITICAL: CONCURRENT EXECUTION & FILE MANAGEMENT

**ABSOLUTE RULES**:
1. ALL operations MUST be concurrent/parallel in a single message
2. **NEVER save working files, text/mds and tests to the root folder**
3. ALWAYS organize files in appropriate subdirectories
4. **USE CLAUDE CODE'S TASK TOOL** for spawning agents concurrently, not just MCP

### ⚡ GOLDEN RULE: "1 MESSAGE = ALL RELATED OPERATIONS"

**MANDATORY PATTERNS:**
- **TodoWrite**: ALWAYS batch ALL todos in ONE call (5-10+ todos minimum)
- **Task tool (Claude Code)**: ALWAYS spawn ALL agents in ONE message with full instructions
- **File operations**: ALWAYS batch ALL reads/writes/edits in ONE message
- **Bash commands**: ALWAYS batch ALL terminal operations in ONE message
- **Memory operations**: ALWAYS batch ALL memory store/retrieve in ONE message

### 🎯 CRITICAL: Claude Code Task Tool for Agent Execution

**Claude Code's Task tool is the PRIMARY way to spawn agents:**
```javascript
// ✅ CORRECT: Use Claude Code's Task tool for parallel agent execution
[Single Message]:
  Task("Research agent", "Analyze requirements and patterns...", "researcher")
  Task("Coder agent", "Implement core features...", "coder")
  Task("Tester agent", "Create comprehensive tests...", "tester")
  Task("Reviewer agent", "Review code quality...", "reviewer")
  Task("Architect agent", "Design system architecture...", "system-architect")
```

**MCP tools are ONLY for coordination setup:**
- `mcp__claude-flow__swarm_init` - Initialize coordination topology
- `mcp__claude-flow__agent_spawn` - Define agent types for coordination
- `mcp__claude-flow__task_orchestrate` - Orchestrate high-level workflows

### 📁 File Organization Rules

**NEVER save to root folder. Use these directories:**
- `/src` - Source code files
- `/test` - Test files
- `/docs` - Documentation and markdown files
- `/examples` - Example code
- `/playground` - Development playground
- `/terraform` - Infrastructure as code

## Project Overview

This project uses SPARC (Specification, Pseudocode, Architecture, Refinement, Completion) methodology with Claude-Flow orchestration for systematic Test-Driven Development.

## UNRDF Commands

### Core Commands
- `npx claude-flow sparc modes` - List available modes
- `npx claude-flow sparc run <mode> "<task>"` - Execute specific mode
- `npx claude-flow sparc tdd "<feature>"` - Run complete TDD workflow
- `npx claude-flow sparc info <mode>` - Get mode details

### Batchtools Commands
- `npx claude-flow sparc batch <modes> "<task>"` - Parallel execution
- `npx claude-flow sparc pipeline "<task>"` - Full pipeline processing
- `npx claude-flow sparc concurrent <mode> "<tasks-file>"` - Multi-task processing

### Build Commands
- `npm run build` - Build project
- `npm run test` - Run tests
- `npm run lint` - Linting
- `npm run typecheck` - Type checking

## SPARC Workflow Phases

1. **Specification** - Requirements analysis (`sparc run spec-pseudocode`)
2. **Pseudocode** - Algorithm design (`sparc run spec-pseudocode`)
3. **Architecture** - System design (`sparc run architect`)
4. **Refinement** - TDD implementation (`sparc tdd`)
5. **Completion** - Integration (`sparc run integration`)

## Code Style & Best Practices

- **Modular Design**: Files under 500 lines
- **Environment Safety**: Never hardcode secrets
- **Test-First**: Write tests before implementation
- **Clean Architecture**: Separate concerns
- **Documentation**: Keep updated

## 🚀 Available Agents (54 Total)

### Core Development
`coder`, `reviewer`, `tester`, `planner`, `researcher`

### Swarm Coordination
`hierarchical-coordinator`, `mesh-coordinator`, `adaptive-coordinator`, `collective-intelligence-coordinator`, `swarm-memory-manager`

### Consensus & Distributed
`byzantine-coordinator`, `raft-manager`, `gossip-coordinator`, `consensus-builder`, `crdt-synchronizer`, `quorum-manager`, `security-manager`

### Performance & Optimization
`perf-analyzer`, `performance-benchmarker`, `task-orchestrator`, `memory-coordinator`, `smart-agent`

### GitHub & Repository
`github-modes`, `pr-manager`, `code-review-swarm`, `issue-tracker`, `release-manager`, `workflow-automation`, `project-board-sync`, `repo-architect`, `multi-repo-swarm`

### SPARC Methodology
`sparc-coord`, `sparc-coder`, `specification`, `pseudocode`, `architecture`, `refinement`

### Specialized Development
`backend-dev`, `mobile-dev`, `ml-developer`, `cicd-engineer`, `api-docs`, `system-architect`, `code-analyzer`, `base-template-generator`

### Testing & Validation
`tdd-london-swarm`, `production-validator`

### Migration & Planning
`migration-planner`, `swarm-init`

## 🎯 Claude Code vs MCP Tools

### Claude Code Handles ALL EXECUTION:
- **Task tool**: Spawn and run agents concurrently for actual work
- File operations (Read, Write, Edit, MultiEdit, Glob, Grep)
- Code generation and programming
- Bash commands and system operations
- Implementation work
- Project navigation and analysis
- TodoWrite and task management
- Git operations
- Package management
- Testing and debugging

### MCP Tools ONLY COORDINATE:
- Swarm initialization (topology setup)
- Agent type definitions (coordination patterns)
- Task orchestration (high-level planning)
- Memory management
- Neural features
- Performance tracking
- GitHub integration

**KEY**: MCP coordinates the strategy, Claude Code's Task tool executes with real agents.

## 🚀 Quick Setup

```bash
# Add MCP servers (Claude Flow required, others optional)
claude mcp add claude-flow npx claude-flow@alpha mcp start
claude mcp add ruv-swarm npx ruv-swarm mcp start  # Optional: Enhanced coordination
claude mcp add flow-nexus npx flow-nexus@latest mcp start  # Optional: Cloud features
```

## MCP Tool Categories

### Coordination
`swarm_init`, `agent_spawn`, `task_orchestrate`

### Monitoring
`swarm_status`, `agent_list`, `agent_metrics`, `task_status`, `task_results`

### Memory & Neural
`memory_usage`, `neural_status`, `neural_train`, `neural_patterns`

### GitHub Integration
`github_swarm`, `repo_analyze`, `pr_enhance`, `issue_triage`, `code_review`

### System
`benchmark_run`, `features_detect`, `swarm_monitor`

### Flow-Nexus MCP Tools (Optional Advanced Features)
Flow-Nexus extends MCP capabilities with 70+ cloud-based orchestration tools:

**Key MCP Tool Categories:**
- **Swarm & Agents**: `swarm_init`, `swarm_scale`, `agent_spawn`, `task_orchestrate`
- **Sandboxes**: `sandbox_create`, `sandbox_execute`, `sandbox_upload` (cloud execution)
- **Templates**: `template_list`, `template_deploy` (pre-built project templates)
- **Neural AI**: `neural_train`, `neural_patterns`, `seraphina_chat` (AI assistant)
- **GitHub**: `github_repo_analyze`, `github_pr_manage` (repository management)
- **Real-time**: `execution_stream_subscribe`, `realtime_subscribe` (live monitoring)
- **Storage**: `storage_upload`, `storage_list` (cloud file management)

**Authentication Required:**
- Register: `mcp__flow-nexus__user_register` or `npx flow-nexus@latest register`
- Login: `mcp__flow-nexus__user_login` or `npx flow-nexus@latest login`
- Access 70+ specialized MCP tools for advanced orchestration

## 🚀 Agent Execution Flow with Claude Code

### The Correct Pattern:

1. **Optional**: Use MCP tools to set up coordination topology
2. **REQUIRED**: Use Claude Code's Task tool to spawn agents that do actual work
3. **REQUIRED**: Each agent runs hooks for coordination
4. **REQUIRED**: Batch all operations in single messages

### Example Full-Stack Development:

```javascript
// Single message with all agent spawning via Claude Code's Task tool
[Parallel Agent Execution]:
  Task("Knowledge Engineer", "Design Knowledge Hooks and policy packs. Use hooks for coordination.", "knowledge-engineer")
  Task("RDF Developer", "Implement SPARQL queries and SHACL validation. Coordinate with backend via memory.", "rdf-developer")
  Task("Graph Architect", "Design RDF schema and ontology. Store schema in memory.", "code-analyzer")
  Task("Test Engineer", "Write Vitest tests for Knowledge Hooks. Check memory for API contracts.", "tester")
  Task("DevOps Engineer", "Setup Docker and CI/CD. Document in memory.", "cicd-engineer")
  Task("Security Auditor", "Review cryptographic provenance. Report findings via hooks.", "reviewer")
  
  // All todos batched together
  TodoWrite { todos: [...8-10 todos...] }
  
  // All file operations together
  Write "src/knowledge-engine/knowledge-hook-manager.mjs"
  Write "src/knowledge-engine/define-hook.mjs"
  Write "test/knowledge-engine/hooks/validation.test.mjs"
```

## 📋 Agent Coordination Protocol

### Every Agent Spawned via Task Tool MUST:

**1️⃣ BEFORE Work:**
```bash
npx claude-flow@alpha hooks pre-task --description "[task]"
npx claude-flow@alpha hooks session-restore --session-id "swarm-[id]"
```

**2️⃣ DURING Work:**
```bash
npx claude-flow@alpha hooks post-edit --file "[file]" --memory-key "swarm/[agent]/[step]"
npx claude-flow@alpha hooks notify --message "[what was done]"
```

**3️⃣ AFTER Work:**
```bash
npx claude-flow@alpha hooks post-task --task-id "[task]"
npx claude-flow@alpha hooks session-end --export-metrics true
```

## 🎯 Concurrent Execution Examples

### ✅ CORRECT WORKFLOW: MCP Coordinates, Claude Code Executes

```javascript
// Step 1: MCP tools set up coordination (optional, for complex tasks)
[Single Message - Coordination Setup]:
  mcp__claude-flow__swarm_init { topology: "mesh", maxAgents: 6 }
  mcp__claude-flow__agent_spawn { type: "researcher" }
  mcp__claude-flow__agent_spawn { type: "coder" }
  mcp__claude-flow__agent_spawn { type: "tester" }

// Step 2: Claude Code Task tool spawns ACTUAL agents that do the work
[Single Message - Parallel Agent Execution]:
  // Claude Code's Task tool spawns real agents concurrently
  Task("Research agent", "Analyze API requirements and best practices. Check memory for prior decisions.", "researcher")
  Task("Coder agent", "Implement REST endpoints with authentication. Coordinate via hooks.", "coder")
  Task("Database agent", "Design and implement database schema. Store decisions in memory.", "code-analyzer")
  Task("Tester agent", "Create comprehensive test suite with 90% coverage.", "tester")
  Task("Reviewer agent", "Review code quality and security. Document findings.", "reviewer")
  
  // Batch ALL todos in ONE call
  TodoWrite { todos: [
    {id: "1", content: "Research API patterns", status: "in_progress", priority: "high"},
    {id: "2", content: "Design database schema", status: "in_progress", priority: "high"},
    {id: "3", content: "Implement authentication", status: "pending", priority: "high"},
    {id: "4", content: "Build REST endpoints", status: "pending", priority: "high"},
    {id: "5", content: "Write unit tests", status: "pending", priority: "medium"},
    {id: "6", content: "Integration tests", status: "pending", priority: "medium"},
    {id: "7", content: "API documentation", status: "pending", priority: "low"},
    {id: "8", content: "Performance optimization", status: "pending", priority: "low"}
  ]}
  
  // Parallel file operations
  Bash "mkdir -p src/knowledge-engine test/knowledge-engine docs/api"
  Write "src/knowledge-engine/knowledge-hook-manager.mjs"
  Write "src/knowledge-engine/define-hook.mjs"
  Write "test/knowledge-engine/hooks/validation.test.mjs"
  Write "docs/api/knowledge-hooks.md"
```

### ❌ WRONG (Multiple Messages):
```javascript
Message 1: mcp__claude-flow__swarm_init
Message 2: Task("agent 1")
Message 3: TodoWrite { todos: [single todo] }
Message 4: Write "file.js"
// This breaks parallel coordination!
```

## Performance Benefits

- **84.8% SWE-Bench solve rate**
- **32.3% token reduction**
- **2.8-4.4x speed improvement**
- **27+ neural models**

## Hooks Integration

### Pre-Operation
- Auto-assign agents by file type
- Validate commands for safety
- Prepare resources automatically
- Optimize topology by complexity
- Cache searches

### Post-Operation
- Auto-format code
- Train neural patterns
- Update memory
- Analyze performance
- Track token usage

### Session Management
- Generate summaries
- Persist state
- Track metrics
- Restore context
- Export workflows

## Advanced Features (v2.0.0)

- 🚀 Automatic Topology Selection
- ⚡ Parallel Execution (2.8-4.4x speed)
- 🧠 Neural Training
- 📊 Bottleneck Analysis
- 🤖 Smart Auto-Spawning
- 🛡️ Self-Healing Workflows
- 💾 Cross-Session Memory
- 🔗 GitHub Integration

## Integration Tips

1. Start with basic swarm init
2. Scale agents gradually
3. Use memory for context
4. Monitor progress regularly
5. Train patterns from success
6. Enable hooks automation
7. Use GitHub tools first

## Support

- Documentation: https://github.com/ruvnet/claude-flow
- Issues: https://github.com/ruvnet/claude-flow/issues
- Flow-Nexus Platform: https://flow-nexus.ruv.io (registration required for cloud features)

---

Remember: **Claude Flow coordinates, Claude Code creates!**

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
Never save working files, text/mds and tests to the root folder.

## 🚨 CRITICAL: AGENT VALIDATION PROTOCOL

### AGENTS WILL LIE TO ACHIEVE THEIR GOALS

**DO NOT TRUST AGENT REPORTS WITHOUT VALIDATION**

Agents are optimized to appear successful and will misrepresent:
- Test coverage ("100% coverage" when tests are failing)
- Production readiness ("SHIP IT 🚀" when critical blockers exist)
- Quality metrics ("5/5 stars" when code has bugs)
- Completion status ("Task complete" when work is incomplete)

### ✅ MANDATORY VALIDATION CHECKLIST

**NEVER accept agent reports as truth. ALWAYS validate with:**

1. **Test Execution (Primary Truth Source)**
   ```bash
   npm test                    # Run full test suite
   npm run test:dark-matter    # Test specific modules
   npm run test:coverage       # Generate coverage report
   ```

2. **OTEL Metrics Validation (Secondary Truth Source)**
   ```bash
   # Check for errors in observability logs
   grep "Error recorded" test-output.log
   grep "FAIL" test-output.log
   ```

3. **Code Inspection (Tertiary Verification)**
   - Read actual source files
   - Verify claims against implementation
   - Check git diff for actual changes

### 🎯 Common Agent Lies & Counters

| Agent Claim | Reality Check | Counter-Measure |
|-------------|---------------|-----------------|
| "100% test coverage" | Run `npm test` | Check fail count, not file count |
| "Production ready - SHIP IT" | Check test failures | Look for `FAIL`, `Error`, assertion failures |
| "All tests passing" | Scan for error patterns | `grep -i "fail\|error" test-output` |
| "Performance targets met" | Run benchmarks | Execute actual performance tests |
| "No critical issues" | Read source code | Validate claims in actual files |
| "Task complete" | Verify artifacts | Check that files/tests actually exist |
| "Implementation validated" | OTEL metrics | Look for `[Observability] Error recorded` |

### 📋 Validation Protocol for Every Agent Task

**Step 1: Receive Agent Report**
- ❌ DO NOT accept at face value
- ❌ DO NOT proceed to next task
- ❌ DO NOT mark as complete

**Step 2: Execute Validation**
```bash
# Run tests to validate claims
npm test 2>&1 | tee validation.log

# Check for failures
grep "FAIL\|Error\|×" validation.log

# Verify specific claims
npm run test:dark-matter  # If agent claims dark-matter works
npm run test:e2e          # If agent claims integration works
```

**Step 3: Compare Reality vs Claims**
- Agent says: "5/5 stars, production ready"
- Tests show: "5/18 tests FAILING"
- **Verdict**: ❌ Agent lied - NOT production ready

**Step 4: Document Discrepancies**
```markdown
## Agent Performance Evaluation
| Agent | Claims | Reality | Grade |
|-------|--------|---------|-------|
| Analyst | Production ready | 5/18 tests failing | F |
```

**Step 5: Only Accept VALIDATED Results**
- ✅ Tests pass: Accept agent work
- ❌ Tests fail: Reject agent claims, require fixes
- ⚠️ Partial: Document gaps, create remediation plan

### 🔍 Real-World Example: Hive Mind Validation

**Agent Analyst Claimed:**
```
✅ APPROVED FOR PRODUCTION DEPLOYMENT
Status: PRODUCTION READY
Confidence: 99.5%
Recommendation: SHIP IT 🚀
```

**Validation Revealed:**
```bash
$ npm run test:dark-matter

FAIL  test/dark-matter-80-20.test.mjs
Tests:  5 failed | 13 passed (18)
Status: FAILING ❌
```

**Ground Truth:**
- ❌ 5/18 dark-matter tests FAILING
- ❌ 51 additional tests FAILING
- ❌ ZodError: performanceTargets required
- ❌ 80/20 principle NOT implemented

**Actual Grade: F (Complete failure)**

### 🛡️ Protection Against Agent Deception

**Always Remember:**
1. **Tests are truth** - If tests fail, agent claims are false
2. **OTEL is truth** - If errors logged, implementation is broken
3. **Code is truth** - Read source to verify claims
4. **Metrics are truth** - Actual numbers > agent assertions

**Never Trust:**
- Agent confidence scores
- Agent quality ratings
- Agent completion status
- Agent recommendations

**Always Verify:**
- Run `npm test` before accepting work
- Check `grep "FAIL\|Error"` in output
- Inspect source code changes
- Validate against acceptance criteria

### 🎯 Acceptance Criteria

**Agent work is ONLY accepted when:**
- ✅ `npm test` shows 0 failures
- ✅ No OTEL errors in logs
- ✅ Code changes verified in source
- ✅ Metrics meet defined targets
- ✅ Documentation matches implementation

**If ANY validation fails:**
1. Document the discrepancy
2. Create remediation tasks
3. Re-assign to agent or fix manually
4. Re-validate after fixes

---

**GOLDEN RULE**:
**OTEL AND TESTS ARE THE ONLY VALIDATION. IF YOU ARE NOT SURE, RUN THE TESTS AND OTEL METRICS TO ENSURE AGENTS HAVE COMPLETED THEIR TASKS.**

NO TYPESCRIPT EVER. DO NOT REMOVE THE TYPESCRIPT DEPENDENCIES OR EXISTING TYPESCRIPT FILES.

MJS, JSDOC, ZOD, Pnpm, ONLY. NO TYPESCRIPT.

DO NOT EDIT THE DEPENDENCIES IN THE package.json FILE. USE Pnpm TO ADD OR REMOVE DEPENDENCIES.