# Claude Code Capability Research: 10-Agent Swarm Orchestration Prompt

Use this prompt to spawn all 10 research agents in parallel. Copy and paste into Claude Code.

---

## Spawn All 10 Agents

I need to systematically explore Claude Code's capability surface. Spawn all 10 research agents in parallel using the Task tool.

**CRITICAL**: Make all 10 Task calls in a SINGLE message for parallel execution.

### Agent 1: Subagents & Delegation
```
Task("Subagent exploration",
     "Research Claude Code's subagent system. Catalog all agent types, test 10-way parallel decomposition, document delegation patterns. Return structured JSON with evidence.",
     "researcher")
```

### Agent 2: Hooks & Tool Governance
```
Task("Hook system research",
     "Research Claude Code's hook system. Document lifecycle events (PreToolUse, PostToolUse, etc.), test matcher patterns, create policy catalog with 10+ patterns. Return structured JSON with evidence.",
     "researcher")
```

### Agent 3: Plugins
```
Task("Plugin system research",
     "Research Claude Code's plugin system. Document structure, test installation, create minimal plugin skeleton with command+agent+hook. Return structured JSON with evidence.",
     "researcher")
```

### Agent 4: Slash Commands
```
Task("Slash command research",
     "Research Claude Code's slash command system. Catalog built-in commands, document syntax with frontmatter and arguments, create research workflow commands. Return structured JSON with evidence.",
     "researcher")
```

### Agent 5: MCP (Model Context Protocol)
```
Task("MCP integration research",
     "Research Claude Code's MCP integration. Document config format, test tool discovery, map permission model, identify risk boundaries. Return structured JSON with evidence.",
     "researcher")
```

### Agent 6: Programmatic Execution
```
Task("Programmatic mode research",
     "Research Claude Code's programmatic/headless execution. Document CLI flags, test output formats (json, stream-json), research session resume, create pipeline patterns. Return structured JSON with evidence.",
     "researcher")
```

### Agent 7: Checkpointing
```
Task("Checkpoint system research",
     "Research Claude Code's checkpointing and rewind. Document creation triggers, test all rewind options, measure recovery times, quantify risk tolerance impact. Return structured JSON with evidence.",
     "researcher")
```

### Agent 8: IDE/VS Code Surface
```
Task("IDE extension research",
     "Research Claude Code's VS Code extension. Inventory features, build CLI vs Extension parity matrix, document workflow implications, test @-mentions. Return structured JSON with evidence.",
     "researcher")
```

### Agent 9: Composition Hunter
```
Task("Composition testing",
     "Test cross-compositions of Claude Code capabilities. Identify high-value combinations (hooks+subagents+programmatic, etc.), execute composition tests, measure emergent properties using novelty metrics. Return structured JSON with evidence.",
     "researcher")
```

### Agent 10: Librarian
```
Task("Research synthesis",
     "Synthesize all capability research findings. Build complete capability lattice, generate Diataxis documentation index, rank unexplored frontier by expected value, produce executive summary. Return structured JSON.",
     "researcher")
```

---

## After Agents Return

1. Collect all 10 agent findings
2. Update `research/claude-code-capabilities/capability-lattice.json` with discovered edges
3. Update `research/claude-code-capabilities/composition-closure-report.md` with results
4. Update `research/claude-code-capabilities/novelty-metrics.json` with measurements
5. Commit findings with evidence

---

## Validation Questions

Before declaring complete:
- [ ] Did all 10 agents return findings?
- [ ] Are findings based on actual tests (not theory)?
- [ ] Is evidence captured (commands, outputs)?
- [ ] Are novelty metrics calculated?
- [ ] Is the capability lattice updated?
