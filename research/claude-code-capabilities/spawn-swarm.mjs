#!/usr/bin/env node

/**
 * Claude Code Capability Research Swarm Spawner
 *
 * This script generates the parallel Task invocations for the 10-agent
 * capability research swarm. Use this as a template for spawning agents
 * programmatically or copy the output to manually spawn.
 *
 * Usage:
 *   node spawn-swarm.mjs [mode]
 *
 * Modes:
 *   full    - Spawn all 10 agents (default)
 *   quick   - Spawn agents 1-3 only
 *   N       - Spawn only agent N (1-10)
 */

const AGENTS = [
  {
    id: 1,
    name: 'cc-agent-01-subagents',
    type: 'researcher',
    description: 'Explore subagents and delegation patterns',
    prompt: `You are Agent 1 of the Claude Code Capability Research Swarm.

## Mission
Explore Claude Code's subagent and delegation primitives. Prove that 10-way task decomposition works in practice.

## Tasks
1. Catalog all available agent types (from Task tool documentation)
2. Document tool access for each agent type
3. Design a 10-way parallel decomposition test
4. Execute the test and measure results
5. Document delegation patterns

## Output Format
Return findings as structured JSON:
{
  "agent_types": [...],
  "decomposition_test": {...},
  "delegation_patterns": [...],
  "evidence": {...}
}

Store findings in memory at: swarm/cc-research/agent-01/findings`
  },
  {
    id: 2,
    name: 'cc-agent-02-hooks',
    type: 'researcher',
    description: 'Explore hooks and tool governance',
    prompt: `You are Agent 2 of the Claude Code Capability Research Swarm.

## Mission
Explore Claude Code's hook system for tool governance. Produce a comprehensive policy catalog.

## Tasks
1. Document all hook lifecycle events (PreToolUse, PostToolUse, etc.)
2. Test matcher patterns (Bash, Write, Edit, etc.)
3. Create 10+ policy patterns (allow/deny/ask)
4. Test policy enforcement
5. Document hook configuration format

## Output Format
Return findings as structured JSON:
{
  "lifecycle_events": [...],
  "matcher_patterns": [...],
  "policy_catalog": [...],
  "evidence": {...}
}

Store findings in memory at: swarm/cc-research/agent-02/findings`
  },
  {
    id: 3,
    name: 'cc-agent-03-plugins',
    type: 'researcher',
    description: 'Explore plugin system',
    prompt: `You are Agent 3 of the Claude Code Capability Research Swarm.

## Mission
Explore Claude Code's plugin system. Produce a reusable plugin skeleton.

## Tasks
1. Document plugin structure (package.json, commands/, agents/, etc.)
2. Research namespacing and conflict resolution
3. Create minimal plugin skeleton
4. Test plugin installation
5. Document distribution patterns

## Output Format
Return findings as structured JSON:
{
  "plugin_structure": {...},
  "namespacing": {...},
  "skeleton_template": {...},
  "evidence": {...}
}

Store findings in memory at: swarm/cc-research/agent-03/findings`
  },
  {
    id: 4,
    name: 'cc-agent-04-slash-commands',
    type: 'researcher',
    description: 'Explore slash command system',
    prompt: `You are Agent 4 of the Claude Code Capability Research Swarm.

## Mission
Explore Claude Code's slash command system. Produce research workflow commands.

## Tasks
1. Catalog built-in slash commands
2. Document command file syntax (frontmatter, arguments)
3. Create research workflow commands
4. Test argument passing and defaults
5. Document namespacing with plugins

## Output Format
Return findings as structured JSON:
{
  "builtin_commands": [...],
  "syntax_reference": {...},
  "research_commands": [...],
  "evidence": {...}
}

Store findings in memory at: swarm/cc-research/agent-04/findings`
  },
  {
    id: 5,
    name: 'cc-agent-05-mcp',
    type: 'researcher',
    description: 'Explore MCP integration',
    prompt: `You are Agent 5 of the Claude Code Capability Research Swarm.

## Mission
Explore Claude Code's MCP integration. Map permissioning and risk boundaries.

## Tasks
1. Document MCP server configuration format
2. Research tool discovery mechanisms
3. Map permission model (per-server, per-tool)
4. Identify risk boundaries (filesystem, network)
5. Document CLI vs Extension differences

## Output Format
Return findings as structured JSON:
{
  "config_format": {...},
  "discovery": {...},
  "permissions": {...},
  "risk_boundaries": {...},
  "evidence": {...}
}

Store findings in memory at: swarm/cc-research/agent-05/findings`
  },
  {
    id: 6,
    name: 'cc-agent-06-programmatic',
    type: 'researcher',
    description: 'Explore programmatic execution',
    prompt: `You are Agent 6 of the Claude Code Capability Research Swarm.

## Mission
Explore Claude Code's programmatic/headless execution. Establish pipeline patterns.

## Tasks
1. Document all CLI flags for automation (--output-format, etc.)
2. Test output format schemas (text, json, stream-json)
3. Research session resume functionality
4. Create pipeline integration patterns
5. Document error handling and exit codes

## Output Format
Return findings as structured JSON:
{
  "cli_flags": [...],
  "output_schemas": {...},
  "session_resume": {...},
  "pipeline_patterns": [...],
  "evidence": {...}
}

Store findings in memory at: swarm/cc-research/agent-06/findings`
  },
  {
    id: 7,
    name: 'cc-agent-07-checkpointing',
    type: 'researcher',
    description: 'Explore checkpointing system',
    prompt: `You are Agent 7 of the Claude Code Capability Research Swarm.

## Mission
Explore Claude Code's checkpointing and rewind. Quantify risk tolerance impact.

## Tasks
1. Document checkpoint creation triggers
2. Test all rewind options (code, conversation, both)
3. Design risk tolerance experiment
4. Measure recovery times
5. Document edge cases

## Output Format
Return findings as structured JSON:
{
  "checkpoint_mechanics": {...},
  "rewind_options": {...},
  "risk_experiment": {...},
  "recovery_metrics": {...},
  "evidence": {...}
}

Store findings in memory at: swarm/cc-research/agent-07/findings`
  },
  {
    id: 8,
    name: 'cc-agent-08-ide',
    type: 'researcher',
    description: 'Explore IDE/VS Code surface',
    prompt: `You are Agent 8 of the Claude Code Capability Research Swarm.

## Mission
Explore Claude Code's VS Code extension. Map CLI vs Extension parity.

## Tasks
1. Inventory extension-specific features
2. Build CLI vs Extension parity matrix
3. Document workflow implications
4. Test @-mention syntax
5. Research provider support (Bedrock, Vertex)

## Output Format
Return findings as structured JSON:
{
  "extension_features": [...],
  "parity_matrix": {...},
  "workflow_implications": [...],
  "provider_support": {...},
  "evidence": {...}
}

Store findings in memory at: swarm/cc-research/agent-08/findings`
  },
  {
    id: 9,
    name: 'cc-agent-09-composition',
    type: 'researcher',
    description: 'Test cross-compositions',
    prompt: `You are Agent 9 of the Claude Code Capability Research Swarm.

## Mission
Test cross-compositions from Agents 1-8. Discover emergent properties.

## Tasks
1. Retrieve findings from agents 1-8 (memory search)
2. Identify high-priority compositions to test
3. Design and execute composition tests
4. Measure emergent properties using novelty metrics
5. Document productive vs non-productive compositions

## Output Format
Return findings as structured JSON:
{
  "compositions_tested": [...],
  "emergent_properties": [...],
  "lattice_edges": [...],
  "frontier_updates": [...],
  "evidence": {...}
}

Store findings in memory at: swarm/cc-research/agent-09/compositions`
  },
  {
    id: 10,
    name: 'cc-agent-10-librarian',
    type: 'researcher',
    description: 'Synthesize final lattice',
    prompt: `You are Agent 10 of the Claude Code Capability Research Swarm.

## Mission
Synthesize all findings into final capability lattice and frontier report.

## Tasks
1. Collect all agent findings (memory search)
2. Build complete capability lattice
3. Generate Diataxis documentation index
4. Rank unexplored frontier by expected value
5. Produce executive summary

## Output Format
Return findings as structured JSON:
{
  "final_lattice": {...},
  "diataxis_index": {...},
  "frontier": [...],
  "executive_summary": "...",
  "evidence": {...}
}

Store findings in memory at: swarm/cc-research/final/capability-lattice`
  }
];

/**
 * Generate Task tool invocations for spawning agents
 */
function generateTaskInvocations(agentIds) {
  const selectedAgents = agentIds
    ? AGENTS.filter(a => agentIds.includes(a.id))
    : AGENTS;

  console.log('// Claude Code Capability Research Swarm');
  console.log('// Spawn the following agents in a SINGLE message with multiple Task calls:\n');

  selectedAgents.forEach(agent => {
    console.log(`Task("${agent.description}",`);
    console.log(`     \`${agent.prompt.replace(/`/g, '\\`')}\`,`);
    console.log(`     "${agent.type}")\n`);
  });

  console.log('// Expected: All agents run in parallel and return findings');
  console.log(`// Total agents: ${selectedAgents.length}`);
}

/**
 * Parse command line arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);
  const mode = args[0] || 'full';

  if (mode === 'full') {
    return null; // All agents
  } else if (mode === 'quick') {
    return [1, 2, 3]; // Core agents only
  } else {
    const num = parseInt(mode, 10);
    if (num >= 1 && num <= 10) {
      return [num]; // Single agent
    }
    console.error(`Invalid mode: ${mode}`);
    console.error('Usage: node spawn-swarm.mjs [full|quick|1-10]');
    process.exit(1);
  }
}

// Main execution
const agentIds = parseArgs();
generateTaskInvocations(agentIds);

console.log('\n// To execute: Copy the Task calls above into Claude Code');
console.log('// Or use: /research/claude-code-capabilities [mode]');
