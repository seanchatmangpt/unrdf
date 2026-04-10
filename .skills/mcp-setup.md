# Skill: MCP Server Setup

Start and verify the MCP daemon server for ontology operations.

## Quick-Start

```bash
# Start the MCP daemon server
pnpm --filter @unrdf/daemon start

# In another terminal, test MCP tools
node packages/cli/src/cli/src/cli/main.mjs mcp:list

# Sync MCP definitions (required before commit)
pnpm mcp:sync
```

## Available MCP Tools

15 tools for ontology operations: `onto_validate`, `onto_stats`, `onto_query`, `onto_load`, `onto_marketplace`, `onto_reason`, `onto_shacl`, `onto_save`, `onto_clear`, `onto_convert`, `onto_align`, `onto_drift`, `onto_plan`, `onto_apply`, `onto_version`.

## Key Files

- `packages/daemon/src/mcp/` — MCP protocol implementation
- `docs/MCP_INTEGRATION.md` — Complete MCP guide
- `packages/daemon/GROQ-INTEGRATION.md` — Groq LLM provider docs

## Troubleshooting

If MCP tools not found after setup:
```bash
pnpm --filter @unrdf/daemon build && pnpm mcp:sync
```
