# Scoped Rule: Daemon MCP Tool Registration

**Scope**: `@unrdf/daemon` - MCP tool definitions and registration

## Tool Registration Pattern

All MCP tools must be registered in `packages/daemon/src/mcp/index.mjs`:

```javascript
// Correct - tool registered
export const tools = {
  onto_validate: {
    name: 'onto_validate',
    description: 'Validate ontology against SHACL shapes',
    inputSchema: { /* ... */ }
  },
  // ... more tools
};

// Wrong - tool exists but not registered
// Tool defined in separate file but not added to export
```

## Tool Count

Daemon provides exactly **15 ontology tools**:

| Tool | Purpose |
|------|---------|
| `onto_validate` | SHACL validation |
| `onto_query` | SPARQL SELECT |
| `onto_reason` | RDFS/OWL reasoning |
| `onto_import` | Load RDF data |
| `onto_export` | Serialize RDF |
| `onto_shapes` | List SHACL shapes |
| `onto_classes` | List ontology classes |
| `onto_properties` | List properties |
| `onto_individuals` | List individuals |
| `onto_prefixes` | Manage prefixes |
| `onto_namespaces` | List namespaces |
| `onto_construct` | SPARQL CONSTRUCT |
| `onto_update` | SPARQL UPDATE |
| `onto_explain` | Query explanation |
| `onto_stats` | Store statistics |

## Tool Naming Convention

- `onto_*` for ontology operations
- Descriptive verb: `_validate`, `_query`, `_reason`, `_import`
- Snake_case (not camelCase)

## Input Schema Validation

Each tool defines JSON Schema for input validation:

```javascript
{
  inputSchema: {
    type: 'object',
    properties: {
      sparql: { type: 'string', description: 'SPARQL query' },
      format: { type: 'string', enum: ['json', 'xml', 'text'] }
    },
    required: ['sparql']
  }
}
```

## Tool Documentation

Every tool must have:
- `name` - Unique identifier
- `description` - Human-readable purpose
- `inputSchema` - JSON Schema validation

## MCP Sync

Pre-commit hook syncs MCP definitions:

```bash
pnpm mcp:sync
```

This updates `.claude/mcp.json` with current tool definitions.

## File Locations

```
packages/daemon/
├── src/mcp/
│   ├── index.mjs           # Tool registration
│   ├── tools/              # Tool implementations
│   └── schemas/            # Input/output schemas
└── GROQ-INTEGRATION.md     # Groq LLM integration
```

## Verification

After adding/modifying tools:

```bash
# Sync MCP definitions
pnpm mcp:sync

# Verify tool count
node packages/cli/src/cli/main.mjs mcp:list | wc -l
# Should show 15 tools
```
