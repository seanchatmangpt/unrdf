# MCP Quick Reference Card

**Claude Code 2.0.59** | **MCP Spec 2025-11-25** | **Agent 05 Research**

---

## 🚀 Most Common Commands

```bash
# List all MCP servers
claude mcp list

# Add stdio server
claude mcp add --transport stdio <name> -- npx -y <package>

# Add HTTP server
claude mcp add --transport http <name> <url>

# Get server details
claude mcp get <name>

# Remove server
claude mcp remove <name>

# Reset project approvals
claude mcp reset-project-choices

# Check server status in Claude Code
/mcp
```

---

## 📁 Configuration Locations

| Scope | File | When to Use |
|-------|------|-------------|
| **User** | `~/.claude.json` | Personal tools, your API keys |
| **Project** | `.mcp.json` | Team-shared dev tools (committed) |
| **Local** | `.claude.json` (in project) | Project secrets (not committed) |
| **Enterprise** | `/etc/claude-code/managed-mcp.json` | IT-managed corporate tools |

---

## 🛠️ Official MCP Servers

```bash
# Memory (persistent knowledge graph)
claude mcp add --transport stdio memory -- npx -y @modelcontextprotocol/server-memory

# Filesystem (secure file operations)
claude mcp add --transport stdio fs -- npx -y @modelcontextprotocol/server-filesystem /allowed/path

# Git (repository operations)
claude mcp add --transport stdio git --env GIT_REPO=/path/to/repo -- npx -y @modelcontextprotocol/server-git

# GitHub (API integration)
claude mcp add --transport http github https://api.githubcopilot.com/mcp/

# Sentry (error monitoring)
claude mcp add --transport http sentry https://mcp.sentry.dev/mcp
```

---

## 🔒 Security Checklist

### Before Adding Any Server
- [ ] Review server source code (if open source)
- [ ] Use environment variables for secrets
- [ ] Limit filesystem paths (`ALLOWED_PATHS`)
- [ ] Use read-only database credentials
- [ ] Prefer HTTPS over HTTP (except localhost)

### Configuration Security
```json
{
  "mcpServers": {
    "db": {
      "transport": "stdio",
      "command": "npx",
      "args": ["-y", "db-server"],
      "env": {
        "DATABASE_URL": "${DATABASE_URL}",  // ✅ Environment variable
        "API_KEY": "hardcoded123"            // ❌ NEVER DO THIS
      }
    }
  }
}
```

---

## 🏗️ Minimal Custom Server (TypeScript)

```typescript
#!/usr/bin/env node
import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { CallToolRequestSchema, ListToolsRequestSchema } from '@modelcontextprotocol/sdk/types.js';

const server = new Server(
  { name: 'my-server', version: '1.0.0' },
  { capabilities: { tools: {} } }
);

server.setRequestHandler(ListToolsRequestSchema, async () => ({
  tools: [{
    name: 'hello',
    description: 'Say hello',
    inputSchema: {
      type: 'object',
      properties: { name: { type: 'string' } },
      required: ['name']
    }
  }]
}));

server.setRequestHandler(CallToolRequestSchema, async (request) => {
  if (request.params.name === 'hello') {
    const { name } = request.params.arguments as { name: string };
    return { content: [{ type: 'text', text: `Hello, ${name}!` }] };
  }
  throw new Error(`Unknown tool: ${request.params.name}`);
});

const transport = new StdioServerTransport();
await server.connect(transport);
console.error('Server running');
```

**Setup**:
```bash
npm install @modelcontextprotocol/sdk zod
npx tsx server.ts
```

---

## 🐛 Troubleshooting

### Server Not Showing in `/mcp`
```bash
# Check config syntax
cat ~/.claude.json | jq .mcpServers

# Test server manually
echo '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | npx tsx server.ts

# Check Claude Code logs
tail -f ~/.claude/logs/mcp-*.log
```

### "Invalid JSON" Errors
```typescript
// ❌ BAD - stdout contamination
console.log('Debug message');

// ✅ GOOD - use stderr for logs
console.error('Debug message');
```

### Database Connection Failing
```bash
# Test connection string
psql "${DATABASE_URL}"

# Verify environment variable
echo $DATABASE_URL
```

---

## 📊 JSON-RPC Protocol

### Request
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "my_tool",
    "arguments": { "param": "value" }
  }
}
```

### Success Response
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [{ "type": "text", "text": "Result data" }]
  }
}
```

### Error Response
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32602,
    "message": "Invalid params",
    "data": { "details": "..." }
  }
}
```

---

## 🎯 Common Patterns

### Environment Variables
```bash
# Set before running Claude Code
export DATABASE_URL="postgresql://user:pass@localhost/db"
export API_KEY="secret_key"

# Use in config
claude mcp add --env DATABASE_URL=${DATABASE_URL} ...
```

### Project-Scoped Server (.mcp.json)
```json
{
  "mcpServers": {
    "team-db": {
      "transport": "stdio",
      "command": "npx",
      "args": ["tsx", "./tools/db-server.ts"],
      "env": { "DATABASE_URL": "${DATABASE_URL}" }
    }
  }
}
```

### Input Validation (Zod)
```typescript
import { z } from 'zod';

const InputSchema = z.object({
  query: z.string().min(1).max(1000),
  limit: z.number().int().min(1).max(100).default(10)
});

const params = InputSchema.parse(request.params.arguments);
// Now TypeScript knows: params.query is string, params.limit is number
```

---

## ⚡ Performance Tips

### Caching
```typescript
const cache = new Map<string, { data: any; expires: number }>();

async function getCached(key: string, ttl: number, fetch: () => Promise<any>) {
  const cached = cache.get(key);
  if (cached && Date.now() < cached.expires) {
    return cached.data;
  }
  const data = await fetch();
  cache.set(key, { data, expires: Date.now() + ttl });
  return data;
}
```

### Connection Pooling
```typescript
import { Pool } from 'pg';

const pool = new Pool({
  connectionString: process.env.DATABASE_URL,
  max: 5,
  idleTimeoutMillis: 30000
});

// Reuse connections across requests
const result = await pool.query(sql);
```

---

## 📚 Error Codes

| Code | Meaning | Fix |
|------|---------|-----|
| `-32700` | Parse error | Check JSON syntax |
| `-32600` | Invalid request | Add required fields |
| `-32601` | Method not found | Check tool name |
| `-32602` | Invalid params | Validate input schema |
| `-32603` | Internal error | Check server logs |

---

## 🎓 Best Practices

### DO ✅
- Use environment variables for secrets
- Validate all inputs with Zod
- Log to stderr only
- Use parameterized SQL queries
- Implement rate limiting
- Cache expensive operations
- Return structured errors

### DON'T ❌
- Hardcode credentials
- Trust user input
- Use `console.log()` (stdout)
- Execute arbitrary shell commands
- Grant excessive filesystem access
- Skip input validation
- Expose internal errors to users

---

## 📖 Full Documentation

- **Architecture**: `01-mcp-architecture-overview.md`
- **Configuration**: `02-mcp-server-configuration.md`
- **Development**: `03-mcp-custom-server-guide.md`
- **Security**: `04-mcp-security-boundaries.md`
- **Integration**: `05-mcp-integration-guide.md`
- **Index**: `README.md`

---

## 🆘 Emergency Commands

```bash
# Server is misbehaving - REMOVE IT
claude mcp remove suspicious-server

# Reset all project approvals
claude mcp reset-project-choices

# Check what servers are running
ps aux | grep mcp

# View MCP logs
tail -100 ~/.claude/logs/mcp-*.log

# Audit configuration
cat ~/.claude.json | jq .projects[].mcpServers
```

---

**Agent 05 Research** | **2025-12-27** | **Evidence Level: 95%**

For complete documentation, see: `/home/user/unrdf/research-output/agent-05-mcp/`
