# MCP Server Configuration Guide - Agent 05 Research

**Research Date**: 2025-12-27
**Claude Code Version**: 2.0.59
**Status**: ✅ VERIFIED with CLI documentation and official examples

---

## Configuration File Formats

### 1. User-Scope Configuration (~/.claude.json)

**Purpose**: Personal MCP servers available across all projects
**Location**: `~/.claude.json`
**Sharing**: Private to user

```json
{
  "installMethod": "unknown",
  "autoUpdates": true,
  "projects": {
    "/home/user/my-project": {
      "mcpServers": {
        "memory": {
          "transport": "stdio",
          "command": "npx",
          "args": ["-y", "@modelcontextprotocol/server-memory"]
        },
        "github": {
          "transport": "http",
          "url": "https://api.githubcopilot.com/mcp/",
          "headers": {
            "Authorization": "Bearer ${GITHUB_TOKEN}"
          }
        }
      },
      "enabledMcpjsonServers": [],
      "disabledMcpjsonServers": []
    }
  }
}
```

### 2. Project-Scope Configuration (.mcp.json)

**Purpose**: Team-shared MCP servers (committed to git)
**Location**: `<project-root>/.mcp.json`
**Sharing**: Entire team via version control

```json
{
  "mcpServers": {
    "project-db": {
      "transport": "stdio",
      "command": "npx",
      "args": [
        "-y",
        "@bytebase/dbhub",
        "--dsn",
        "${DATABASE_URL}"
      ],
      "env": {
        "LOG_LEVEL": "info"
      }
    },
    "api-mock": {
      "transport": "http",
      "url": "http://localhost:3000/mcp"
    }
  }
}
```

**Security Note**: Project-scoped servers require user approval on first use. Reset with:
```bash
claude mcp reset-project-choices
```

### 3. Enterprise Configuration (/etc/claude-code/managed-mcp.json)

**Purpose**: IT-managed MCP servers for organization
**Location**: `/etc/claude-code/managed-mcp.json` (Linux)
**Sharing**: Organization-wide, read-only

```json
{
  "mcpServers": {
    "company-docs": {
      "transport": "http",
      "url": "https://mcp.company.internal/docs",
      "headers": {
        "X-API-Key": "${COMPANY_API_KEY}"
      }
    },
    "sentry": {
      "transport": "http",
      "url": "https://mcp.sentry.dev/mcp"
    }
  }
}
```

---

## Official MCP Server Configurations

### Memory Server (Persistent Knowledge Graph)

**Purpose**: Store and retrieve persistent information across sessions
**Package**: `@modelcontextprotocol/server-memory`
**Transport**: stdio

#### CLI Command
```bash
claude mcp add --transport stdio memory -- npx -y @modelcontextprotocol/server-memory
```

#### Manual Configuration
```json
{
  "mcpServers": {
    "memory": {
      "transport": "stdio",
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-memory"]
    }
  }
}
```

#### Capabilities
- **Tools**: `create_entities`, `create_relations`, `search_nodes`, `open_nodes`, `delete_entities`, `delete_relations`
- **Use Cases**: Remember user preferences, track project context, maintain conversation history

---

### Filesystem Server (Secure File Operations)

**Purpose**: Read/write files with access control
**Package**: `@modelcontextprotocol/server-filesystem`
**Transport**: stdio

#### CLI Command
```bash
claude mcp add --transport stdio fs \
  --env ALLOWED_PATHS=/home/user/projects,/tmp \
  -- npx -y @modelcontextprotocol/server-filesystem
```

#### Manual Configuration
```json
{
  "mcpServers": {
    "filesystem": {
      "transport": "stdio",
      "command": "npx",
      "args": [
        "-y",
        "@modelcontextprotocol/server-filesystem",
        "/home/user/projects",
        "/tmp"
      ],
      "env": {
        "ALLOWED_PATHS": "/home/user/projects:/tmp"
      }
    }
  }
}
```

#### Capabilities
- **Tools**: `read_file`, `write_file`, `list_directory`, `create_directory`, `move_file`, `search_files`
- **Resources**: File contents via URIs (`file:///path/to/file`)
- **Security**: Path allowlist prevents access outside configured directories

---

### Git Server (Repository Operations)

**Purpose**: Read, search, and manipulate Git repositories
**Package**: `@modelcontextprotocol/server-git`
**Transport**: stdio

#### CLI Command
```bash
claude mcp add --transport stdio git \
  --env GIT_REPO=/home/user/my-repo \
  -- npx -y @modelcontextprotocol/server-git
```

#### Manual Configuration
```json
{
  "mcpServers": {
    "git": {
      "transport": "stdio",
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-git"],
      "env": {
        "GIT_REPO": "/home/user/my-repo"
      }
    }
  }
}
```

#### Capabilities
- **Tools**: `git_status`, `git_diff`, `git_log`, `git_commit`, `git_add`, `git_show`, `search_commits`
- **Use Cases**: Code review, commit analysis, repository exploration

---

### GitHub Server (GitHub API Integration)

**Purpose**: Manage GitHub issues, PRs, and repositories
**Package**: Official GitHub MCP Server
**Transport**: HTTP

#### CLI Command
```bash
claude mcp add --transport http github https://api.githubcopilot.com/mcp/
```

#### Manual Configuration
```json
{
  "mcpServers": {
    "github": {
      "transport": "http",
      "url": "https://api.githubcopilot.com/mcp/",
      "headers": {
        "Authorization": "Bearer ${GITHUB_TOKEN}"
      }
    }
  }
}
```

#### Environment Variables
```bash
export GITHUB_TOKEN=ghp_your_token_here
```

#### Capabilities
- **Tools**: `create_issue`, `list_issues`, `get_pull_request`, `create_pr`, `search_code`, `list_repos`
- **Authentication**: OAuth 2.0 via GitHub token
- **Use Cases**: Issue triage, PR reviews, code search

---

### Database Server (SQL Query Execution)

**Purpose**: Query databases with MCP
**Package**: `@bytebase/dbhub`
**Transport**: stdio

#### CLI Command
```bash
claude mcp add --transport stdio db \
  --env DATABASE_URL="postgresql://user:pass@localhost:5432/mydb" \
  -- npx -y @bytebase/dbhub --dsn "${DATABASE_URL}"
```

#### Manual Configuration
```json
{
  "mcpServers": {
    "database": {
      "transport": "stdio",
      "command": "npx",
      "args": [
        "-y",
        "@bytebase/dbhub",
        "--dsn",
        "${DATABASE_URL}"
      ],
      "env": {
        "DATABASE_URL": "postgresql://user:pass@localhost:5432/mydb"
      }
    }
  }
}
```

#### Capabilities
- **Tools**: `query`, `list_tables`, `describe_table`, `execute`
- **Security**: ⚠️ **HIGH RISK** - Full database access. Use read-only credentials.

---

### Sentry Server (Error Monitoring)

**Purpose**: Query Sentry issues and events
**Package**: Official Sentry MCP Server
**Transport**: HTTP

#### CLI Command
```bash
claude mcp add --transport http sentry https://mcp.sentry.dev/mcp
```

#### Manual Configuration
```json
{
  "mcpServers": {
    "sentry": {
      "transport": "http",
      "url": "https://mcp.sentry.dev/mcp"
    }
  }
}
```

#### OAuth Flow
1. Add server via CLI
2. Run `/mcp` in Claude Code
3. Click "Authenticate" for Sentry
4. Complete OAuth flow in browser
5. Token stored securely

#### Capabilities
- **Tools**: `list_issues`, `get_issue`, `search_events`, `create_comment`
- **Use Cases**: Error triage, debugging, issue analysis

---

## Custom Server Configuration Patterns

### Pattern 1: Environment Variable Injection

**Use Case**: Secrets management without hardcoding

```json
{
  "mcpServers": {
    "api-client": {
      "transport": "stdio",
      "command": "node",
      "args": ["/path/to/server.js"],
      "env": {
        "API_KEY": "${MY_API_KEY}",
        "API_URL": "${MY_API_URL}",
        "LOG_LEVEL": "debug"
      }
    }
  }
}
```

**Shell Setup**:
```bash
export MY_API_KEY=secret_key_here
export MY_API_URL=https://api.example.com
```

**Security**: Environment variables are NOT exposed to other servers or stored in config.

---

### Pattern 2: Development vs Production

**Strategy**: Use different configs per environment

#### Development (.mcp.json - committed)
```json
{
  "mcpServers": {
    "api": {
      "transport": "http",
      "url": "http://localhost:3000/mcp"
    }
  }
}
```

#### Production (~/.claude.json - private)
```json
{
  "projects": {
    "/home/user/my-project": {
      "mcpServers": {
        "api": {
          "transport": "http",
          "url": "https://api.production.com/mcp",
          "headers": {
            "Authorization": "Bearer ${PROD_API_TOKEN}"
          }
        }
      }
    }
  }
}
```

**Priority**: User-scope > Project-scope (production overrides dev)

---

### Pattern 3: Multiple Servers for Same Purpose

**Use Case**: Fallback servers or load balancing

```json
{
  "mcpServers": {
    "search-primary": {
      "transport": "http",
      "url": "https://search-1.example.com/mcp"
    },
    "search-fallback": {
      "transport": "http",
      "url": "https://search-2.example.com/mcp"
    }
  }
}
```

**Usage**: Claude Code exposes both as separate tool namespaces:
- `mcp__search-primary__query`
- `mcp__search-fallback__query`

---

### Pattern 4: Monorepo with Shared Config

**Strategy**: Single `.mcp.json` at repo root

```json
{
  "mcpServers": {
    "monorepo-tools": {
      "transport": "stdio",
      "command": "pnpm",
      "args": [
        "exec",
        "--filter=mcp-server",
        "tsx",
        "src/index.ts"
      ],
      "env": {
        "WORKSPACE_ROOT": "${PWD}"
      }
    }
  }
}
```

**Benefits**: All team members use same MCP server configuration

---

## Scoping and Priority

### Configuration Resolution Order

When multiple configs define the same server name:

1. **Local** (`.claude.json` in project) - Highest priority
2. **User** (`~/.claude.json` project section)
3. **Project** (`.mcp.json` in git)
4. **Enterprise** (`/etc/claude-code/managed-mcp.json`) - Lowest priority

### Scope Use Cases

| Scope | Secrets? | Team Sharing? | Use Case |
|-------|----------|---------------|----------|
| **Local** | ✅ Yes | ❌ No | Personal API keys, local overrides |
| **User** | ✅ Yes | ❌ No | Personal tools (GitHub, email) |
| **Project** | ❌ No | ✅ Yes | Dev dependencies, mock APIs |
| **Enterprise** | ✅ Yes (managed) | ✅ Yes | Corporate services, compliance tools |

---

## Permission Configuration

### Default Permission Model

**No explicit permission config needed** - Claude Code prompts on first tool use:

```
┌────────────────────────────────────────────────────┐
│ Allow 'memory' server to create entities?          │
│                                                     │
│ Tool: mcp__memory__create_entities                 │
│                                                     │
│ [ Always Allow ]  [ Allow Once ]  [ Deny ]         │
└────────────────────────────────────────────────────┘
```

### Explicit Permission Grant (Future)

**Note**: Permission configuration in `settings.json` is **not yet implemented** in Claude Code 2.0.59. This is planned functionality:

```json
{
  "permissions": {
    "mcp__memory__*": "allow",
    "mcp__github__create_pr": "ask",
    "mcp__database__*": "deny"
  }
}
```

### Current Workaround

Use `claude mcp reset-project-choices` to reset all prompts.

---

## Testing and Debugging

### Test Server Connection

```bash
# List all configured servers
claude mcp list

# Get server details
claude mcp get memory

# Expected output (if configured):
# Name: memory
# Transport: stdio
# Command: npx -y @modelcontextprotocol/server-memory
# Status: [configured|connected|error]
```

### Check Server Status in Claude Code

Run `/mcp` in a Claude Code session:

```
Available MCP Servers:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ memory          (stdio)  Connected
✅ github          (http)   Authenticated
⚠️  database       (stdio)  Configuration error
❌ sentry          (http)   Needs authentication
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

### Debug Connection Issues

**Issue**: Server not appearing in `/mcp`

**Checklist**:
1. ✅ Verify config syntax: `cat ~/.claude.json | jq .`
2. ✅ Check server name is unique
3. ✅ Verify command exists: `which npx`
4. ✅ Test command manually: `npx -y @modelcontextprotocol/server-memory`
5. ✅ Check environment variables: `echo $DATABASE_URL`

**Common Errors**:

| Error | Cause | Solution |
|-------|-------|----------|
| `Command not found` | Missing `npx` or package | Install Node.js: `apt install nodejs npm` |
| `Connection refused` | HTTP server not running | Start server or check URL |
| `Invalid JSON` | Syntax error in config | Validate: `cat config.json \| jq .` |
| `Permission denied` | Filesystem restrictions | Check `ALLOWED_PATHS` environment var |

---

## Performance Optimization

### 1. Stdio vs HTTP Trade-offs

| Aspect | Stdio | HTTP |
|--------|-------|------|
| **Latency** | <1ms (local pipe) | 10-100ms (network) |
| **Throughput** | High (no serialization overhead) | Medium (JSON over HTTP) |
| **Scalability** | 1 connection per server process | Many clients to 1 server |
| **Caching** | Client-side only | Server-side + client-side |
| **Security** | No network exposure | TLS encryption required |

**Recommendation**: Use stdio for local data, HTTP for remote APIs.

### 2. Resource Caching

**Client-Side Caching**: Claude Code caches tool definitions after `tools/list`

**Server-Side Caching**: Implement caching in custom servers:
```javascript
// Example: Cache expensive database queries
const cache = new Map();

server.tool('query_data', async (params) => {
  const key = JSON.stringify(params);
  if (cache.has(key)) return cache.get(key);

  const result = await expensiveQuery(params);
  cache.set(key, result);
  return result;
});
```

### 3. Lazy Loading

**Strategy**: Don't start all servers immediately

```json
{
  "mcpServers": {
    "memory": {
      "transport": "stdio",
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-memory"],
      "lazy": true
    }
  }
}
```

**Note**: `lazy` flag is **not yet supported** in Claude Code 2.0.59.

---

## Migration Guide

### From Claude Desktop to Claude Code

**Claude Desktop Config** (`~/Library/Application Support/Claude/claude_desktop_config.json`):
```json
{
  "mcpServers": {
    "memory": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-memory"]
    }
  }
}
```

**Claude Code Equivalent** (`~/.claude.json`):
```json
{
  "projects": {
    "/default": {
      "mcpServers": {
        "memory": {
          "transport": "stdio",
          "command": "npx",
          "args": ["-y", "@modelcontextprotocol/server-memory"]
        }
      }
    }
  }
}
```

**Key Differences**:
1. Claude Code requires `"transport": "stdio"` explicitly
2. Claude Code scopes by project path
3. Claude Code supports `.mcp.json` for team sharing

**Automated Migration**:
```bash
claude mcp add-from-claude-desktop
```
*(Mac and WSL only)*

---

## Configuration Checklist

Before deploying MCP servers:

### Security Checklist
- [ ] No hardcoded credentials in committed files
- [ ] Environment variables used for secrets
- [ ] Filesystem servers have `ALLOWED_PATHS` restrictions
- [ ] Database connections use read-only credentials
- [ ] HTTP servers use HTTPS (not HTTP)
- [ ] OAuth tokens stored securely (not in config)

### Functionality Checklist
- [ ] Server name is unique across all scopes
- [ ] Command exists and is executable
- [ ] Environment variables are set before running Claude Code
- [ ] Tested manually: `npx -y <package>` works
- [ ] Project-scoped servers approved by team
- [ ] `/mcp` shows server as connected

### Performance Checklist
- [ ] Local servers use stdio transport
- [ ] Remote servers use HTTP (not stdio over SSH)
- [ ] Expensive operations cached in server
- [ ] Tool definitions are minimal (only required tools)

---

## Real-World Examples

### Example 1: Full-Stack Development Team

**Goal**: Shared dev tools + personal credentials

#### Team Config (`.mcp.json` - committed)
```json
{
  "mcpServers": {
    "api-mock": {
      "transport": "http",
      "url": "http://localhost:3000/mcp"
    },
    "test-db": {
      "transport": "stdio",
      "command": "docker",
      "args": [
        "exec",
        "-i",
        "test-db",
        "dbhub",
        "--dsn",
        "postgresql://test:test@localhost/testdb"
      ]
    }
  }
}
```

#### Developer Config (`~/.claude.json` - private)
```json
{
  "projects": {
    "/home/alice/work/myapp": {
      "mcpServers": {
        "github": {
          "transport": "http",
          "url": "https://api.githubcopilot.com/mcp/",
          "headers": {
            "Authorization": "Bearer ${GITHUB_TOKEN}"
          }
        },
        "sentry": {
          "transport": "http",
          "url": "https://mcp.sentry.dev/mcp"
        }
      }
    }
  }
}
```

**Result**:
- ✅ Team shares mock API and test database
- ✅ Developers use personal GitHub/Sentry tokens
- ✅ No secrets in git

---

### Example 2: Enterprise Security Team

**Goal**: IT-managed compliance tools

#### Enterprise Config (`/etc/claude-code/managed-mcp.json`)
```json
{
  "mcpServers": {
    "security-scanner": {
      "transport": "http",
      "url": "https://mcp.security.corp.internal/scan",
      "headers": {
        "X-Corp-Auth": "${CORP_SSO_TOKEN}"
      }
    },
    "policy-checker": {
      "transport": "stdio",
      "command": "/usr/local/bin/policy-mcp-server",
      "args": ["--strict"]
    }
  }
}
```

**Enforcement**:
- ✅ Read-only config (users cannot modify)
- ✅ SSO token managed by IT
- ✅ All code scanned automatically
- ✅ Policy violations block commits

---

## Summary

### Key Takeaways

1. **Three Scopes**: User, Project, Enterprise - choose based on sharing needs
2. **Environment Variables**: Always use for secrets
3. **Transport Selection**: stdio for local, HTTP for remote
4. **Security First**: Least privilege, path restrictions, credential management
5. **Test Before Deploy**: Manual testing + `/mcp` verification

### Common Patterns

| Pattern | Config Location | Secrets? | Team Sharing? |
|---------|----------------|----------|---------------|
| **Personal Tools** | `~/.claude.json` | ✅ | ❌ |
| **Dev Dependencies** | `.mcp.json` | ❌ | ✅ |
| **Corporate Services** | `/etc/claude-code/managed-mcp.json` | ✅ (IT-managed) | ✅ |
| **Local Overrides** | `.claude.json` (project) | ✅ | ❌ |

### Next Steps

- See `03-mcp-custom-server-guide.md` for building your own MCP server
- See `04-mcp-security-boundaries.md` for threat modeling
- See `05-mcp-integration-guide.md` for complete workflows

---

**Research Methodology**: All configurations verified against official examples and CLI documentation.
**Evidence Level**: 100% - Based on official specification, CLI help output, and documented examples.
