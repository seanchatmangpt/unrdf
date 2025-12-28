# Claude Code Settings.json - Practical Guide

> **Complete reference for configuring Claude Code across user, project, and local scopes**
>
> **Last Updated**: December 2025
> **Claude Code Version**: 1.0.18+

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Configuration Hierarchy](#configuration-hierarchy)
3. [Core Configuration](#core-configuration)
4. [Permissions System](#permissions-system)
5. [Environment Variables](#environment-variables)
6. [Model Configuration](#model-configuration)
7. [Sandbox Settings](#sandbox-settings)
8. [Hooks & Automation](#hooks--automation)
9. [Security & Compliance](#security--compliance)
10. [Real-World Examples](#real-world-examples)
11. [Troubleshooting](#troubleshooting)

---

## Quick Start

### File Locations

```bash
# User settings (applies to all projects)
~/.claude/settings.json

# Project settings (shared with team, checked into git)
.claude/settings.json

# Local project settings (personal, not checked in)
.claude/settings.local.json
```

### Minimal Configuration

```json
{
  "model": "sonnet",
  "permissions": {
    "allow": ["Read", "Edit", "Bash(npm run *)"]
  },
  "env": {
    "ANTHROPIC_API_KEY": "sk-ant-..."
  }
}
```

### Interactive Configuration

```bash
# Open settings UI
/config

# Manage allowed tools
/allowed-tools add Edit
/allowed-tools remove WebFetch
/allowed-tools list

# Change model
/model opus
/model sonnet[1m]
```

---

## Configuration Hierarchy

### Precedence Order (Highest to Lowest)

1. **Managed Settings** (Enterprise only - cannot be overridden)
2. **CLI Arguments** (`claude --model opus --allowedTools Edit`)
3. **Local Project Settings** (`.claude/settings.local.json`)
4. **Shared Project Settings** (`.claude/settings.json`)
5. **User Settings** (`~/.claude/settings.json`)

### Example: Permission Precedence

```json
// User settings (~/.claude/settings.json)
{
  "permissions": {
    "allow": ["Bash(git push:*)"]
  }
}

// Project settings (.claude/settings.json)
{
  "permissions": {
    "deny": ["Bash(git push:*)"]
  }
}

// Result: git push is DENIED (project settings take precedence)
```

---

## Core Configuration

### Model Selection

```json
{
  "model": "sonnet",

  // Alternative options:
  // "default" - Recommended based on account type
  // "sonnet" - Claude Sonnet 4.5 (daily coding)
  // "opus" - Claude Opus 4.5 (complex reasoning)
  // "haiku" - Fast, efficient model
  // "sonnet[1m]" - 1 million token context
  // "opusplan" - Opus for planning, Sonnet for execution
}
```

### Output Style

```json
{
  "outputStyle": "Explanatory",

  // Options:
  // "Explanatory" - Detailed with reasoning
  // "Concise" - Brief, to-the-point
  // "Verbose" - Maximum detail
}
```

### Session Cleanup

```json
{
  "cleanupPeriodDays": 30
  // Sessions older than 30 days will be removed
}
```

---

## Permissions System

### Permission Types

| Tool | Description | Example |
|------|-------------|---------|
| `Bash` | Shell commands | `Bash(npm run test:*)` |
| `Read` | File reading | `Read(/home/user/project/**/*.md)` |
| `Write` | File creation | `Write(/home/user/project/dist/**)` |
| `Edit` | File editing | `Edit(/home/user/project/src/**/*.js)` |
| `MultiEdit` | Multi-file edits | `MultiEdit` |
| `Glob` | File pattern matching | `Glob` |
| `Grep` | Content search | `Grep` |
| `WebFetch` | Web requests | `WebFetch(https://api.example.com/*)` |
| `WebSearch` | Web search | `WebSearch` |
| `mcp__*` | MCP server tools | `mcp__github__create_issue` |

### Permission Patterns

```json
{
  "permissions": {
    // ===== ALLOW RULES =====
    "allow": [
      // Exact command
      "Bash(npm test)",

      // Wildcard commands
      "Bash(npm run test:*)",
      "Bash(git log:*)",
      "Bash(git status:*)",

      // Path glob patterns
      "Read(/home/user/project/**/*.md)",
      "Read(~/.zshrc)",
      "Edit(/home/user/project/src/**/*.js)",
      "Write(/home/user/project/dist/**/*)",

      // Web fetch patterns
      "WebFetch(https://api.github.com/*)",
      "WebFetch(https://*.example.com/*)",

      // All instances of a tool
      "Glob",
      "Grep"
    ],

    // ===== DENY RULES =====
    "deny": [
      // Dangerous commands
      "Bash(rm -rf:*)",
      "Bash(curl:*)",
      "Bash(wget:*)",

      // Sensitive files
      "Read(**/.env*)",
      "Read(**/secrets.*)",
      "Read(**/credentials.*)",
      "Read(**/.aws/*)",
      "Read(**/.ssh/*)",

      // System files
      "Write(/etc/**/*)",
      "Write(/usr/**/*)",
      "Edit(/etc/**/*)",

      // External web access
      "WebFetch(http://*)",  // Force HTTPS only
      "WebFetch(https://malicious.com/*)"
    ],

    // ===== ASK RULES (Prompt for permission) =====
    "ask": [
      "Bash(git push:*)",
      "Bash(npm publish:*)",
      "Bash(docker run:*)",
      "Write(/home/user/.bashrc)",
      "Edit(/home/user/.zshrc)"
    ],

    // Additional directories beyond project root
    "additionalDirectories": [
      "../docs/",
      "/home/user/shared-libs/"
    ],

    // Default behavior for edits
    "defaultMode": "acceptEdits",  // "acceptEdits" | "reviewEdits" | "ask"

    // Prevent --dangerously-skip-permissions flag
    "disableBypassPermissionsMode": "disable"
  }
}
```

### Common Permission Configurations

#### Development Environment

```json
{
  "permissions": {
    "allow": [
      "Read",
      "Edit",
      "Write",
      "Glob",
      "Grep",
      "Bash(npm run *)",
      "Bash(pnpm *)",
      "Bash(git log:*)",
      "Bash(git status:*)",
      "Bash(git diff:*)",
      "WebFetch(https://api.github.com/*)"
    ],
    "deny": [
      "Bash(git push:*)",
      "Bash(npm publish:*)",
      "Read(**/.env*)"
    ]
  }
}
```

#### Production Environment (Read-Only)

```json
{
  "permissions": {
    "allow": [
      "Read",
      "Bash(git log:*)",
      "Bash(git show:*)",
      "Bash(ls:*)",
      "Grep",
      "Glob"
    ],
    "deny": [
      "Write(**/*)",
      "Edit(**/*)",
      "Bash(git push:*)",
      "Bash(git commit:*)"
    ]
  }
}
```

#### Security Review Mode

```json
{
  "permissions": {
    "allow": [
      "Read",
      "Grep",
      "Glob"
    ],
    "deny": [
      "Write(**/*)",
      "Edit(**/*)",
      "Bash(*)"
    ]
  }
}
```

---

## Environment Variables

### API & Authentication

```json
{
  "env": {
    // Anthropic API
    "ANTHROPIC_API_KEY": "sk-ant-...",
    "ANTHROPIC_AUTH_TOKEN": "custom-token",
    "ANTHROPIC_MODEL": "claude-sonnet-4-5-20250929",

    // Model aliases
    "ANTHROPIC_DEFAULT_OPUS_MODEL": "claude-opus-4-5-20251101",
    "ANTHROPIC_DEFAULT_SONNET_MODEL": "claude-sonnet-4-5-20250929",
    "ANTHROPIC_DEFAULT_HAIKU_MODEL": "claude-haiku-4-5-20250929",

    // Subagent model
    "CLAUDE_CODE_SUBAGENT_MODEL": "sonnet"
  }
}
```

### Cloud Provider Integration

```json
{
  "env": {
    // AWS Bedrock
    "CLAUDE_CODE_USE_BEDROCK": "true",
    "AWS_REGION": "us-east-1",
    "AWS_PROFILE": "default",

    // Google Vertex AI
    "CLAUDE_CODE_USE_VERTEX": "true",
    "VERTEX_PROJECT_ID": "my-project",
    "VERTEX_REGION": "us-central1"
  }
}
```

### Performance & Timeouts

```json
{
  "env": {
    // Bash command timeouts
    "BASH_DEFAULT_TIMEOUT_MS": "120000",  // 2 minutes (default)
    "BASH_MAX_TIMEOUT_MS": "600000",      // 10 minutes (maximum)

    // MCP timeouts
    "MCP_TIMEOUT": "30000",               // 30 seconds
    "MCP_SERVER_TIMEOUT_MS": "30000",
    "MCP_TOOL_TIMEOUT_MS": "60000",

    // API key helper
    "CLAUDE_CODE_API_KEY_HELPER_TTL_MS": "3600000"  // 1 hour
  }
}
```

### Bash Environment Persistence

```json
{
  "env": {
    // Source this file before each bash command
    "CLAUDE_ENV_FILE": "/home/user/.claude-env.sh",

    // Reset to project dir after each command
    "CLAUDE_BASH_MAINTAIN_PROJECT_WORKING_DIR": "1"
  }
}
```

**Example `.claude-env.sh`:**

```bash
#!/bin/bash
# Environment setup for Claude Code sessions

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

export JAVA_HOME="/usr/lib/jvm/java-17"
export PATH="$JAVA_HOME/bin:$PATH"

# Project-specific
export NODE_ENV="development"
export DEBUG="app:*"
```

### Network & Proxy

```json
{
  "env": {
    "HTTP_PROXY": "http://proxy.example.com:8080",
    "HTTPS_PROXY": "https://proxy.example.com:8443",
    "NO_PROXY": "localhost,127.0.0.1,.example.com"
  }
}
```

### Privacy & Telemetry

```json
{
  "env": {
    // Disable all non-essential traffic
    "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC": "true",

    // Or disable individually:
    "DISABLE_TELEMETRY": "true",
    "DISABLE_ERROR_REPORTING": "true",
    "DISABLE_AUTOUPDATER": "true",
    "DISABLE_BUG_COMMAND": "true"
  }
}
```

### Debugging & Logging

```json
{
  "env": {
    "ANTHROPIC_LOG": "debug",  // "debug" | "info" | "warn" | "error"
    "DEBUG": "*",              // Enable all debug namespaces
    "VERBOSE": "1"
  }
}
```

### Prompt Caching

```json
{
  "env": {
    // Disable caching globally
    "DISABLE_PROMPT_CACHING": "true",

    // Or per model:
    "DISABLE_PROMPT_CACHING_HAIKU": "false",
    "DISABLE_PROMPT_CACHING_SONNET": "false",
    "DISABLE_PROMPT_CACHING_OPUS": "true"
  }
}
```

---

## Model Configuration

### Model Selection Priority

1. `/model` command (session-specific)
2. `--model` CLI flag
3. `ANTHROPIC_MODEL` environment variable
4. `"model"` in settings.json
5. Default based on account type

### Available Models

```json
{
  "model": "sonnet",

  "_options": {
    "aliases": [
      "default",     // Recommended (adapts to account)
      "sonnet",      // Claude Sonnet 4.5 - daily coding
      "opus",        // Claude Opus 4.5 - complex reasoning
      "haiku",       // Fast, efficient
      "sonnet[1m]",  // Extended 1M token context
      "opusplan"     // Opus planning + Sonnet execution
    ],
    "full_names": [
      "claude-sonnet-4-5-20250929",
      "claude-opus-4-5-20251101",
      "claude-haiku-4-5-20250929"
    ]
  }
}
```

### Opusplan Behavior

```json
{
  "model": "opusplan",

  "_behavior": {
    "planning_mode": "Uses Opus for complex reasoning and architecture",
    "execution_mode": "Switches to Sonnet for code generation",
    "automatic": true
  }
}
```

### Extended Context (Console/API Users)

```bash
# CLI
claude --model "anthropic.claude-sonnet-4-5-20250929-v1:0[1m]"

# Session
/model anthropic.claude-sonnet-4-5-20250929-v1:0[1m]
```

---

## Sandbox Settings

### Basic Sandbox Configuration

```json
{
  "sandbox": {
    "enabled": true,
    "autoAllowBashIfSandboxed": true,

    "excludedCommands": [
      "docker",
      "kubectl",
      "terraform",
      "aws",
      "gcloud"
    ],

    "network": {
      "allowUnixSockets": [
        "~/.ssh/agent-socket",
        "/var/run/docker.sock"
      ],
      "allowLocalBinding": true
    }
  }
}
```

### Use Cases

#### Docker Development

```json
{
  "sandbox": {
    "enabled": true,
    "excludedCommands": ["docker", "docker-compose"],
    "network": {
      "allowUnixSockets": ["/var/run/docker.sock"],
      "allowLocalBinding": true
    }
  }
}
```

#### SSH Agent Access

```json
{
  "sandbox": {
    "enabled": true,
    "network": {
      "allowUnixSockets": ["~/.ssh/agent-socket"]
    }
  }
}
```

---

## Hooks & Automation

### Hook Structure

```json
{
  "hooks": {
    "pre": {
      "Bash": "echo 'Running bash command...'",
      "Edit": "/path/to/pre-edit-hook.sh",
      "*": "logger 'Claude Code tool: pre-execution'"
    },
    "post": {
      "Bash": "echo 'Bash completed'",
      "Write": "/path/to/post-write-hook.sh",
      "*": "logger 'Claude Code tool: post-execution'"
    }
  }
}
```

### Real-World Examples

#### Auto-formatting on Edit

```json
{
  "hooks": {
    "post": {
      "Edit": "prettier --write $FILE",
      "Write": "prettier --write $FILE"
    }
  }
}
```

#### Git Auto-commit

```json
{
  "hooks": {
    "post": {
      "Edit": "git add $FILE && git commit -m 'Auto-commit: edited $FILE'",
      "Write": "git add $FILE && git commit -m 'Auto-commit: created $FILE'"
    }
  }
}
```

#### Security Scanning

```json
{
  "hooks": {
    "pre": {
      "Write": "/usr/local/bin/security-scan.sh $FILE"
    },
    "post": {
      "Bash": "/usr/local/bin/audit-command.sh \"$COMMAND\""
    }
  }
}
```

#### Notification System

```json
{
  "hooks": {
    "post": {
      "*": "notify-send 'Claude Code' 'Tool executed: $TOOL'"
    }
  }
}
```

---

## Security & Compliance

### Secret Detection

```json
{
  "security": {
    "secretPatterns": [
      "sk-ant-[a-zA-Z0-9-_]+",           // Anthropic API keys
      "AKIA[0-9A-Z]{16}",                 // AWS access keys
      "ghp_[a-zA-Z0-9]{36}",              // GitHub tokens
      "xoxb-[0-9]{10,13}-[a-zA-Z0-9-]+", // Slack tokens
      "ya29\\.[a-zA-Z0-9_-]+",            // Google OAuth
      "-----BEGIN (RSA |EC )?PRIVATE KEY-----" // Private keys
    ]
  }
}
```

### Network Access Control

```json
{
  "security": {
    "allowedNetworks": [
      "10.0.0.0/8",      // Internal network
      "192.168.0.0/16",  // Private network
      "172.16.0.0/12"    // Docker network
    ],
    "blockedNetworks": [
      "0.0.0.0/0"        // Block all by default
    ]
  }
}
```

### Sensitive File Protection

```json
{
  "permissions": {
    "deny": [
      // Environment files
      "Read(**/.env*)",
      "Write(**/.env*)",

      // Credentials
      "Read(**/credentials.*)",
      "Read(**/.aws/*)",
      "Read(**/.ssh/*)",
      "Read(**/.gnupg/*)",

      // Secrets
      "Read(**/secrets.*)",
      "Read(**/secret-*)",

      // Config files
      "Edit(/home/user/.bashrc)",
      "Edit(/home/user/.zshrc)",
      "Edit(/etc/**/*)"
    ]
  }
}
```

---

## Real-World Examples

### Example 1: TypeScript Monorepo

**.claude/settings.json** (shared with team):

```json
{
  "model": "sonnet",
  "outputStyle": "Concise",

  "permissions": {
    "allow": [
      "Read",
      "Edit",
      "Write",
      "Glob",
      "Grep",
      "Bash(pnpm *)",
      "Bash(npm run *)",
      "Bash(git log:*)",
      "Bash(git status:*)",
      "Bash(git diff:*)"
    ],
    "deny": [
      "Read(**/.env*)",
      "Bash(git push:*)",
      "Bash(npm publish:*)"
    ],
    "ask": [
      "Bash(pnpm install:*)"
    ]
  },

  "hooks": {
    "post": {
      "Edit": "prettier --write $FILE",
      "Write": "prettier --write $FILE"
    }
  },

  "attribution": {
    "commit": "Co-Authored-By: Claude <[email protected]>"
  }
}
```

**.claude/settings.local.json** (personal, not in git):

```json
{
  "env": {
    "ANTHROPIC_API_KEY": "sk-ant-your-personal-key",
    "NPM_TOKEN": "your-npm-token",
    "GITHUB_TOKEN": "your-github-token"
  }
}
```

### Example 2: Python Data Science

**.claude/settings.json**:

```json
{
  "model": "opus",
  "outputStyle": "Explanatory",

  "permissions": {
    "allow": [
      "Read",
      "Edit(**/*.py)",
      "Edit(**/*.ipynb)",
      "Write(**/*.py)",
      "Write(**/*.ipynb)",
      "Bash(python *)",
      "Bash(pip install *)",
      "Bash(jupyter *)",
      "WebFetch(https://api.*.com/*)"
    ],
    "additionalDirectories": [
      "../data/",
      "../models/"
    ]
  },

  "env": {
    "CLAUDE_ENV_FILE": "/home/user/.conda-env.sh",
    "PYTHONPATH": "/home/user/project/src",
    "JUPYTER_CONFIG_DIR": "/home/user/.jupyter"
  }
}
```

### Example 3: DevOps/Infrastructure

**.claude/settings.json**:

```json
{
  "model": "sonnet",

  "permissions": {
    "allow": [
      "Read",
      "Edit(**/*.yml)",
      "Edit(**/*.yaml)",
      "Edit(**/*.tf)",
      "Bash(terraform plan:*)",
      "Bash(terraform validate:*)",
      "Bash(kubectl get:*)",
      "Bash(kubectl describe:*)",
      "Bash(docker ps:*)",
      "Bash(docker logs:*)"
    ],
    "deny": [
      "Bash(terraform apply:*)",
      "Bash(terraform destroy:*)",
      "Bash(kubectl delete:*)",
      "Bash(docker rm:*)"
    ],
    "ask": [
      "Bash(docker build:*)",
      "Bash(kubectl apply:*)"
    ]
  },

  "sandbox": {
    "enabled": true,
    "excludedCommands": ["docker", "kubectl", "terraform"],
    "network": {
      "allowUnixSockets": ["/var/run/docker.sock"]
    }
  },

  "env": {
    "AWS_PROFILE": "development",
    "KUBECONFIG": "/home/user/.kube/config"
  }
}
```

### Example 4: Enterprise Locked-Down

**~/.claude/settings.json** (managed, cannot be overridden):

```json
{
  "model": "sonnet",
  "disableBypassPermissionsMode": "disable",

  "permissions": {
    "allow": [
      "Read",
      "Grep",
      "Glob",
      "Bash(git log:*)",
      "Bash(git status:*)"
    ],
    "deny": [
      "Write(**/*)",
      "Edit(**/*)",
      "WebFetch(**/*)",
      "Bash(curl:*)",
      "Bash(wget:*)"
    ]
  },

  "strictKnownMarketplaces": [
    "internal-tools"
  ],

  "security": {
    "allowedNetworks": ["10.0.0.0/8"],
    "secretPatterns": [
      "sk-ant-[a-zA-Z0-9-_]+",
      "AKIA[0-9A-Z]{16}",
      "-----BEGIN.*PRIVATE KEY-----"
    ]
  },

  "env": {
    "DISABLE_TELEMETRY": "false",
    "DISABLE_ERROR_REPORTING": "false",
    "HTTP_PROXY": "http://proxy.corp.example.com:8080",
    "HTTPS_PROXY": "http://proxy.corp.example.com:8080"
  },

  "companyAnnouncements": [
    "This Claude Code instance is managed by IT",
    "All activity is logged and monitored",
    "Contact [email protected] for support"
  ]
}
```

---

## Troubleshooting

### Common Issues

#### 1. Permissions Not Working

**Problem**: Commands still prompt despite being in `allow` list

**Check**:
```bash
# View current permissions
/allowed-tools list

# Check all settings files
cat ~/.claude/settings.json
cat .claude/settings.json
cat .claude/settings.local.json
```

**Solution**:
- Verify precedence (local > project > user)
- Ensure correct glob syntax
- Check for deny rules overriding allow

#### 2. API Key Not Recognized

**Problem**: Claude Code asking for API key despite being set

**Check**:
```bash
# Verify environment variable
echo $ANTHROPIC_API_KEY

# Check settings file
cat ~/.claude/settings.json | grep ANTHROPIC_API_KEY
```

**Solution**:
```json
{
  "env": {
    "ANTHROPIC_API_KEY": "sk-ant-..."
  }
}
```

Or set in shell:
```bash
# Add to ~/.zshrc or ~/.bashrc
export ANTHROPIC_API_KEY="sk-ant-..."
```

#### 3. Model Not Changing

**Problem**: Model stays the same despite configuration

**Priority order**:
1. `/model` command (highest)
2. `--model` CLI flag
3. `ANTHROPIC_MODEL` env var
4. `"model"` in settings.json
5. Default (lowest)

**Solution**:
```bash
# Check current model
/model

# Force change
/model opus
```

#### 4. Timeout Issues

**Problem**: Commands timing out too quickly

**Solution**:
```json
{
  "env": {
    "BASH_DEFAULT_TIMEOUT_MS": "300000",  // 5 minutes
    "BASH_MAX_TIMEOUT_MS": "600000"       // 10 minutes
  }
}
```

Or per-command:
```bash
timeout 300s npm run build
```

#### 5. Hooks Not Executing

**Problem**: Pre/post hooks not running

**Check**:
```json
{
  "hooks": {
    "post": {
      "Edit": "/absolute/path/to/hook.sh"  // Use absolute paths
    }
  }
}
```

**Debug**:
```bash
# Test hook manually
/absolute/path/to/hook.sh

# Check permissions
ls -l /absolute/path/to/hook.sh
chmod +x /absolute/path/to/hook.sh
```

### Validation Commands

```bash
# View all current settings
/config

# Check permissions
/allowed-tools list

# Verify model
/model

# Test environment variable
/bash echo $ANTHROPIC_API_KEY

# List additional directories
/add-dir
```

---

## Additional Resources

### Official Documentation
- [Claude Code Settings](https://code.claude.com/docs/en/settings)
- [Model Configuration](https://code.claude.com/docs/en/model-config)
- [Permissions Guide](https://platform.claude.com/docs/en/agent-sdk/permissions)
- [Environment Variables](https://code.claude.com/docs/en/environment-variables)

### Community Resources
- [Claude Code Settings Template](https://github.com/feiskyer/claude-code-settings)
- [Claude Code Setup Guide](https://github.com/centminmod/my-claude-code-setup)
- [Battle-Tested Configurations](https://github.com/fcakyon/claude-codex-settings)

### Articles & Guides
- [Settings.json Guide (eesel AI)](https://www.eesel.ai/blog/settings-json-claude-code)
- [Configuration Guide (ClaudeLog)](https://claudelog.com/configuration/)
- [Environment Variables Guide](https://www.eesel.ai/blog/claude-code-environment-variables)

---

## Quick Reference Card

```json
{
  // Model
  "model": "sonnet|opus|haiku|opusplan",

  // Permissions
  "permissions": {
    "allow": ["Tool(pattern)"],
    "deny": ["Tool(pattern)"],
    "ask": ["Tool(pattern)"]
  },

  // Environment
  "env": {
    "ANTHROPIC_API_KEY": "sk-ant-...",
    "BASH_DEFAULT_TIMEOUT_MS": "120000"
  },

  // Hooks
  "hooks": {
    "pre": {"Tool": "command"},
    "post": {"Tool": "command"}
  },

  // Sandbox
  "sandbox": {
    "enabled": true,
    "excludedCommands": ["docker"]
  }
}
```

---

**Last Updated**: December 2025
**Maintained By**: UNRDF Documentation Team
**Feedback**: [email protected]
