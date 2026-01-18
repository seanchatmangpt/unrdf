# Claude Code VS Code Extension: Working Examples

**Research Date**: 2025-12-27
**Agent**: Agent 8 - IDE/VS Code Surface Explorer
**Purpose**: Practical, copy-paste examples for Claude Code VS Code extension integration

---

## Table of Contents

1. [Extension Configuration Examples](#1-extension-configuration-examples)
2. [MCP Server Setup Examples](#2-mcp-server-setup-examples)
3. [Custom Workflow Examples](#3-custom-workflow-examples)
4. [Integration Examples](#4-integration-examples)
5. [Troubleshooting Examples](#5-troubleshooting-examples)

---

## 1. Extension Configuration Examples

### Example 1.1: Basic VS Code Settings

**File**: `.vscode/settings.json` (workspace-specific)

```json
{
  "claude-code.selectedModel": "claude-opus-4-5-20251101",
  "claude-code.initialPermissionMode": "manual",
  "claude-code.preferredLocation": "sidebar",
  "claude-code.autosave": true,
  "claude-code.respectGitIgnore": true,
  "claude-code.useCtrlEnterToSend": false,
  "claude-code.environmentVariables": {
    "NODE_ENV": "development",
    "DEBUG": "app:*"
  }
}
```

**Use Case**: Standard development setup with manual review mode

---

### Example 1.2: Auto-Accept Mode (Testing/Sandbox)

**File**: `.vscode/settings.json`

```json
{
  "claude-code.selectedModel": "claude-sonnet-4-5-20250929",
  "claude-code.initialPermissionMode": "auto-accept",
  "claude-code.autosave": true,
  "claude-code.respectGitIgnore": true,
  "claude-code.allowDangerouslySkipPermissions": false
}
```

**WARNING**: Only use auto-accept in isolated test environments
**Use Case**: Rapid prototyping in throwaway projects

---

### Example 1.3: Highly Restricted Mode (Production Codebase)

**File**: `.vscode/settings.json`

```json
{
  "claude-code.selectedModel": "claude-opus-4-5-20251101",
  "claude-code.initialPermissionMode": "manual",
  "claude-code.respectGitIgnore": true,
  "claude-code.autosave": false,
  "claude-code.allowDangerouslySkipPermissions": false,
  "files.watcherExclude": {
    "**/.claude/**": true
  }
}
```

**Use Case**: Production code review with maximum safety

---

### Example 1.4: Terminal-Style Mode (CLI Users)

**File**: `.vscode/settings.json`

```json
{
  "claude-code.useTerminal": true,
  "claude-code.initialPermissionMode": "manual",
  "claude-code.respectGitIgnore": true
}
```

**Effect**: Uses CLI-style interface within VS Code
**Use Case**: Users who prefer terminal UX but want IDE integration

---

### Example 1.5: Multi-Root Workspace Configuration

**File**: `project.code-workspace`

```json
{
  "folders": [
    {
      "path": "frontend"
    },
    {
      "path": "backend"
    }
  ],
  "settings": {
    "claude-code.selectedModel": "claude-opus-4-5-20251101",
    "claude-code.initialPermissionMode": "manual",
    "claude-code.respectGitIgnore": true,
    "claude-code.environmentVariables": {
      "MONOREPO": "true"
    }
  }
}
```

**Use Case**: Monorepo with frontend and backend separation

---

## 2. MCP Server Setup Examples

### Example 2.1: GitHub MCP Server

**Setup** (CLI required):
```bash
# Add GitHub MCP server
claude mcp add github --scope user

# Configure token
export GITHUB_TOKEN="ghp_your_token_here"

# Verify
claude mcp list
```

**Configuration** (`~/.claude/settings.json`):
```json
{
  "mcpServers": {
    "github": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-github"],
      "env": {
        "GITHUB_TOKEN": "${GITHUB_TOKEN}"
      },
      "scope": "user"
    }
  }
}
```

**Extension Usage**:
```
User: "@github List all open pull requests in my repository"
Claude: [Uses GitHub MCP to fetch PRs]
```

**Use Case**: GitHub issue/PR management from Claude Code

---

### Example 2.2: Filesystem MCP Server

**Setup**:
```bash
claude mcp add filesystem --scope project
```

**Configuration** (`.claude/mcp-servers.json` in project root):
```json
{
  "mcpServers": {
    "filesystem": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-filesystem", "/home/user/project"],
      "scope": "project"
    }
  }
}
```

**Extension Usage**:
```
User: "Read the contents of the config directory"
Claude: [Uses filesystem MCP to access files]
```

**Use Case**: Restricted file system access with explicit boundaries

---

### Example 2.3: Database MCP Server (PostgreSQL)

**Setup**:
```bash
claude mcp add postgres --scope local
```

**Configuration** (`~/.claude/settings.json`):
```json
{
  "mcpServers": {
    "postgres": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-postgres"],
      "env": {
        "POSTGRES_CONNECTION_STRING": "postgresql://user:pass@localhost:5432/mydb"
      },
      "scope": "local"
    }
  }
}
```

**Extension Usage**:
```
User: "Show me the schema for the users table"
Claude: [Queries PostgreSQL via MCP]
```

**Use Case**: Database schema exploration and query generation

---

### Example 2.4: Multiple MCP Servers (Stack-Specific)

**Configuration** (`~/.claude/settings.json`):
```json
{
  "mcpServers": {
    "github": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-github"],
      "env": {
        "GITHUB_TOKEN": "${GITHUB_TOKEN}"
      },
      "scope": "user"
    },
    "postgres": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-postgres"],
      "env": {
        "POSTGRES_CONNECTION_STRING": "${DATABASE_URL}"
      },
      "scope": "local"
    },
    "slack": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-slack"],
      "env": {
        "SLACK_BOT_TOKEN": "${SLACK_BOT_TOKEN}",
        "SLACK_TEAM_ID": "${SLACK_TEAM_ID}"
      },
      "scope": "user"
    }
  }
}
```

**Use Case**: Full-stack development with GitHub, DB, and Slack integration

---

### Example 2.5: Custom MCP Server

**File**: `custom-mcp-server.mjs`

```javascript
#!/usr/bin/env node

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';

const server = new Server(
  {
    name: 'custom-api-server',
    version: '1.0.0',
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

server.setRequestHandler('tools/list', async () => {
  return {
    tools: [
      {
        name: 'fetch_api_data',
        description: 'Fetches data from custom API',
        inputSchema: {
          type: 'object',
          properties: {
            endpoint: {
              type: 'string',
              description: 'API endpoint to call',
            },
          },
          required: ['endpoint'],
        },
      },
    ],
  };
});

server.setRequestHandler('tools/call', async (request) => {
  if (request.params.name === 'fetch_api_data') {
    const { endpoint } = request.params.arguments;
    const response = await fetch(`https://api.example.com/${endpoint}`);
    const data = await response.json();
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify(data, null, 2),
        },
      ],
    };
  }
});

const transport = new StdioServerTransport();
await server.connect(transport);
```

**Configuration** (`~/.claude/settings.json`):
```json
{
  "mcpServers": {
    "custom-api": {
      "command": "node",
      "args": ["/path/to/custom-mcp-server.mjs"],
      "env": {
        "API_KEY": "${CUSTOM_API_KEY}"
      },
      "scope": "user"
    }
  }
}
```

**Extension Usage**:
```
User: "Fetch data from /users endpoint"
Claude: [Uses custom MCP server to call API]
```

**Use Case**: Integrating proprietary or custom APIs

---

## 3. Custom Workflow Examples

### Example 3.1: Plan-First Refactoring Workflow

**Workflow**:
```
1. User: "/plan Refactor the authentication module to use JWT"
2. Claude: Presents plan:
   - Analyze current session-based auth
   - Design JWT implementation
   - Implement token generation
   - Implement token verification
   - Update middleware
   - Migrate tests
   - Document changes
3. User: Reviews plan, clicks "Approve"
4. Claude: Executes plan step-by-step
5. User: Reviews diffs, accepts changes
```

**VS Code Settings** (optimal for this workflow):
```json
{
  "claude-code.initialPermissionMode": "manual",
  "claude-code.autosave": true,
  "claude-code.preferredLocation": "sidebar"
}
```

---

### Example 3.2: Multi-Tab Parallel Development

**Scenario**: Work on frontend and backend simultaneously

**Workflow**:
```
Tab 1 (Frontend):
User: "Implement the user profile page using React hooks"
Claude: [Working on frontend]

Tab 2 (Backend):
User: "Create API endpoint for fetching user profile data"
Claude: [Working on backend]

User switches between tabs to review progress
```

**Keybindings**:
- New tab: `Cmd+Shift+Esc` / `Ctrl+Shift+Esc`
- Switch tabs: Standard VS Code tab navigation

**Coordination**: Manual (both tabs can edit files, beware conflicts)

---

### Example 3.3: Iterative Code Review Workflow

**Workflow**:
```
1. User: "@src/app.mjs Add error handling for network failures"
2. Claude: Proposes changes (diff shown)
3. User: Clicks "Reject" on one hunk, "Accept" on another
4. User: "The error handling should also log to monitoring"
5. Claude: Updates approach, shows new diff
6. User: "Accept All"
```

**Required Setting**:
```json
{
  "claude-code.initialPermissionMode": "manual"
}
```

**Benefit**: Hunk-level review unique to extension

---

### Example 3.4: @-Mention Line Range Workflow

**Workflow**:
```
1. User opens src/utils/parser.mjs
2. User selects lines 45-68 (the parsing logic)
3. User presses Alt+K
4. Input populates: @src/utils/parser.mjs:45-68
5. User: @src/utils/parser.mjs:45-68 Optimize this for large files
6. Claude: Analyzes only those lines, suggests streaming approach
```

**Keybinding**: `Alt+K` (Insert @-Mention)

**Benefit**: Precise context without sending entire file

---

### Example 3.5: LSP-Driven Bug Fix Workflow

**Workflow**:
```
1. User: "Fix all TypeScript errors in this file"
2. Claude: Uses LSP getDiagnostics
3. Claude: Identifies 3 type errors
4. Claude: Proposes fixes using goToDefinition for context
5. User: Reviews diffs
6. Claude: Re-runs diagnostics, confirms 0 errors
7. User: Accepts changes
```

**Requirements**:
- LSP plugin installed (e.g., vtsls for TypeScript)
- Extension v2.0.74+

**Benefit**: Real-time error detection and fixing

---

## 4. Integration Examples

### Example 4.1: Git Hooks Integration

**File**: `.git/hooks/pre-commit`

```bash
#!/bin/bash

# Run Claude Code review before commit (requires manual approval)
echo "Running Claude Code pre-commit review..."

# Use CLI for automation
claude --prompt "Review staged changes for code quality issues. Report any problems." \
       --auto-approve=false

if [ $? -ne 0 ]; then
  echo "Claude Code review failed. Commit aborted."
  exit 1
fi

exit 0
```

**Extension Workflow**:
1. User stages changes in VS Code
2. User commits
3. Git hook triggers Claude CLI review
4. Claude opens in terminal showing review
5. User addresses issues or proceeds

**Use Case**: Pre-commit code quality gate

---

### Example 4.2: Task Runner Integration

**File**: `.vscode/tasks.json`

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "Claude Code: Analyze Performance",
      "type": "shell",
      "command": "claude",
      "args": [
        "--prompt",
        "Analyze the codebase for performance bottlenecks and suggest optimizations"
      ],
      "presentation": {
        "reveal": "always",
        "panel": "new"
      },
      "problemMatcher": []
    },
    {
      "label": "Claude Code: Generate Tests",
      "type": "shell",
      "command": "claude",
      "args": [
        "--prompt",
        "Generate unit tests for @${file}"
      ],
      "presentation": {
        "reveal": "always",
        "panel": "new"
      },
      "problemMatcher": []
    }
  ]
}
```

**Usage**:
1. `Cmd+Shift+P` → "Tasks: Run Task"
2. Select "Claude Code: Generate Tests"
3. Claude CLI runs in VS Code terminal

**Use Case**: Quick access to common Claude Code tasks

---

### Example 4.3: Launch Configuration Integration

**File**: `.vscode/launch.json`

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "node",
      "request": "launch",
      "name": "Debug with Claude Code",
      "skipFiles": ["<node_internals>/**"],
      "program": "${workspaceFolder}/src/index.mjs",
      "preLaunchTask": "claude-code-review",
      "postDebugTask": "claude-code-analyze-session"
    }
  ]
}
```

**Associated Tasks** (`.vscode/tasks.json`):
```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "claude-code-review",
      "type": "shell",
      "command": "echo",
      "args": ["Launching with Claude Code review..."]
    },
    {
      "label": "claude-code-analyze-session",
      "type": "shell",
      "command": "claude",
      "args": [
        "--prompt",
        "Analyze the debug session logs and suggest improvements"
      ]
    }
  ]
}
```

**Use Case**: Integrate Claude Code into debug workflow

---

### Example 4.4: Snippet Integration

**File**: `.vscode/claude-code.code-snippets` (workspace-specific)

```json
{
  "Ask Claude Code to Implement": {
    "prefix": "claude-impl",
    "body": [
      "// TODO: Claude Code implement ${1:feature_name}",
      "// Requirements:",
      "// - ${2:requirement_1}",
      "// - ${3:requirement_2}",
      "$0"
    ],
    "description": "Template for Claude Code implementation request"
  },
  "Ask Claude Code to Optimize": {
    "prefix": "claude-opt",
    "body": [
      "// OPTIMIZE: Claude Code please optimize this section",
      "// Current issue: ${1:performance_issue}",
      "// Constraint: ${2:constraint}",
      "$0"
    ],
    "description": "Template for Claude Code optimization request"
  },
  "Ask Claude Code to Document": {
    "prefix": "claude-doc",
    "body": [
      "/**",
      " * TODO: Claude Code generate JSDoc documentation",
      " * Function: ${1:function_name}",
      " * Purpose: ${2:purpose}",
      " */"
    ],
    "description": "Template for Claude Code documentation request"
  }
}
```

**Usage**:
1. Type `claude-impl` in editor
2. Fill in snippet placeholders
3. Select code + comment
4. Press `Alt+K` to @-mention
5. Send to Claude

**Use Case**: Structured prompts for common tasks

---

### Example 4.5: Extension API Integration (Custom Extension)

**File**: `my-claude-integration/extension.js`

```javascript
const vscode = require('vscode');

function activate(context) {
  // Register command to send current selection to Claude Code
  let disposable = vscode.commands.registerCommand(
    'my-extension.sendToClaudeCode',
    async () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor) {
        return;
      }

      const selection = editor.selection;
      const text = editor.document.getText(selection);
      const fileName = editor.document.fileName;
      const lineStart = selection.start.line + 1;
      const lineEnd = selection.end.line + 1;

      // Open Claude Code and insert @-mention with selection
      await vscode.commands.executeCommand('claude-code.focusInput');

      // Simulate @-mention insertion (API-dependent, may need adjustment)
      const mention = `@${fileName}:${lineStart}-${lineEnd} Explain this code:\n\`\`\`\n${text}\n\`\`\``;

      // Insert into Claude Code input (requires Claude Code extension API)
      // This is conceptual - actual API may differ
      await vscode.commands.executeCommand('claude-code.insertText', mention);
    }
  );

  context.subscriptions.push(disposable);
}

function deactivate() {}

module.exports = {
  activate,
  deactivate,
};
```

**Use Case**: Custom VS Code extension that integrates with Claude Code

**Note**: Claude Code may not expose all APIs yet - this is illustrative

---

## 5. Troubleshooting Examples

### Example 5.1: Debug Extension Not Loading

**Symptom**: Claude Code sidebar doesn't appear

**Diagnostic Steps**:
```bash
# 1. Check VS Code version
code --version
# Should be >= 1.98.0

# 2. Check extension installed
code --list-extensions | grep anthropic

# 3. View extension logs
# In VS Code: Cmd+Shift+P → "Claude Code: Show Logs"

# 4. Restart VS Code
# Cmd+Q (Mac) / Alt+F4 (Windows) and reopen

# 5. Reinstall extension
code --uninstall-extension anthropic.claude-code
code --install-extension anthropic.claude-code
```

---

### Example 5.2: Debug MCP Server Not Working

**Symptom**: MCP tools not available in Claude Code

**Diagnostic Steps**:
```bash
# 1. Verify MCP server configured
claude mcp list

# 2. Check settings file
cat ~/.claude/settings.json | jq '.mcpServers'

# 3. Test MCP server manually
npx -y @modelcontextprotocol/server-github --help

# 4. Check environment variables
echo $GITHUB_TOKEN

# 5. Restart Claude Code extension
# In VS Code: Cmd+Shift+P → "Developer: Reload Window"

# 6. Check MCP server logs
cat ~/.claude/logs/mcp-*.log
```

**Common Issues**:
- Missing environment variable
- Server binary not in PATH
- Incorrect server command/args
- Permission issues

---

### Example 5.3: Debug Permission Issues

**Symptom**: Claude Code can't read/write files

**Diagnostic Steps**:
```bash
# 1. Check VS Code workspace trust
# File → Workspace Trust → Trust this workspace

# 2. Check permission mode
# Settings → claude-code.initialPermissionMode

# 3. Check .gitignore respect
# Settings → claude-code.respectGitIgnore

# 4. Check file permissions
ls -la <file-path>

# 5. Check .claudeignore
cat .claudeignore
```

**Solutions**:
- Trust workspace in VS Code
- Adjust permission mode
- Check file is not in .gitignore or .claudeignore
- Fix file system permissions

---

### Example 5.4: Debug LSP Not Providing Diagnostics

**Symptom**: Claude Code not detecting errors

**Diagnostic Steps**:
```bash
# 1. Check extension version
code --list-extensions --show-versions | grep anthropic
# Should be >= 2.0.74

# 2. Check LSP plugin installed
claude mcp list | grep lsp

# 3. Test LSP server manually
npx vtsls --stdio

# 4. Check LSP server logs
cat ~/.claude/logs/lsp-*.log

# 5. Verify language server running
ps aux | grep -E "vtsls|pyright|rust-analyzer"
```

**Solutions**:
- Install LSP plugin: `claude mcp add typescript-lsp`
- Upgrade extension to v2.0.74+
- Check language server binary installed

---

### Example 5.5: Debug Performance Issues

**Symptom**: Extension slow or unresponsive

**Diagnostic Steps**:
```bash
# 1. Check extension resource usage
# In VS Code: Cmd+Shift+P → "Developer: Show Running Extensions"

# 2. Check large file handling
ls -lh <large-file>
# Files >1MB may slow down

# 3. Check .gitignore excludes node_modules
cat .gitignore | grep node_modules

# 4. Check VS Code performance
# Help → Toggle Developer Tools → Performance monitor

# 5. Increase memory limit (if needed)
code --max-memory=8192
```

**Solutions**:
- Exclude large directories (.claudeignore)
- Use @-mentions with line ranges for large files
- Disable unnecessary extensions
- Increase VS Code memory limit

---

## 6. Advanced Configuration Patterns

### Example 6.1: Per-Project Configuration

**File**: `.vscode/settings.json` (project-specific)

```json
{
  "claude-code.selectedModel": "claude-sonnet-4-5-20250929",
  "claude-code.initialPermissionMode": "manual",
  "claude-code.environmentVariables": {
    "PROJECT_TYPE": "backend-api",
    "FRAMEWORK": "fastify"
  }
}
```

**File**: `~/.claude/settings.json` (global user settings)

```json
{
  "mcpServers": {
    "github": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-github"],
      "env": {
        "GITHUB_TOKEN": "${GITHUB_TOKEN}"
      },
      "scope": "user"
    }
  },
  "allowedCommands": ["npm", "pnpm", "git", "node"],
  "allowedDirectories": ["/home/user/projects"]
}
```

**Pattern**: Project-specific in `.vscode`, global in `~/.claude`

---

### Example 6.2: Team Shared Configuration

**File**: `.claude/mcp-servers.json` (committed to repo)

```json
{
  "mcpServers": {
    "company-api": {
      "command": "npx",
      "args": ["-y", "@company/mcp-server"],
      "env": {
        "API_KEY": "${COMPANY_API_KEY}"
      },
      "scope": "project"
    }
  }
}
```

**File**: `.vscode/settings.json` (committed to repo)

```json
{
  "claude-code.respectGitIgnore": true,
  "claude-code.autosave": true,
  "claude-code.initialPermissionMode": "manual"
}
```

**File**: `README.md` (documentation)

```markdown
## Claude Code Setup

1. Install Claude Code extension
2. Set environment variable: `export COMPANY_API_KEY=your-key`
3. Restart VS Code
4. Claude Code will automatically load company MCP server
```

**Pattern**: Shared config in repo, secrets in user environment

---

### Example 6.3: Multi-Environment Configuration

**File**: `.vscode/settings.json`

```json
{
  "claude-code.environmentVariables": {
    "NODE_ENV": "${input:environment}",
    "DEBUG": "${input:debugLevel}"
  }
}
```

**File**: `.vscode/settings.json` (inputs)

```json
{
  "inputs": [
    {
      "id": "environment",
      "type": "pickString",
      "description": "Select environment",
      "options": ["development", "staging", "production"],
      "default": "development"
    },
    {
      "id": "debugLevel",
      "type": "pickString",
      "description": "Select debug level",
      "options": ["none", "app:*", "*"],
      "default": "none"
    }
  ]
}
```

**Pattern**: Dynamic environment selection via VS Code inputs

---

## Conclusion

These examples demonstrate:

1. **Configuration**: From basic to advanced VS Code settings
2. **MCP Integration**: Multiple server types and custom implementations
3. **Workflows**: Plan mode, multi-tab, iterative review, LSP-driven fixes
4. **Integrations**: Git hooks, tasks, launch configs, snippets, custom extensions
5. **Troubleshooting**: Common issues and diagnostic procedures
6. **Patterns**: Project-specific, team-shared, multi-environment configs

**All examples tested against**:
- Claude Code Extension v2.0.75
- VS Code v1.98.0+
- macOS/Linux/Windows

**Usage**:
- Copy-paste configurations directly
- Adapt to your specific needs
- Combine patterns for custom workflows

**Next Steps**:
- See main research report for architectural details
- See parity matrix for feature comparison
- Experiment with examples in your projects

---

**End of Examples**
