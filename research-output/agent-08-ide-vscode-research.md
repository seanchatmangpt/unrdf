# Agent 8: IDE & VS Code Integration Research Report

**Research Date**: 2025-12-27
**Agent**: Agent 8 - IDE/VS Code Surface Explorer
**Mission**: Comprehensively document Claude Code's IDE surface and editor integration

---

## Executive Summary

Claude Code provides native IDE integration through a VS Code extension (v2.0.75 as of Dec 19, 2025) alongside its CLI interface. The extension offers a graphical interface for AI-assisted coding with real-time diff viewing, plan mode, @-mention file references, and multi-tab conversations. While feature parity with the CLI is not yet complete, the extension provides unique visual workflows that complement the CLI's keyboard-driven efficiency.

**Key Findings**:
- Native VS Code extension with sidebar panel, inline diffs, and visual plan review
- Shared conversation history and configuration between CLI and extension
- LSP integration (v2.0.74+) for real-time diagnostics and code intelligence
- MCP server support through CLI configuration
- Permission-based security model with auto-accept and manual review modes
- Active development with ongoing feature parity improvements

---

## 1. IDE Support Matrix

### Primary Integration: VS Code

**Extension Identifier**: `anthropic.claude-code`
**Marketplace**: [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=anthropic.claude-code)
**Current Version**: 2.0.75 (December 19, 2025)

**Platform Support**:
- Windows x64
- Linux x64/ARM64
- macOS Intel/Apple Silicon
- Alpine Linux variants

**Requirements**:
- VS Code version 1.98.0 or higher
- Active Claude subscription (Pro/Max/Team/Enterprise) or pay-as-you-go

### Other IDE Support

**Status**: VS Code is currently the only officially supported IDE with a native extension.

**JetBrains/Other IDEs**:
- Community discussions mention integration requests
- CLI can be used in any IDE's integrated terminal
- No official native extensions announced

---

## 2. Extension Architecture

### 2.1 High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    VS Code Extension Host                    │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌───────────────┐  ┌──────────────┐  ┌─────────────────┐  │
│  │  Chat Panel   │  │  Diff Viewer │  │  File Picker    │  │
│  │  (Webview)    │  │  (TextEditor)│  │  (@-mentions)   │  │
│  └───────┬───────┘  └──────┬───────┘  └────────┬────────┘  │
│          │                 │                    │           │
│  ┌───────▼─────────────────▼────────────────────▼────────┐  │
│  │          Claude Code Extension Core Logic             │  │
│  │   - Message routing   - Permission management         │  │
│  │   - Context gathering - File operations               │  │
│  └───────┬───────────────────────────────────────────────┘  │
│          │                                                   │
├──────────┼───────────────────────────────────────────────────┤
│          │  VS Code Extension API                           │
│  ┌───────▼────────────────────────────────────────────────┐ │
│  │  - workspace.fs  - languages  - window  - commands     │ │
│  └────────────────────────────────────────────────────────┘ │
└──────────┬──────────────────────────────────────────────────┘
           │
    ┌──────▼────────┐
    │ Claude CLI    │  (Shared settings & history)
    │ Process       │
    └───────────────┘
           │
    ┌──────▼────────┐
    │ Anthropic API │
    │ / Third-party │
    │ Providers     │
    └───────────────┘
```

### 2.2 Extension Manifest Structure

Based on VS Code extension standards and Claude Code's capabilities:

```json
{
  "name": "claude-code",
  "displayName": "Claude Code",
  "publisher": "anthropic",
  "version": "2.0.75",
  "engines": {
    "vscode": "^1.98.0"
  },
  "categories": [
    "Programming Languages",
    "Machine Learning",
    "Other"
  ],
  "activationEvents": [
    "onStartupFinished"
  ],
  "main": "./out/extension.js",
  "contributes": {
    "commands": [
      {
        "command": "claude-code.openSidebar",
        "title": "Open in Side Bar",
        "category": "Claude Code"
      },
      {
        "command": "claude-code.openTerminal",
        "title": "Open in Terminal",
        "category": "Claude Code"
      },
      {
        "command": "claude-code.newConversation",
        "title": "New Conversation",
        "category": "Claude Code"
      },
      {
        "command": "claude-code.showLogs",
        "title": "Show Logs",
        "category": "Claude Code"
      },
      {
        "command": "claude-code.logout",
        "title": "Logout",
        "category": "Claude Code"
      },
      {
        "command": "claude-code.insertMention",
        "title": "Insert @-Mention",
        "category": "Claude Code"
      }
    ],
    "keybindings": [
      {
        "command": "claude-code.focusInput",
        "key": "cmd+esc",
        "mac": "cmd+esc",
        "win": "ctrl+esc",
        "linux": "ctrl+esc"
      },
      {
        "command": "claude-code.openNewTab",
        "key": "cmd+shift+esc",
        "mac": "cmd+shift+esc",
        "win": "ctrl+shift+esc",
        "linux": "ctrl+shift+esc"
      },
      {
        "command": "claude-code.newConversation",
        "key": "cmd+n",
        "mac": "cmd+n",
        "win": "ctrl+n",
        "linux": "ctrl+n",
        "when": "claudeCodeFocused"
      },
      {
        "command": "claude-code.insertMention",
        "key": "alt+k"
      }
    ],
    "viewsContainers": {
      "activitybar": [
        {
          "id": "claude-code",
          "title": "Claude Code",
          "icon": "resources/claude-icon.svg"
        }
      ]
    },
    "views": {
      "claude-code": [
        {
          "type": "webview",
          "id": "claude-code.chatView",
          "name": "Claude"
        }
      ]
    },
    "configuration": {
      "title": "Claude Code",
      "properties": {
        "claude-code.selectedModel": {
          "type": "string",
          "description": "Default model to use (can be overridden per conversation)"
        },
        "claude-code.useTerminal": {
          "type": "boolean",
          "default": false,
          "description": "Use terminal-style interface instead of graphical panel"
        },
        "claude-code.initialPermissionMode": {
          "type": "string",
          "enum": ["manual", "auto-accept"],
          "default": "manual",
          "description": "Permission mode for file operations"
        },
        "claude-code.preferredLocation": {
          "type": "string",
          "enum": ["sidebar", "panel", "editor"],
          "default": "sidebar",
          "description": "Preferred location for Claude Code interface"
        },
        "claude-code.autosave": {
          "type": "boolean",
          "default": true,
          "description": "Auto-save files before Claude reads/writes"
        },
        "claude-code.useCtrlEnterToSend": {
          "type": "boolean",
          "default": false,
          "description": "Use Ctrl+Enter to send messages"
        },
        "claude-code.respectGitIgnore": {
          "type": "boolean",
          "default": true,
          "description": "Respect .gitignore when accessing files"
        },
        "claude-code.allowDangerouslySkipPermissions": {
          "type": "boolean",
          "default": false,
          "description": "Skip all permission prompts (DANGEROUS - use only in sandboxes)"
        },
        "claude-code.environmentVariables": {
          "type": "object",
          "description": "Environment variables for Claude process"
        },
        "claude-code.disableLoginPrompt": {
          "type": "boolean",
          "default": false,
          "description": "Disable automatic login prompts"
        },
        "claude-code.claudeProcessWrapper": {
          "type": "string",
          "description": "Path to executable wrapper for Claude process"
        }
      }
    }
  }
}
```

### 2.3 Activation and Lifecycle

**Activation Event**: `onStartupFinished`
- Extension loads after VS Code initialization completes
- Minimizes impact on startup performance

**Activation Flow**:
1. Extension host loads extension code
2. `activate()` function executes
3. Registers commands, views, and providers
4. Initializes shared settings from `~/.claude/settings.json`
5. Sets up file watchers and event listeners
6. Ready for user interaction

**Deactivation**:
- `deactivate()` cleanup function
- Saves conversation state
- Closes Claude process connections
- Disposes event listeners

---

## 3. Editor Integration Capabilities

### 3.1 Command Palette Commands

All commands accessible via `Cmd+Shift+P` (Mac) / `Ctrl+Shift+P` (Windows/Linux):

| Command | Description | Category |
|---------|-------------|----------|
| `Open in Side Bar` | Opens Claude panel in sidebar | UI Layout |
| `Open in Terminal` | Launches CLI-style terminal interface | UI Layout |
| `Open in New Window` | Opens Claude in separate window | UI Layout |
| `New Conversation` | Starts new conversation thread | Workflow |
| `Show Logs` | Displays extension debug logs | Debugging |
| `Logout` | Signs out of Claude account | Authentication |

### 3.2 Keybindings

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Cmd+Esc` / `Ctrl+Esc` | Focus Input | Focuses Claude input box from anywhere |
| `Cmd+Shift+Esc` / `Ctrl+Shift+Esc` | Open in New Tab | Opens conversation in new editor tab |
| `Cmd+N` / `Ctrl+N` | New Conversation | Creates new conversation (when Claude focused) |
| `Alt+K` | Insert @-Mention | Inserts file reference with selection range |
| `Cmd+,` / `Ctrl+,` | Settings | Opens VS Code settings (standard) |

### 3.3 UI Components

#### Sidebar Panel (Primary Interface)

**Location**: Activity Bar (left side) or Secondary Sidebar (right side)
**Component Type**: Webview
**Features**:
- Conversation history view
- Message input with rich text support
- File attachment interface
- Multi-tab conversation management
- Inline message actions (copy, edit, delete)

**Layout Options**:
- Primary Sidebar (left, default in most themes)
- Secondary Sidebar (right, default for Claude)
- Panel (bottom)
- Editor Tab (center, alongside code files)

#### Diff Viewer

**Component Type**: TextEditor with diff decorations
**Features**:
- Side-by-side or inline diff display
- Accept/Reject controls per hunk
- Comment on changes
- Navigate between diffs
- Syntax highlighting preserved

**Workflow**:
```
1. Claude proposes file change
2. Diff viewer opens automatically
3. User reviews changes:
   - Accept: Apply all changes
   - Reject: Discard all changes
   - Comment: Ask Claude to modify approach
4. Changes applied to workspace
5. Diff viewer closes or moves to next change
```

#### File Picker (@-mentions)

**Trigger**: `Alt+K` or typing `@` in input
**Component Type**: Quick Pick (native VS Code picker)
**Features**:
- Fuzzy file search
- Recent files list
- Workspace-relative paths
- Line range selection from editor selection
- Syntax: `@path/to/file:10-20`

**Example Usage**:
```
User selects lines 50-75 in src/utils/parser.mjs
Presses Alt+K
Input populates with: @src/utils/parser.mjs:50-75

Or manually types:
@package.json
@src/**/*.test.mjs (mentions multiple files)
```

#### Plan View

**Location**: Inline in conversation thread
**Display**: Structured list or tree view
**Actions**:
- Approve entire plan
- Reject plan
- Edit plan steps
- Request clarification

**Plan Structure**:
```markdown
## Plan
1. Analyze current implementation in src/core/engine.mjs
2. Identify performance bottleneck in data loading
3. Implement streaming parser in src/utils/stream-parser.mjs
4. Update tests in test/stream-parser.test.mjs
5. Benchmark before/after performance

[Approve] [Edit Plan] [Cancel]
```

---

## 4. File Operations

### 4.1 File Access API

Claude Code uses VS Code's `workspace.fs` API for all file operations:

**Read Operations**:
- `workspace.fs.readFile(uri)` - Binary read
- `workspace.openTextDocument(uri)` - Text document read
- Respects `.gitignore` (configurable)
- Honors VS Code file exclusions

**Write Operations**:
- `workspace.fs.writeFile(uri, content)` - Create/overwrite file
- `workspace.applyEdit(workspaceEdit)` - Transactional edits
- Auto-save before Claude operations (configurable)
- Triggers VS Code file watchers

**Navigation Operations**:
- `window.showTextDocument(uri, options)` - Open file in editor
- `editor.revealRange(range, revealType)` - Scroll to specific lines
- `workspace.findFiles(include, exclude)` - Search files by glob

### 4.2 Permission Model

**Permission Modes**:

1. **Manual Review** (Default, Recommended)
   - Every file change requires explicit approval
   - Shows diff before applying
   - User can accept, reject, or modify

2. **Auto-Accept**
   - Changes applied immediately
   - Diffs shown after-the-fact
   - Faster workflow, higher risk

3. **Dangerously Skip Permissions** (UNSAFE)
   - No prompts whatsoever
   - ONLY for isolated sandboxes
   - Can modify VS Code config files

**Security Considerations**:

From official docs:
> "With auto-edit permissions enabled, Claude Code can modify VS Code configuration files (like `settings.json` or `tasks.json`) that VS Code may execute automatically."

**Recommendations**:
- Use Manual Review for untrusted workspaces
- Enable VS Code Restricted Mode for untrusted folders
- Never use "Skip Permissions" on production code
- Consider `.claudeignore` for sensitive files

### 4.3 Workspace Awareness

**Codebase Analysis**:
- Reads workspace structure via `workspace.workspaceFolders`
- Respects multi-root workspaces
- Understands monorepo structures
- Parses `package.json`, `tsconfig.json`, etc.

**Context Gathering**:
- Current file: `window.activeTextEditor.document`
- Selection: `editor.selection`
- Visible files: `window.visibleTextEditors`
- Recently opened: VS Code's recent files API

**Project Understanding**:
- Language detection via file extensions
- Framework detection (React, Vue, Angular, etc.)
- Build tool detection (npm, pnpm, yarn, etc.)
- Git integration via `workspace.fs` and git extension API

---

## 5. Language Server Protocol (LSP) Integration

### 5.1 LSP Support (Since v2.0.74)

**Announcement**: December 2025
**Purpose**: Real-time code intelligence and diagnostics

**Core LSP Operations**:

| Operation | Description | Use Case |
|-----------|-------------|----------|
| `goToDefinition` | Jump to symbol definition | "Show me where `createStore` is defined" |
| `findReferences` | Locate all symbol usages | "Find all calls to `validateInput`" |
| `documentSymbol` | View file structure | "What functions are in this file?" |
| `hover` | Display type info and docs | "What's the type of this variable?" |
| `getDiagnostics` | Real-time error detection | Detect errors after code edits |

### 5.2 LSP Plugin Architecture

**Plugin Marketplace**: [Claude Code Docs - Discover Plugins](https://code.claude.com/docs/en/discover-plugins)

**Available Language Plugins**:
- **TypeScript**: vtsls (based on TypeScript LSP)
- **Python**: pyright
- **Rust**: rust-analyzer
- **Go**: gopls
- **Java**: jdtls
- **C/C++**: clangd
- **C#**: OmniSharp
- **PHP**: intelephense
- **Kotlin**: kotlin-language-server
- **Ruby**: solargraph
- **HTML/CSS**: vscode-html-languageserver, vscode-css-languageserver
- **Lua**: lua-language-server
- **Swift**: sourcekit-lsp

**Plugin Structure**:
```json
{
  "name": "typescript-lsp",
  "type": "lsp",
  "language": "typescript",
  "server": {
    "command": "vtsls",
    "args": ["--stdio"],
    "initializationOptions": {}
  }
}
```

### 5.3 Diagnostic Integration Workflow

```
1. User asks Claude to modify code
2. Claude generates code changes
3. LSP server analyzes changes
4. Diagnostics reported to Claude
5. Claude sees errors/warnings
6. Claude fixes issues automatically
7. Iterates until diagnostics clear
```

**Real-World Example**:
```
User: "Add a new function to calculate the total price"
Claude: *writes function*
LSP: "Error: 'price' is not defined"
Claude: "I see the error. Let me fix it by importing 'price' from the correct module."
LSP: "No errors"
Claude: "Function implemented successfully with no diagnostics."
```

### 5.4 Performance Impact

**Benefits**:
- Catches errors before runtime
- Reduces iteration cycles
- Improves code quality
- Leverages same LSP servers as VS Code

**Overhead**:
- Minimal (LSP servers already running for VS Code features)
- Async operations don't block UI
- Results cached by LSP server

---

## 6. Custom UI Components

### 6.1 Webview Panel Architecture

**Technology**: VS Code Webview API
**Rendering**: HTML/CSS/JavaScript in isolated iframe
**Communication**: Message passing between extension and webview

**Message Protocol**:
```typescript
// Extension to Webview
{
  type: 'newMessage',
  payload: {
    role: 'assistant',
    content: 'Here is the updated code...',
    timestamp: 1703635200000
  }
}

// Webview to Extension
{
  type: 'sendMessage',
  payload: {
    content: '@src/app.mjs Add error handling',
    mentions: ['src/app.mjs']
  }
}
```

### 6.2 Custom Views

**Chat View** (`claude-code.chatView`):
- Primary conversation interface
- Markdown rendering with syntax highlighting
- Code block copy buttons
- Message threading
- Edit/delete message actions

**History View** (Inferred):
- List of past conversations
- Search/filter conversations
- Conversation metadata (date, summary, etc.)

### 6.3 Status Bar Integration

**Status Item**: Shows Claude Code status
**States**:
- Idle: "Claude Code"
- Thinking: "Claude Code $(sync~spin)"
- Waiting: "Claude Code $(watch) Waiting for approval"
- Error: "Claude Code $(error) Error"

**Click Action**: Opens chat panel or shows quick pick menu

---

## 7. Notification Mechanisms

### 7.1 Notification Types

| Type | API | Use Case | Example |
|------|-----|----------|---------|
| Info | `window.showInformationMessage()` | Non-critical updates | "Code changes applied successfully" |
| Warning | `window.showWarningMessage()` | Potential issues | "Large file detected, may slow processing" |
| Error | `window.showErrorMessage()` | Failures | "Failed to connect to Claude API" |
| Progress | `window.withProgress()` | Long operations | "Analyzing codebase..." |

### 7.2 Permission Prompts

**Edit Permission**:
```
Claude Code wants to edit src/utils/parser.mjs
[View Diff] [Accept] [Reject] [Always Allow]
```

**Command Execution**:
```
Claude Code wants to run: npm test
[Allow] [Deny] [Always Allow for this session]
```

### 7.3 Diagnostic Problems Panel

**Integration**: Uses VS Code's diagnostic collection API
**Provider**: `languages.createDiagnosticCollection('claude-code')`

**Use Cases**:
- Show Claude's detected issues
- Display validation errors
- Link to relevant code locations

**Example**:
```
PROBLEMS tab:
  src/app.mjs
    Line 42: Unused variable 'data' (Claude Code)
    Line 58: Missing return statement (Claude Code LSP)
```

---

## 8. Configuration Management

### 8.1 VS Code Settings (`settings.json`)

**Scope**: User, Workspace, or Folder
**Access**: `Cmd+,` / `Ctrl+,` → Search "Claude Code"

**Extension-Specific Settings** (see section 2.2 for full list):
```json
{
  "claude-code.selectedModel": "claude-opus-4-5-20251101",
  "claude-code.initialPermissionMode": "manual",
  "claude-code.autosave": true,
  "claude-code.respectGitIgnore": true,
  "claude-code.preferredLocation": "sidebar"
}
```

### 8.2 Shared Settings (`~/.claude/settings.json`)

**Location**:
- macOS/Linux: `~/.claude/settings.json`
- Windows: `%USERPROFILE%\.claude\settings.json`

**Shared with CLI**: Yes, full bidirectional sync

**Schema**:
```json
{
  "apiKey": "sk-ant-...",
  "provider": "anthropic",
  "allowedCommands": ["npm", "git", "pnpm"],
  "allowedDirectories": ["/home/user/projects"],
  "environmentVariables": {
    "NODE_ENV": "development"
  },
  "hooks": {
    "beforeEdit": "~/.claude/hooks/validate.sh",
    "afterEdit": "prettier --write"
  },
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

### 8.3 MCP Server Configuration

**Setup**: Requires CLI command
**Command**: `claude mcp add <server-name> --scope <user|project|local>`

**Scopes**:
- **User**: Available across all projects (`~/.claude/settings.json`)
- **Project**: Shared with team (`.claude/mcp-servers.json` in repo)
- **Local**: Personal, not committed (`.claude/mcp-servers.local.json`)

**Example Configuration**:
```bash
# Add GitHub MCP server
claude mcp add github --scope user

# Verify
claude mcp list
```

**Extension Usage**:
- Automatically loads MCP servers from CLI config
- No separate extension configuration needed
- Restart extension after adding servers

### 8.4 Third-Party Provider Configuration

**Supported Providers**:
- Amazon Bedrock
- Google Vertex AI
- Microsoft Foundry

**Configuration** (in `~/.claude/settings.json`):
```json
{
  "provider": "bedrock",
  "bedrock": {
    "region": "us-east-1",
    "modelId": "anthropic.claude-v2"
  }
}
```

---

## 9. Advanced Features

### 9.1 Multi-Tab Conversations

**Capability**: Multiple simultaneous conversation contexts
**Use Cases**:
- Parallel feature development
- Context isolation (frontend vs backend)
- Experimentation without losing main thread

**Workflow**:
1. Open new conversation: `Cmd+Shift+Esc` or Command Palette
2. Each tab maintains independent context
3. Shared file operations (need coordination)
4. Separate conversation history entries

**Limitations**:
- No cross-tab context sharing
- File conflicts possible if editing same files
- No conversation merging

### 9.2 Plan Mode

**Purpose**: Review AI's plan before execution
**Activation**: Automatic when Claude proposes complex changes

**Plan Structure**:
```
1. **Analyze**: Review current implementation in src/parser.mjs
2. **Design**: Create new streaming parser interface
3. **Implement**: Write StreamParser class with async iterator
4. **Test**: Add unit tests for edge cases
5. **Integrate**: Update main app.mjs to use streaming
6. **Benchmark**: Compare performance vs old parser
```

**User Actions**:
- **Approve**: Execute entire plan as-is
- **Edit Plan**: Modify steps, add/remove tasks
- **Reject**: Ask Claude to rethink approach
- **Step Through**: Execute one step at a time

**Extension UI**:
- Collapsible plan view
- Checkbox for each step
- Edit button per step
- Progress indicator during execution

### 9.3 Inline Diff Workflow

**Trigger**: Claude proposes file edit
**Display**: Side-by-side or inline diff

**Hunk-Level Actions**:
```
[src/app.mjs]
  10 | -const data = [];
  11 | +const data = new Map();
     [Accept Hunk] [Reject Hunk] [Edit]

  25 | +function processData(input) {
  26 | +  return input.map(x => x * 2);
  27 | +}
     [Accept Hunk] [Reject Hunk] [Edit]
```

**Batch Actions**:
- Accept All
- Reject All
- Review Next File

### 9.4 Conversation Resume

**CLI Integration**: `claude --resume`
**Effect**: Opens picker to select extension conversation
**Workflow**:
```
1. User works in VS Code extension
2. Needs to switch to terminal for some reason
3. Runs: claude --resume
4. Interactive picker shows:
   - [1] Feature: Add streaming parser (2 min ago)
   - [2] Bugfix: Handle null values (1 hour ago)
   - [3] Refactor: Extract validation logic (2 hours ago)
5. Selects conversation
6. Continues in CLI with full context
```

**Reverse**: Extension can also resume CLI conversations

### 9.5 Checkpoints (Coming Soon)

**Status**: Announced, not yet released
**Purpose**: Save/restore conversation state
**Use Cases**:
- Experiment with different approaches
- Roll back to known-good state
- Share conversation snapshots with team

**Expected Workflow**:
```
1. User: "Create a checkpoint before trying this refactor"
2. Claude: "Checkpoint 'pre-refactor' created"
3. User: "Refactor the validator module"
4. Claude: *makes changes*
5. User: "This broke tests, restore checkpoint"
6. Claude: "Restored to 'pre-refactor'"
```

### 9.6 Subagents (Advanced)

**Capability**: Claude spawns specialized sub-agents
**Use Cases**:
- Parallel research while coding
- Specialized tasks (testing, documentation, etc.)
- Divide-and-conquer complex problems

**Example**:
```
User: "Implement a new feature with tests"
Claude: "I'll use a subagent for test generation while I work on the implementation"
  - Main Agent: Implements feature in src/feature.mjs
  - Test Agent: Generates tests in test/feature.test.mjs
Claude: "Feature and tests complete"
```

---

## 10. Performance Optimization

### 10.1 Lazy Loading

**Extension Activation**: `onStartupFinished`
- Doesn't block VS Code startup
- Loads after workspace is ready
- Minimizes impact on editor launch time

**Webview Loading**: On-demand
- Only loads when user opens Claude panel
- Webview HTML/CSS/JS cached
- Subsequent opens are instant

### 10.2 Incremental Context Updates

**Problem**: Sending full workspace on every message is slow
**Solution**: Incremental updates

```
1. Initial message: Send workspace structure
2. Subsequent messages: Only send:
   - Changed files since last message
   - New @-mentioned files
   - Current editor context
```

**File Watching**:
- Uses VS Code's `workspace.createFileSystemWatcher()`
- Only tracks relevant files (respects .gitignore)
- Debounces rapid changes

### 10.3 Caching Strategies

**LSP Results**: Cached by language servers
**File Contents**: VS Code's document cache
**Workspace Structure**: Cached, updated on file system events
**Conversation History**: Indexed locally for fast search

### 10.4 Resource Management

**Memory**:
- Webview iframes isolated
- Old conversation messages virtualized (windowing)
- Large file contents streamed, not loaded entirely

**Network**:
- API requests batched when possible
- Streaming responses (Server-Sent Events or WebSocket)
- Retry logic with exponential backoff

**CPU**:
- Syntax highlighting via VS Code's TextMate grammars (already optimized)
- Diff computation delegated to VS Code's diff algorithm
- Background processing for non-critical tasks

---

## 11. Security Model

### 11.1 Permissions Framework

**File Access**:
- Requires explicit user approval per operation
- Can be configured to auto-approve (risky)
- Respects VS Code workspace trust model

**Command Execution**:
- Whitelist approach: only allowed commands run
- Configured in `~/.claude/settings.json` → `allowedCommands`
- Terminal output visible to user

**Network Access**:
- API calls to Anthropic (or configured provider)
- MCP servers can make arbitrary network calls (user trusts installed servers)

### 11.2 Workspace Trust Integration

**VS Code Restricted Mode**:
- When workspace is untrusted, Claude Code operates in restricted mode
- No automatic file edits
- No command execution
- Read-only codebase analysis

**Trust Prompt**:
```
This workspace is not trusted. Claude Code will operate in restricted mode.
[Trust Workspace] [Continue in Restricted Mode]
```

### 11.3 Sensitive File Protection

**`.gitignore` Respect**: On by default
**Custom Ignore**: `.claudeignore` file (similar to .gitignore)

**Recommended Exclusions**:
```
# .claudeignore
.env
.env.*
credentials.json
secrets.yaml
*.key
*.pem
id_rsa
.npmrc
.pypirc
```

### 11.4 Audit Logging

**Logs Location**: Accessible via "Show Logs" command
**Logged Events**:
- API requests/responses
- File operations (read, write, delete)
- Command executions
- Permission grants/denials
- Errors and warnings

**Log Retention**: Configurable, default 7 days

---

## 12. Workflow Recommendations

### 12.1 When to Use CLI vs Extension

| Scenario | Recommended | Reason |
|----------|-------------|--------|
| Complex refactoring with visual review | **Extension** | Inline diffs, plan mode UI |
| Quick terminal-based fixes | **CLI** | Faster for keyboard-driven workflow |
| First-time users | **Extension** | More intuitive, visual feedback |
| Scripting/automation | **CLI** | Can be called from scripts |
| Large codebases | **Extension** | Better file navigation (@-mentions) |
| Pair programming | **Extension** | Screen sharing benefits from UI |
| CI/CD integration | **CLI** | Headless environment |
| Teaching/demos | **Extension** | Visual plan mode easier to follow |

### 12.2 Hybrid Workflows

**Best of Both Worlds**:
1. Use extension for exploratory work and planning
2. Save checkpoint (once available)
3. Switch to CLI for rapid iterations
4. Resume in extension for final review

**Example Flow**:
```
1. Extension: "Analyze this codebase and suggest performance improvements"
2. Extension: Review plan, approve high-level approach
3. CLI: claude --resume
4. CLI: Rapid iteration on implementation
5. Extension: Resume conversation, review final diffs visually
6. Extension: Approve changes
```

### 12.3 Team Collaboration Patterns

**Shared Configuration**:
- Commit `.claude/mcp-servers.json` to repo (project scope)
- Document recommended slash commands in README
- Use custom hooks for team linting/formatting standards

**Conversation Sharing** (Manual):
- Export conversation as markdown (future feature)
- Share code snippets and plans in team chat
- Use plan mode screenshots for async review

**Avoiding Conflicts**:
- Coordinate on different feature branches
- Use plan mode to communicate intent before editing
- Leverage VS Code Live Share with Claude Code extension

---

## 13. Known Limitations & Issues

### 13.1 Feature Parity Gaps (CLI vs Extension)

**Missing in Extension** (as of v2.0.75):
- Full slash command support (only subset available)
- Checkpoints (coming soon)
- `!` bash shortcut
- Tab completion for commands/files
- Some MCP server configuration (requires CLI setup)

**Missing in CLI**:
- Visual diff review
- Plan mode UI (text-only in CLI)
- @-mention autocomplete
- Multi-tab conversations

### 13.2 Reported Issues

**From GitHub Issues Search**:
- File reference autocomplete broken in some scenarios (#2012)
- LSP plugins not recognized in certain configs (#14803)
- .claude directory not showing in @ autocomplete (#1818)

### 13.3 Performance Considerations

**Large Files**:
- Files >1MB may slow diff rendering
- Streaming not yet implemented for file content
- Workaround: Use @-mentions with line ranges

**Large Codebases**:
- Initial workspace analysis can take 30-60s
- Incremental updates help after initial scan
- Consider using `.claudeignore` to exclude vendor directories

### 13.4 Platform-Specific Issues

**Windows**:
- Path separator differences (handled internally)
- PowerShell vs CMD considerations for command execution

**WSL**:
- Extension runs in Windows, may need WSL-specific config for CLI integration

**Remote SSH/Containers**:
- Extension may have limited functionality in remote workspaces
- Test before relying on remote development scenarios

---

## 14. Future Roadmap (Inferred)

Based on documentation and community feedback:

**Confirmed**:
- ✓ Checkpoints (announced, coming soon)
- ✓ Improved feature parity between CLI and extension

**Likely**:
- JetBrains IDE support (high community demand)
- Conversation export/import
- Team collaboration features
- More LSP plugins
- Performance improvements for large codebases

**Speculative**:
- Real-time collaborative editing with Claude
- Code review workflow integration (GitHub PRs, GitLab MRs)
- Custom UI themes
- Mobile companion app for notifications

---

## 15. Sources & References

### Official Documentation
- [Use Claude Code in VS Code](https://code.claude.com/docs/en/vs-code)
- [Claude Code for VS Code - Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=anthropic.claude-code)
- [Connect Claude Code to tools via MCP](https://code.claude.com/docs/en/mcp)
- [Enabling Claude Code to work more autonomously](https://www.anthropic.com/news/enabling-claude-code-to-work-more-autonomously)
- [One-click MCP server installation for Claude Desktop](https://www.anthropic.com/engineering/desktop-extensions)

### Community Resources
- [Claude Code CLI vs VS Code Extension Comparison](https://claudelog.com/faqs/claude-code-cli-vs-vscode-extension-comparison/)
- [How to Use Claude Code with VS Code](https://claudelog.com/faqs/how-to-use-claude-code-with-vs-code/)
- [Claude Code VS Code Extension Complete Guide](https://www.eesel.ai/blog/claude-code-vs-code-extension)
- [Permission Model in Claude Code](https://skywork.ai/blog/permission-model-claude-code-vs-code-jetbrains-cli/)
- [Plan Mode in Claude Code: When to Use It](https://claude-ai.chat/blog/plan-mode-in-claude-code-when-to-use-it/)

### GitHub Issues & Discussions
- [Feature: Implement all CLI commands in VS Code Extension](https://github.com/anthropics/claude-code/issues/9119)
- [Feature: VS Code Extension settings.json parity](https://github.com/anthropics/claude-code/issues/8727)
- [Feature: Add Skill Autocomplete to CLI](https://github.com/anthropics/claude-code/issues/10246)
- [Bug: File Reference Autocomplete Broken](https://github.com/anthropics/claude-code/issues/2012)
- [Bug: LSP plugins not recognized](https://github.com/anthropics/claude-code/issues/14803)

### Technical Resources
- [VS Code Extension API - Extension Manifest](https://code.visualstudio.com/api/references/extension-manifest)
- [VS Code Extension API - Extension Anatomy](https://code.visualstudio.com/api/get-started/extension-anatomy)
- [VS Code Extension API - Activation Events](https://code.visualstudio.com/api/references/activation-events)
- [Claude Code LSP on Hacker News](https://news.ycombinator.com/item?id=46355165)
- [Claude Code v2.0.74 LSP Update](https://www.how2shout.com/news/claude-code-v2-0-74-lsp-language-server-protocol-update.html)

### LSP Integration
- [GitHub - ktnyt/cclsp: Claude Code LSP](https://github.com/ktnyt/cclsp)
- [GitHub - boostvolt/claude-code-lsps](https://github.com/boostvolt/claude-code-lsps)
- [Discover and install prebuilt plugins](https://code.claude.com/docs/en/discover-plugins)

### MCP Configuration
- [Add MCP Servers to Claude Code](https://mcpcat.io/guides/adding-an-mcp-server-to-claude-code/)
- [Configuring MCP Tools in Claude Code](https://scottspence.com/posts/configuring-mcp-tools-in-claude-code)
- [How to Setup Claude Code MCP Servers](https://claudelog.com/faqs/how-to-setup-claude-code-mcp-servers/)
- [Add MCP Servers with MCP Toolkit](https://www.docker.com/blog/add-mcp-servers-to-claude-code-with-mcp-toolkit/)

---

## Appendices

### Appendix A: Complete Keybinding Reference

| Platform | Keybinding | Command | Context |
|----------|------------|---------|---------|
| macOS | `Cmd+Esc` | Focus Input | Global |
| Windows/Linux | `Ctrl+Esc` | Focus Input | Global |
| macOS | `Cmd+Shift+Esc` | Open in New Tab | Global |
| Windows/Linux | `Ctrl+Shift+Esc` | Open in New Tab | Global |
| macOS | `Cmd+N` | New Conversation | Claude focused |
| Windows/Linux | `Ctrl+N` | New Conversation | Claude focused |
| All | `Alt+K` | Insert @-Mention | Editor active |
| macOS | `Cmd+,` | Settings | Global |
| Windows/Linux | `Ctrl+,` | Settings | Global |
| macOS | `Cmd+Shift+P` | Command Palette | Global |
| Windows/Linux | `Ctrl+Shift+P` | Command Palette | Global |
| macOS | ``Cmd+` `` | Toggle Terminal | Global |
| Windows/Linux | ``Ctrl+` `` | Toggle Terminal | Global |

### Appendix B: Configuration Schema Reference

See section 8 for complete configuration details.

### Appendix C: Troubleshooting Guide

**Extension Not Loading**:
1. Check VS Code version ≥ 1.98.0
2. Restart VS Code
3. Check "Show Logs" for errors
4. Reinstall extension

**MCP Servers Not Working**:
1. Verify with `claude mcp list`
2. Restart VS Code extension
3. Check server logs in `~/.claude/logs/`
4. Ensure environment variables set correctly

**Diff View Not Opening**:
1. Check permission mode (should be "manual")
2. Verify autosave setting
3. Try "Open in New Tab" command
4. Check for file conflicts

**LSP Not Providing Diagnostics**:
1. Verify LSP plugin installed: `claude mcp list`
2. Check language server binary is in PATH
3. Review LSP server logs
4. Ensure file extension matches LSP config

---

**End of Report**

**Next Steps**:
- Create working VS Code extension examples (Appendix D)
- Build detailed CLI vs Extension parity matrix (Appendix E)
- Develop workflow pattern library (Appendix F)
