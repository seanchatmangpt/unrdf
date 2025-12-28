# Claude Code Programmatic API Reference

**Version:** 2.0.59
**Last Updated:** 2025-12-27

## CLI Interface

### Command Syntax

```bash
claude [options] [command] [prompt]
```

### Core Options

#### `-p, --print`

**Type:** Boolean flag
**Description:** Non-interactive prompt mode - executes single prompt and exits
**Security:** Skips workspace trust dialog - only use in trusted directories

```bash
claude -p "Your prompt here"
```

#### `--output-format <format>`

**Type:** Enum: `"text"` | `"json"` | `"stream-json"`
**Default:** `"text"`
**Description:** Controls output format
**Requires:** `--print` mode

```bash
# Text output (default)
claude -p "Hello" --output-format text

# Structured JSON
claude -p "Hello" --output-format json

# Streaming NDJSON
claude -p "Hello" --output-format stream-json
```

**Output Schemas:**

**Text:**
```
Plain text response
```

**JSON:**
```json
{
  "content": "Response text",
  "model": "claude-sonnet-4-5-20250929",
  "session_id": "uuid",
  "tool_uses": [
    {
      "tool": "Bash",
      "input": { "command": "ls" },
      "output": "file1\nfile2"
    }
  ],
  "usage": {
    "input_tokens": 123,
    "output_tokens": 456
  }
}
```

**Stream-JSON (NDJSON):**
```json
{"type":"message_start","message":{"id":"msg_123"}}
{"type":"content_block_delta","delta":{"type":"text_delta","text":"Hello"}}
{"type":"message_stop"}
```

#### `--json-schema <schema>`

**Type:** JSON string
**Description:** Enforce structured output schema validation
**Requires:** `--output-format json`

```bash
claude -p "List 3 colors" \
  --output-format json \
  --json-schema '{"type":"object","properties":{"colors":{"type":"array"}}}'
```

#### `--include-partial-messages`

**Type:** Boolean flag
**Description:** Include partial message chunks in stream-json output
**Requires:** `--output-format stream-json`

```bash
claude -p "Explain AI" \
  --output-format stream-json \
  --include-partial-messages
```

#### `--input-format <format>`

**Type:** Enum: `"text"` | `"stream-json"`
**Default:** `"text"`
**Description:** Input format for prompts
**Use Case:** Streaming bidirectional communication

```bash
echo '{"prompt":"Hello"}' | \
  claude --input-format stream-json \
        --output-format stream-json
```

#### `--replay-user-messages`

**Type:** Boolean flag
**Description:** Re-emit user messages from stdin to stdout
**Requires:** Both `--input-format stream-json` AND `--output-format stream-json`

```bash
# Echo user messages for acknowledgment in streaming mode
claude --input-format stream-json \
       --output-format stream-json \
       --replay-user-messages
```

---

### Tool Control

#### `--allowedTools, --allowed-tools <tools...>`

**Type:** Space or comma-separated list
**Description:** Auto-approve tool patterns (bypasses permission prompts)
**Pattern Syntax:** `ToolName(pattern:*)`

```bash
# Single tool with pattern
claude -p "List files" --allowedTools "Bash(ls:*)"

# Multiple tools
claude -p "Search and edit" --allowedTools "Grep Read Edit"

# Multiple patterns
claude -p "Git operations" --allowedTools "Bash(git:*) Bash(gh:*)"
```

**Available Tools:**
- `Bash(pattern)` - Shell commands with glob pattern
- `Read` - File reading
- `Write` - File writing
- `Edit` - File editing
- `Glob` - File pattern matching
- `Grep` - Content search
- `WebSearch` - Web searching
- `WebFetch` - URL fetching
- `Agent` - Sub-agent spawning
- `TodoWrite` - Task tracking
- `NotebookEdit` - Jupyter editing
- `BashOutput` - Background shell output
- `KillShell` - Kill background shells

#### `--tools <tools...>`

**Type:** Space-separated list or empty string
**Description:** Specify available tools from built-in set
**Special Values:**
- `""` - Disable all tools
- `"default"` - Use all tools

```bash
# Disable all tools (pure text generation)
claude -p "Explain sorting" --tools ""

# Specific tools only
claude -p "Search code" --tools "Bash Grep Read"
```

#### `--disallowedTools, --disallowed-tools <tools...>`

**Type:** Space or comma-separated list
**Description:** Deny specific tool patterns

```bash
# Prevent destructive operations
claude -p "Clean up project" \
  --disallowedTools "Bash(rm:*) Bash(git push:*)"
```

---

### Session Management

#### `--session-id <uuid>`

**Type:** UUID string
**Description:** Use specific session ID for conversation
**Format:** Must be valid UUID (use `uuidgen` or `randomUUID()`)

```bash
# Generate UUID
SESSION_ID=$(uuidgen)

# Use in multiple calls
claude -p "Step 1" --session-id "$SESSION_ID"
claude -p "Step 2" --session-id "$SESSION_ID"
```

#### `-c, --continue`

**Type:** Boolean flag
**Description:** Continue most recent conversation

```bash
claude --continue
```

#### `-r, --resume [value]`

**Type:** Optional string (session ID or search term)
**Description:** Resume conversation by session ID or interactive picker

```bash
# Interactive picker
claude --resume

# Search by keyword
claude --resume "project-name"

# Specific session
claude --resume "uuid-here"
```

#### `--fork-session`

**Type:** Boolean flag
**Description:** Create new session ID when resuming (branch conversation)
**Requires:** `--resume` or `--continue`

```bash
# Fork from most recent
claude --continue --fork-session

# Fork from specific session
claude --resume "uuid" --fork-session
```

---

### Model Selection

#### `--model <model>`

**Type:** String (alias or full name)
**Description:** Model for current session

**Aliases:**
- `sonnet` → `claude-sonnet-4-5-20250929` (default)
- `opus` → `claude-opus-4-5-20251101`
- `haiku` → Latest Haiku model

```bash
# Use alias
claude -p "Complex task" --model opus

# Full model name
claude -p "Quick task" --model claude-haiku-3-5-20241022
```

#### `--fallback-model <model>`

**Type:** String
**Description:** Automatic fallback when default model is overloaded
**Requires:** `--print` mode

```bash
claude -p "Task" --model sonnet --fallback-model haiku
```

---

### Agent System

#### `--agent <agent>`

**Type:** String (agent name)
**Description:** Specify agent for current session
**Overrides:** `agent` config setting

```bash
claude -p "Review code" --agent code-reviewer
```

#### `--agents <json>`

**Type:** JSON object string
**Description:** Define custom agents programmatically

```json
{
  "reviewer": {
    "description": "Code reviewer",
    "prompt": "You are an expert code reviewer..."
  }
}
```

```bash
claude --agents '{"custom":{"description":"My agent","prompt":"..."}}' \
       --agent custom \
       -p "Task"
```

---

### MCP (Model Context Protocol)

#### `--mcp-config <configs...>`

**Type:** Space-separated JSON files or strings
**Description:** Load MCP servers from configuration

```bash
# From file
claude --mcp-config ./mcp-config.json

# Inline JSON
claude --mcp-config '{"servers":{"my-server":{"command":"node","args":["server.js"]}}}'
```

#### `--strict-mcp-config`

**Type:** Boolean flag
**Description:** Only use MCP servers from `--mcp-config`, ignore other configs

```bash
claude --mcp-config ./prod-mcp.json --strict-mcp-config
```

---

### Security & Permissions

#### `--dangerously-skip-permissions`

**Type:** Boolean flag
**Description:** Bypass ALL permission checks
**Use Case:** Sandboxed environments with no internet
**Warning:** Use with extreme caution

```bash
# Only in isolated sandboxes
claude -p "Task" --dangerously-skip-permissions
```

#### `--allow-dangerously-skip-permissions`

**Type:** Boolean flag
**Description:** Enable skip permissions as option without default activation

```bash
claude --allow-dangerously-skip-permissions
```

#### `--permission-mode <mode>`

**Type:** Enum: `"acceptEdits"` | `"bypassPermissions"` | `"default"` | `"dontAsk"` | `"plan"`
**Description:** Permission behavior for session

```bash
# Auto-accept all edits
claude --permission-mode acceptEdits

# Plan mode (no execution)
claude --permission-mode plan
```

---

### Configuration & Settings

#### `--settings <file-or-json>`

**Type:** File path or JSON string
**Description:** Load additional settings

```bash
# From file
claude --settings ./custom-settings.json

# Inline JSON
claude --settings '{"timeout":60000}'
```

#### `--setting-sources <sources>`

**Type:** Comma-separated: `"user"` | `"project"` | `"local"`
**Description:** Specify which setting sources to load

```bash
# Only project settings
claude --setting-sources project

# User and project, not local
claude --setting-sources user,project
```

#### `--add-dir <directories...>`

**Type:** Space-separated directory paths
**Description:** Additional directories for tool access

```bash
claude --add-dir /extra/path /another/path
```

#### `--system-prompt <prompt>`

**Type:** String
**Description:** Replace default system prompt

```bash
claude --system-prompt "You are a Python expert. Be concise."
```

#### `--append-system-prompt <prompt>`

**Type:** String
**Description:** Append to default system prompt

```bash
claude --append-system-prompt "Always output valid JSON."
```

---

### Development & Debugging

#### `-d, --debug [filter]`

**Type:** Optional category filter string
**Description:** Enable debug mode with optional filtering

```bash
# All debug output
claude --debug

# Specific categories
claude --debug "api,hooks"

# Exclude categories
claude --debug "!statsig,!file"
```

#### `--verbose`

**Type:** Boolean flag
**Description:** Override verbose mode from config

```bash
claude --verbose -p "Task"
```

---

### Plugin System

#### `--plugin-dir <paths...>`

**Type:** Space-separated directory paths
**Description:** Load plugins for current session only

```bash
claude --plugin-dir ./plugins ./more-plugins
```

---

### IDE Integration

#### `--ide`

**Type:** Boolean flag
**Description:** Auto-connect to IDE if exactly one valid IDE available

```bash
claude --ide
```

---

### Version & Help

#### `-v, --version`

**Type:** Boolean flag
**Description:** Output version number

```bash
claude --version
# @anthropic-ai/claude-code/2.0.59
```

#### `-h, --help`

**Type:** Boolean flag
**Description:** Display help

```bash
claude --help
```

---

## Tool Input Schemas (SDK)

### TypeScript Definitions

**Location:** `/opt/node22/lib/node_modules/@anthropic-ai/claude-code/sdk-tools.d.ts`

### Available Tool Schemas

```typescript
export type ToolInputSchemas =
  | AgentInput
  | BashInput
  | BashOutputInput
  | ExitPlanModeInput
  | FileEditInput
  | FileReadInput
  | FileWriteInput
  | GlobInput
  | GrepInput
  | KillShellInput
  | ListMcpResourcesInput
  | McpInput
  | NotebookEditInput
  | ReadMcpResourceInput
  | TodoWriteInput
  | WebFetchInput
  | WebSearchInput
  | AskUserQuestionInput;
```

### Example: BashInput

```typescript
export interface BashInput {
  command: string;
  timeout?: number; // max 600000ms (10 minutes)
  description?: string;
  run_in_background?: boolean;
  dangerouslyDisableSandbox?: boolean;
}
```

### Example: FileEditInput

```typescript
export interface FileEditInput {
  file_path: string; // absolute path
  old_string: string;
  new_string: string;
  replace_all?: boolean;
}
```

### Example: AgentInput

```typescript
export interface AgentInput {
  description: string; // 3-5 words
  prompt: string;
  subagent_type: string;
  model?: "sonnet" | "opus" | "haiku";
  resume?: string; // agent ID to resume
}
```

---

## Exit Codes

| Code | Meaning |
|------|---------|
| `0` | Success |
| `1` | General error (model, API, parsing) |
| `2+` | TBD (research needed) |

**Transient Errors (Retry Recommended):**
- Model overloaded
- Rate limit exceeded
- Network errors

**Permanent Errors (Don't Retry):**
- Authentication failed
- Invalid arguments
- Permission denied

---

## Environment Variables

| Variable | Purpose |
|----------|---------|
| `ANTHROPIC_API_KEY` | API authentication |
| `CLAUDE_*` | Various settings (TBD - research needed) |

---

## Commands

### `claude mcp`

Manage MCP servers

```bash
claude mcp list
claude mcp add <server>
claude mcp remove <server>
```

### `claude plugin`

Manage plugins

```bash
claude plugin list
claude plugin install <plugin>
```

### `claude setup-token`

Set up long-lived auth token (requires Claude subscription)

```bash
claude setup-token
```

### `claude doctor`

Check auto-updater health

```bash
claude doctor
```

### `claude update`

Check and install updates

```bash
claude update
```

### `claude install [target]`

Install specific version

```bash
# Stable version
claude install stable

# Latest version
claude install latest

# Specific version
claude install 2.0.59
```

---

## Programmatic Usage Patterns

### Node.js Child Process

```javascript
import { spawn } from 'child_process';

function execute(prompt, options = {}) {
  return new Promise((resolve, reject) => {
    const args = ['-p', prompt, '--output-format', 'json'];

    const proc = spawn('claude', args);
    let stdout = '';

    proc.stdout.on('data', d => stdout += d);
    proc.on('close', code => {
      if (code === 0) {
        resolve(JSON.parse(stdout));
      } else {
        reject(new Error(`Exit code ${code}`));
      }
    });
  });
}
```

### Bash Pipeline

```bash
#!/bin/bash

# Generate and execute code
claude -p "Write hello world in Python" \
  --output-format json \
  --tools "" \
  | jq -r '.content' \
  > script.py

python script.py
```

### Streaming Parser

```javascript
import readline from 'readline';

const proc = spawn('claude', [
  '-p', prompt,
  '--output-format', 'stream-json'
]);

const rl = readline.createInterface({ input: proc.stdout });

rl.on('line', (line) => {
  const event = JSON.parse(line);
  if (event.delta?.text) {
    process.stdout.write(event.delta.text);
  }
});
```

---

## Best Practices

### 1. Always Specify Output Format

```bash
# Good
claude -p "Task" --output-format json

# Bad (unpredictable parsing)
claude -p "Task"
```

### 2. Use Tool Allowlisting in Automation

```bash
# Good (explicit, secure)
claude -p "Task" --allowedTools "Bash(ls:*) Read"

# Bad (will hang waiting for approval)
claude -p "Task"
```

### 3. Set Timeouts

```javascript
// Good
spawn('claude', args, { timeout: 30000 });

// Bad (can hang indefinitely)
spawn('claude', args);
```

### 4. Validate JSON Output

```javascript
try {
  const result = JSON.parse(stdout);
  if (!result.content) {
    throw new Error('Missing content');
  }
} catch (e) {
  // Handle parse errors
}
```

### 5. Use Session IDs for Multi-Step Workflows

```bash
SESSION=$(uuidgen)
claude -p "Step 1" --session-id "$SESSION"
claude -p "Step 2" --session-id "$SESSION"
```

---

## Limitations

1. **No Native SDK** - All access via CLI child processes
2. **No Real-Time Bidirectional Streaming** - Stream-json is unidirectional
3. **Session Storage** - Sessions stored in `~/.claude/sessions/` (disk usage)
4. **Rate Limiting** - Subject to Anthropic API rate limits
5. **Memory Usage** - ~50-100MB per process

---

## Research Gaps

- Exact exit code meanings beyond 0 and 1
- Session persistence duration
- Environment variable complete list
- Rate limit specifics
- Concurrent process limits

---

**Document Status:** Complete
**Verification:** All flags tested with Claude Code 2.0.59
**Source:** `claude --help` + empirical testing
