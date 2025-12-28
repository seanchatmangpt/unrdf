# Claude Code Hook Lifecycle Reference

**Research Agent**: Agent 02 - Hooks & Tool Governance Explorer
**Date**: 2025-12-27
**Status**: ✅ Verified through codebase analysis and live configuration

## Executive Summary

Claude Code provides a comprehensive hook system for tool governance, policy enforcement, and workflow automation. Hooks intercept tool lifecycle events, enabling validation, auditing, and dynamic control over AI agent operations.

## Hook Event Types

### 1. PreToolUse Hook

**Trigger**: Before any tool executes
**Can Block**: ✅ Yes (exit code 1)
**Can Modify**: ⚠️ Limited (via rejection + alternative suggestion)
**Use Cases**: Validation, authorization, safety checks, resource preparation

**Execution Context**:
```json
{
  "tool_name": "Bash",
  "tool_input": {
    "command": "npm test",
    "timeout": 5000,
    "description": "Run tests"
  },
  "session_id": "abc-123",
  "timestamp": "2025-12-27T10:00:00Z"
}
```

**Hook Response**:
```bash
# Allow (exit 0)
exit 0

# Deny (exit 1)
echo "BLOCKED: Command matches dangerous pattern: rm -rf /" >&2
exit 1
```

**Timing**: Synchronous - blocks tool execution until hook completes

### 2. PostToolUse Hook

**Trigger**: After tool completes (success or failure)
**Can Block**: ❌ No (operation already completed)
**Can Modify**: ❌ No (read-only observation)
**Use Cases**: Auditing, logging, metrics, state updates, notifications

**Execution Context**:
```json
{
  "tool_name": "Write",
  "tool_input": {
    "file_path": "/home/user/project/src/app.js",
    "content": "..."
  },
  "tool_output": {
    "success": true,
    "bytes_written": 1234
  },
  "duration_ms": 45,
  "timestamp": "2025-12-27T10:00:05Z"
}
```

**Hook Response**:
```bash
# Log to audit trail
cat | jq '{tool: .tool_name, file: .tool_input.file_path, time: now}' >> ~/.claude/audit.jsonl

# Always exit 0 (can't block post-execution)
exit 0
```

**Timing**: Asynchronous - does not delay next operation

### 3. PreCompact Hook

**Trigger**: Before context window compaction
**Can Block**: ❌ No (informational only)
**Can Modify**: ✅ Yes (can inject guidance into compaction prompt)
**Use Cases**: Preserve critical context, inject summary instructions, maintain state

**Execution Context**:
```json
{
  "reason": "auto",
  "context_size": 150000,
  "threshold": 100000,
  "custom_instructions": "Preserve agent roster and swarm coordination patterns"
}
```

**Hook Response**:
```bash
# Inject guidance into compaction
echo "🔄 PreCompact Guidance:"
echo "📋 CRITICAL: Preserve the following in compacted context:"
echo "   • Agent roster (54 agents in .claude/agents/)"
echo "   • Active swarm coordination patterns"
echo "   • Current task execution state"
echo "✅ Ready for compact operation"
exit 0
```

**Timing**: Synchronous - runs before compaction process

**Matcher Types**:
- `manual`: User-triggered compaction
- `auto`: Automatic compaction (context window full)

### 4. Stop Hook

**Trigger**: Session end (user exit, timeout, error)
**Can Block**: ❌ No (session already ending)
**Can Modify**: ❌ No (cleanup only)
**Use Cases**: Cleanup, state persistence, metrics export, final logging

**Execution Context**:
```json
{
  "session_id": "abc-123",
  "duration_seconds": 3600,
  "tools_executed": 45,
  "errors": 2,
  "reason": "user_exit"
}
```

**Hook Response**:
```bash
# Persist session state
npx claude-flow@alpha hooks session-end \
  --generate-summary true \
  --persist-state true \
  --export-metrics true

exit 0
```

**Timing**: Synchronous - runs before final shutdown

## Hook Lifecycle Execution Flow

```
┌─────────────────────────────────────────────────────────────┐
│                     TOOL EXECUTION FLOW                     │
└─────────────────────────────────────────────────────────────┘

1. User Request / AI Decision
   ↓
2. Tool Selection (e.g., Bash, Write, Edit)
   ↓
3. ┌──────────────────────┐
   │  PreToolUse Hook(s) │  ← Can BLOCK here (exit 1)
   └──────────────────────┘
   ↓ (if allowed)
4. Tool Parameter Preparation
   ↓
5. ┌──────────────────────┐
   │  Tool Executes       │  ← Actual operation happens
   └──────────────────────┘
   ↓
6. ┌──────────────────────┐
   │  PostToolUse Hook(s) │  ← Audit/log (cannot block)
   └──────────────────────┘
   ↓
7. Response to User/AI

┌─────────────────────────────────────────────────────────────┐
│                   COMPACTION FLOW                           │
└─────────────────────────────────────────────────────────────┘

1. Context Window Full (or Manual Trigger)
   ↓
2. ┌──────────────────────┐
   │  PreCompact Hook(s)  │  ← Inject preservation guidance
   └──────────────────────┘
   ↓
3. Context Compaction Process
   ↓
4. Compacted Context Ready

┌─────────────────────────────────────────────────────────────┐
│                    SESSION END FLOW                         │
└─────────────────────────────────────────────────────────────┘

1. Session Termination Signal
   ↓
2. ┌──────────────────────┐
   │    Stop Hook(s)      │  ← Cleanup & persistence
   └──────────────────────┘
   ↓
3. Session Ends
```

## Hook Execution Order

### Multiple Hooks on Same Event

Hooks execute **sequentially** in configuration order:

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          { "type": "command", "command": "hook1.sh" },  // Executes first
          { "type": "command", "command": "hook2.sh" },  // Executes second
          { "type": "command", "command": "hook3.sh" }   // Executes third
        ]
      }
    ]
  }
}
```

**Behavior**:
- If **any** PreToolUse hook exits non-zero → tool execution BLOCKED
- All PreToolUse hooks must succeed (exit 0) for tool to proceed
- PostToolUse hooks always execute (non-blocking)

## Hook Input/Output Protocol

### Input (stdin)

Hooks receive tool context as JSON on stdin:

```bash
# Hook receives this on stdin:
{
  "tool_name": "Write",
  "tool_input": {
    "file_path": "/home/user/project/app.js",
    "content": "console.log('hello');"
  },
  "session_id": "abc-123"
}
```

**Extract fields using jq**:
```bash
#!/bin/bash
INPUT=$(cat)
TOOL=$(echo "$INPUT" | jq -r '.tool_name')
FILE=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')
```

### Output (stdout/stderr)

**stdout**: Informational messages (logged, shown to user)
**stderr**: Error messages (shown on denial)
**exit code**: Decision (0 = allow, non-0 = deny)

```bash
# Example: Block dangerous command
if echo "$COMMAND" | grep -qE 'rm -rf /'; then
  echo "BLOCKED: Destructive command detected" >&2
  echo "Command: $COMMAND" >&2
  exit 1
fi

echo "Command validated successfully"
exit 0
```

## Hook Performance Constraints

### Timeout Limits

| Hook Type    | Default Timeout | Max Recommended | Action on Timeout |
|--------------|-----------------|-----------------|-------------------|
| PreToolUse   | 5s              | 10s             | Allow (fail-open) |
| PostToolUse  | 5s              | 30s             | Warn + continue   |
| PreCompact   | 2s              | 5s              | Continue          |
| Stop         | 10s             | 60s             | Force exit        |

**Best Practice**: Keep hooks < 1s for responsive UX

### Resource Limits

- **CPU**: Hook should not spike CPU usage
- **Memory**: < 50MB recommended
- **Network**: Avoid synchronous network calls in PreToolUse
- **Disk I/O**: Minimize file operations

## Hook Configuration Schema

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash|Write|Edit",  // Tool name pattern (regex)
        "hooks": [
          {
            "type": "command",           // Always "command"
            "command": "bash script.sh"  // Shell command to execute
          }
        ]
      }
    ],
    "PostToolUse": [ /* same structure */ ],
    "PreCompact": [
      {
        "matcher": "auto|manual",      // Compaction trigger type
        "hooks": [ /* ... */ ]
      }
    ],
    "Stop": [
      {
        "hooks": [ /* no matcher for Stop */ ]
      }
    ]
  }
}
```

## Evidence & Verification

**Configuration Location**: `/home/user/unrdf/.claude/settings.json`
**Live Hooks Found**:
- ✅ PreToolUse: 2 matchers (Bash, Write|Edit|MultiEdit)
- ✅ PostToolUse: 2 matchers (Bash, Write|Edit|MultiEdit)
- ✅ PreCompact: 2 matchers (manual, auto)
- ✅ Stop: 1 matcher (no pattern)

**Test Results**: Configuration verified via direct file analysis

## Next Steps

See companion documents:
- [02-matcher-patterns.md](./02-matcher-patterns.md) - Complete matcher reference
- [03-policy-catalog.md](./03-policy-catalog.md) - Proven policy patterns
- [04-working-examples.md](./04-working-examples.md) - Tested implementations
- [05-configuration-guide.md](./05-configuration-guide.md) - Setup instructions
