# Claude Code Hooks Configuration Guide

**Research Agent**: Agent 02 - Hooks & Tool Governance Explorer
**Date**: 2025-12-27
**Status**: ✅ Step-by-step setup instructions

## Quick Start (5 Minutes)

### Step 1: Create Hooks Directory

```bash
mkdir -p .claude/hooks
cd .claude/hooks
```

### Step 2: Create Your First Hook

```bash
cat > safety-check.sh << 'EOF'
#!/bin/bash
INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

if echo "$COMMAND" | grep -qE 'rm -rf /'; then
  echo "BLOCKED: Dangerous command" >&2
  exit 1
fi

exit 0
EOF

chmod +x safety-check.sh
```

### Step 3: Configure in settings.json

```bash
cat > ..settings.json << 'EOF'
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "/bin/bash .claude/hooks/safety-check.sh"
          }
        ]
      }
    ]
  }
}
EOF
```

### Step 4: Test

```bash
# Test that hook works
echo '{"tool_name":"Bash","tool_input":{"command":"rm -rf /"}}' | bash .claude/hooks/safety-check.sh
# Should exit with code 1 and show "BLOCKED" message
```

**Done!** Your first hook is active.

---

## Complete Configuration Structure

### settings.json Schema

```json
{
  "env": {
    // Environment variables for hooks
    "CLAUDE_FLOW_HOOKS_ENABLED": "true",
    "CLAUDE_AUDIT_LOG": "/var/log/claude/audit.jsonl"
  },
  "permissions": {
    // Simple allow/deny lists (evaluated before hooks)
    "allow": [
      "Bash(git status)",
      "Bash(npm run:*)",
      "Read",
      "Glob",
      "Grep"
    ],
    "deny": [
      "Bash(rm -rf /)"
    ]
  },
  "hooks": {
    // PreToolUse: Before tool execution
    "PreToolUse": [
      {
        "matcher": "Bash|Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "bash .claude/hooks/pre-hook.sh"
          }
        ]
      }
    ],
    // PostToolUse: After tool execution
    "PostToolUse": [
      {
        "matcher": "*",
        "hooks": [
          {
            "type": "command",
            "command": "bash .claude/hooks/audit-log.sh"
          }
        ]
      }
    ],
    // PreCompact: Before context compaction
    "PreCompact": [
      {
        "matcher": "auto",
        "hooks": [
          {
            "type": "command",
            "command": "bash .claude/hooks/preserve-context.sh"
          }
        ]
      }
    ],
    // Stop: Session end
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "bash .claude/hooks/cleanup.sh"
          }
        ]
      }
    ]
  },
  "includeCoAuthoredBy": true,
  "statusLine": {
    "type": "command",
    "command": ".claude/statusline-command.sh"
  }
}
```

---

## Hook File Best Practices

### 1. Use Strict Error Handling

```bash
#!/bin/bash
set -euo pipefail  # Exit on error, undefined vars, pipe failures
```

### 2. Read Input Safely

```bash
# Read entire stdin
INPUT=$(cat)

# Extract fields with defaults
TOOL=$(echo "$INPUT" | jq -r '.tool_name // "unknown"')
FILE=$(echo "$INPUT" | jq -r '.tool_input.file_path // "N/A"')
```

### 3. Provide Clear Error Messages

```bash
if [ "$VIOLATION" = true ]; then
  echo "🚫 BLOCKED: [Clear reason]" >&2
  echo "" >&2
  echo "Details: [Specific details]" >&2
  echo "Remediation: [How to fix]" >&2
  exit 1
fi
```

### 4. Exit Codes

```bash
exit 0  # Allow - tool proceeds
exit 1  # Deny - tool blocked
```

### 5. Performance

```bash
# Keep hooks fast (<100ms)
timeout 1s your-expensive-check || echo "Warning: Check timeout"

# Use caching
CACHE_FILE="/tmp/hook-cache.txt"
if [ -f "$CACHE_FILE" ]; then
  # Use cached result
fi
```

### 6. Logging

```bash
# Log to dedicated file
LOG_FILE="$HOME/.claude/hooks.log"
echo "$(date -u +"%Y-%m-%dT%H:%M:%SZ") $TOOL $FILE" >> "$LOG_FILE"
```

---

## Common Configuration Patterns

### Pattern 1: Multiple Hooks on Same Event

**Execute multiple checks in sequence**:

```json
{
  "PreToolUse": [
    {
      "matcher": "Bash",
      "hooks": [
        {"type": "command", "command": "bash .claude/hooks/safety-check.sh"},
        {"type": "command", "command": "bash .claude/hooks/rate-limit.sh"},
        {"type": "command", "command": "bash .claude/hooks/audit-pre.sh"}
      ]
    }
  ]
}
```

**Execution order**: top to bottom
**Behavior**: If ANY hook exits non-zero, tool is blocked

### Pattern 2: Different Hooks for Different Tools

```json
{
  "PreToolUse": [
    {
      "matcher": "Bash",
      "hooks": [
        {"type": "command", "command": "bash .claude/hooks/bash-safety.sh"}
      ]
    },
    {
      "matcher": "Write|Edit",
      "hooks": [
        {"type": "command", "command": "bash .claude/hooks/file-protection.sh"}
      ]
    },
    {
      "matcher": "Read",
      "hooks": [
        {"type": "command", "command": "bash .claude/hooks/read-audit.sh"}
      ]
    }
  ]
}
```

### Pattern 3: Pre + Post Hook Pair

**Use case**: Measure operation duration

```json
{
  "PreToolUse": [
    {
      "matcher": "*",
      "hooks": [
        {"type": "command", "command": "bash .claude/hooks/timer-start.sh"}
      ]
    }
  ],
  "PostToolUse": [
    {
      "matcher": "*",
      "hooks": [
        {"type": "command", "command": "bash .claude/hooks/timer-end.sh"}
      ]
    }
  ]
}
```

**Implementation**:

```bash
# timer-start.sh
echo "$(date +%s)" > /tmp/claude-timer-$$.txt

# timer-end.sh
START=$(cat /tmp/claude-timer-$$.txt)
END=$(date +%s)
DURATION=$((END - START))
echo "⏱️  Operation took ${DURATION}s"
rm /tmp/claude-timer-$$.txt
```

### Pattern 4: Conditional Hook Execution

**Use case**: Only audit in production environment

```bash
#!/bin/bash
# Only run audit in production
if [ "$ENV" != "production" ]; then
  exit 0  # Skip hook
fi

# ... audit logic ...
```

```json
{
  "env": {
    "ENV": "production"
  },
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "*",
        "hooks": [
          {"type": "command", "command": "bash .claude/hooks/prod-audit.sh"}
        ]
      }
    ]
  }
}
```

---

## Environment Variables for Hooks

### Pass Configuration to Hooks

```json
{
  "env": {
    "CLAUDE_AUDIT_LOG": "/var/log/claude/audit.jsonl",
    "CLAUDE_MAX_OPS": "60",
    "CLAUDE_PROTECTED_DIRS": "/etc:/var:/usr",
    "CLAUDE_ALLOW_DESTRUCTIVE": "false"
  }
}
```

**Access in hook**:

```bash
#!/bin/bash
MAX_OPS="${CLAUDE_MAX_OPS:-60}"  # Default to 60 if not set
AUDIT_LOG="${CLAUDE_AUDIT_LOG:-$HOME/.claude/audit.jsonl}"
```

---

## Debugging Hooks

### Enable Debug Output

```bash
#!/bin/bash
set -x  # Print all commands

INPUT=$(cat)
echo "DEBUG: Received input: $INPUT" >&2
```

### Test Hook Manually

```bash
# Create test input
cat > test-input.json << 'EOF'
{
  "tool_name": "Bash",
  "tool_input": {
    "command": "npm test"
  }
}
EOF

# Run hook
cat test-input.json | bash .claude/hooks/your-hook.sh

# Check exit code
echo $?  # 0 = allow, 1 = deny
```

### Common Issues

#### Issue 1: Hook Not Executing

**Symptom**: No hook output, tool executes normally

**Fixes**:
```bash
# 1. Check file permissions
chmod +x .claude/hooks/*.sh

# 2. Verify path in settings.json
cat .claude/settings.json | jq '.hooks'

# 3. Test hook manually (see above)

# 4. Check Claude Code logs
tail -f ~/.claude/logs/hooks.log
```

#### Issue 2: Hook Always Blocks

**Symptom**: All operations blocked

**Fixes**:
```bash
# Check exit code in hook
echo "Exit code: $?" >&2  # Should be 0 for allow

# Verify conditional logic
set -x  # Debug mode
```

#### Issue 3: jq Not Found

**Symptom**: `jq: command not found`

**Fix**:
```bash
# Install jq
# Ubuntu/Debian:
sudo apt-get install jq

# macOS:
brew install jq

# Or use alternative:
python3 -c 'import json, sys; print(json.load(sys.stdin)["tool_name"])'
```

---

## Performance Tuning

### Measure Hook Performance

```bash
#!/bin/bash
START=$(date +%s%N)

# ... your hook logic ...

END=$(date +%s%N)
DURATION=$(( (END - START) / 1000000 ))  # Convert to ms
echo "Hook executed in ${DURATION}ms" >&2
```

### Optimization Tips

1. **Cache expensive checks**:
```bash
CACHE="/tmp/hook-cache-$USER.txt"
if [ -f "$CACHE" ] && [ $(($(date +%s) - $(stat -c %Y "$CACHE"))) -lt 300 ]; then
  # Use cache (< 5 min old)
  cat "$CACHE"
  exit 0
fi
```

2. **Parallel execution** (use with caution):
```bash
# Run checks in parallel
check1.sh &
check2.sh &
wait  # Wait for both to complete
```

3. **Early exit**:
```bash
# Check fast patterns first
if echo "$FILE" | grep -qE '\.txt$'; then
  exit 0  # Skip expensive checks for .txt files
fi
```

---

## Migration Guide

### From No Hooks → Basic Hooks

**Step 1**: Start with audit-only (PostToolUse)

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "*",
        "hooks": [
          {"type": "command", "command": "bash .claude/hooks/audit.sh"}
        ]
      }
    ]
  }
}
```

**Step 2**: Add safety checks (PreToolUse)

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {"type": "command", "command": "bash .claude/hooks/safety.sh"}
        ]
      }
    ]
  }
}
```

**Step 3**: Add file protection

**Step 4**: Add rate limiting

### From Basic → Advanced Hooks

- Add GDPR compliance logging
- Implement custom policy engines
- Integrate with external systems (SIEM, Slack)
- Create domain-specific hooks

---

## Security Checklist

- [ ] Hooks validate input (use `jq` or safe parsing)
- [ ] Hooks use absolute paths (prevent PATH injection)
- [ ] Hooks don't execute user-provided input directly
- [ ] Hooks have reasonable timeouts (<5s)
- [ ] Hook files have restrictive permissions (chmod 700)
- [ ] Sensitive data not logged to disk
- [ ] Hooks fail-safe (fail-open for availability vs. fail-closed for security)

---

## Production Deployment

### Pre-Production Checklist

- [ ] All hooks tested in isolation
- [ ] Hook combinations tested
- [ ] Performance benchmarked (<100ms total overhead)
- [ ] Error handling tested (malformed input, missing files)
- [ ] Security review completed
- [ ] Documentation updated
- [ ] Rollback plan prepared

### Rollback Plan

```bash
# 1. Backup current settings
cp .claude/settings.json .claude/settings.json.backup

# 2. Disable specific hook
# Edit settings.json, remove hook entry

# 3. Disable all hooks
rm .claude/settings.json
# Or set: "CLAUDE_FLOW_HOOKS_ENABLED": "false"

# 4. Test without hooks
claude -p "Run test command"
```

---

## Next Steps

- [06-security-guide.md](./06-security-guide.md) - Security hardening
- [04-working-examples.md](./04-working-examples.md) - Copy-paste examples
- [README.md](./README.md) - Research summary
