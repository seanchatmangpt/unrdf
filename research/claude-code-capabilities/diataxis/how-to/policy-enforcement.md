# How-to: Enforce Tool Policies

Create guardrails that prevent dangerous operations.

## Problem

You want to prevent Claude Code from executing dangerous commands or accessing sensitive files.

## Solution

Configure hooks with matchers and policy rules in `.claude/settings.json`.

## Steps

### Step 1: Define Your Policy

Decide what to:
- **Allow**: Operations that should always work
- **Deny**: Operations that should never happen
- **Ask**: Operations that need confirmation

```yaml
policy:
  allow:
    - Safe git commands
    - Read operations
    - Build commands
  deny:
    - Destructive file operations
    - System modifications
    - Credential access
  ask:
    - Write to config files
    - External network calls
```

### Step 2: Configure Permission Rules

Edit `.claude/settings.json`:

```json
{
  "permissions": {
    "allow": [
      "Bash(git status)",
      "Bash(git diff:*)",
      "Bash(git log:*)",
      "Bash(npm run:*)",
      "Read",
      "Glob",
      "Grep"
    ],
    "deny": [
      "Bash(rm -rf *)",
      "Bash(chmod 777 *)",
      "Bash(curl * | bash)"
    ]
  }
}
```

### Step 3: Add Validation Hooks

For complex policies, use hooks:

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "/path/to/validate-command.sh"
          }
        ]
      }
    ]
  }
}
```

### Step 4: Create Validation Script

`validate-command.sh`:
```bash
#!/bin/bash
# Read tool input from stdin
INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command')

# Define blocked patterns
BLOCKED_PATTERNS=(
  "rm -rf /"
  "dd if=/dev"
  "mkfs"
  "> /dev/sda"
  "chmod 777"
  "curl.*|.*bash"
)

for pattern in "${BLOCKED_PATTERNS[@]}"; do
  if echo "$COMMAND" | grep -qE "$pattern"; then
    echo "BLOCKED: Command matches dangerous pattern: $pattern" >&2
    exit 1
  fi
done

exit 0
```

### Step 5: Test Your Policy

```bash
# Should be allowed
claude -p "Run git status"  # ✓

# Should be blocked
claude -p "Run rm -rf /"    # ✗ (blocked by policy)
```

## Policy Patterns

### Safe File Operations
```json
{
  "permissions": {
    "allow": [
      "Write(src/**)",
      "Write(test/**)",
      "Edit(src/**)",
      "Edit(test/**)"
    ],
    "deny": [
      "Write(.env*)",
      "Write(**/secrets/**)",
      "Write(**/.git/**)"
    ]
  }
}
```

### Network Restrictions
```json
{
  "hooks": {
    "PreToolUse": [{
      "matcher": "Bash",
      "hooks": [{
        "type": "command",
        "command": "cat | jq -r '.tool_input.command' | grep -qE 'curl|wget|nc' && exit 1 || exit 0"
      }]
    }]
  }
}
```

### Audit Logging
```json
{
  "hooks": {
    "PostToolUse": [{
      "matcher": "*",
      "hooks": [{
        "type": "command",
        "command": "cat | jq '{tool: .tool_name, input: .tool_input, time: now}' >> ~/.claude/audit.jsonl"
      }]
    }]
  }
}
```

## Measuring Policy Effectiveness

Track:
- Blocked operations count
- False positive rate
- Coverage of attack vectors

```bash
# Count blocked operations
grep "BLOCKED" ~/.claude/audit.log | wc -l

# Review blocked patterns
grep "BLOCKED" ~/.claude/audit.log | sort | uniq -c | sort -rn
```

## Related

- [Tutorial: Your First Hook](../tutorials/02-first-hook.md)
- [Reference: Hook Configuration](../reference/hook-lifecycle.md)
- [Explanation: Policy Design](../explanations/policy-design.md)
