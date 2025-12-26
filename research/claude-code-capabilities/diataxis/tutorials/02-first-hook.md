# Tutorial: Your First Hook

Learn how to create hooks that govern tool usage in Claude Code.

## Prerequisites
- Claude Code CLI installed
- Access to `.claude/settings.json`

## What You'll Learn
- Hook lifecycle events
- Creating hook configurations
- Allow/deny/ask patterns

## Step 1: Understand Hook Events

Hooks fire at specific lifecycle points:

```
┌────────────────┬───────────────────────────────────────┐
│ Event          │ When It Fires                         │
├────────────────┼───────────────────────────────────────┤
│ PreToolUse     │ Before any tool executes              │
│ PostToolUse    │ After tool completes                  │
│ PreCompact     │ Before context compaction             │
│ Stop           │ When session ends                     │
└────────────────┴───────────────────────────────────────┘
```

## Step 2: Create Your Settings File

Create or edit `.claude/settings.json`:

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "echo 'About to run bash command'"
          }
        ]
      }
    ]
  }
}
```

## Step 3: Add a Safety Hook

Block dangerous commands:

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command' | grep -q 'rm -rf /' && exit 1 || exit 0"
          }
        ]
      }
    ]
  }
}
```

The hook:
1. Receives tool input as JSON on stdin
2. Extracts the command using jq
3. Checks for dangerous pattern
4. `exit 1` blocks the tool, `exit 0` allows it

## Step 4: Test Your Hook

1. Save the settings file
2. In Claude Code, try: "Run `echo hello`"
3. Observe the hook firing in terminal
4. Try a blocked pattern to verify protection

## Step 5: Add Logging Hook

Track all tool usage:

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "*",
        "hooks": [
          {
            "type": "command",
            "command": "cat >> ~/.claude/tool-usage.log"
          }
        ]
      }
    ]
  }
}
```

## Common Patterns

### Allow List
```json
{
  "permissions": {
    "allow": [
      "Bash(git:*)",
      "Bash(npm:*)",
      "Read",
      "Write"
    ]
  }
}
```

### Validation Hook
```json
{
  "matcher": "Write",
  "hooks": [{
    "type": "command",
    "command": "cat | jq -r '.tool_input.file_path' | grep -E '\\.(env|secret)' && exit 1 || exit 0"
  }]
}
```

## Try It Yourself

1. Create `.claude/settings.json` with a logging hook
2. Run several commands in Claude Code
3. Check `~/.claude/tool-usage.log` for entries

## Next Steps
- [Tutorial: Policy Enforcement](./policy-enforcement.md)
- [Reference: Hook Configuration](../reference/hook-lifecycle.md)
- [How-to: Create Safety Guardrails](../how-to/policy-enforcement.md)

## Key Takeaways

1. **Hooks** intercept tool lifecycle events
2. **Matchers** select which tools to intercept
3. **Exit codes** determine allow (0) or deny (non-0)
4. **stdin** provides tool input as JSON
