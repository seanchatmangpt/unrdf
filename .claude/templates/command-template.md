# Slash Command Template

Use this template when creating new slash commands in `.claude/commands/`.

## Template Structure

```markdown
---
description: Brief description shown in command list
arguments:
  - name: arg1
    description: Description of argument 1
    required: true
  - name: arg2
    description: Description of argument 2
    required: false
    default: "default value"
---

# Command Name

## Purpose

[What this command does and why it exists]

## Usage

```
/command-name <arg1> [arg2]
```

## Arguments

| Argument | Required | Default | Description |
|----------|----------|---------|-------------|
| arg1 | Yes | - | Description |
| arg2 | No | "default" | Description |

## Workflow

### Step 1: [Step Name]

[Detailed instructions for step 1]

### Step 2: [Step Name]

[Detailed instructions for step 2]

## Examples

### Example 1: [Scenario]

```
/command-name value1 value2
```

Expected output:
[Description of expected output]

### Example 2: [Scenario]

```
/command-name value1
```

## Integration

Works well with:
- `/related-command-1`
- `/related-command-2`

## Validation

After running this command, verify:
- [ ] Verification 1
- [ ] Verification 2

## Error Handling

| Error | Cause | Solution |
|-------|-------|----------|
| Error 1 | Cause | Solution |
| Error 2 | Cause | Solution |
```

## Using Arguments

Access arguments in the command body using `$ARGUMENTS`:

```markdown
The user provided: $ARGUMENTS

Parse and process the arguments as needed.
```

## File Naming

- Use kebab-case: `my-command.md`
- Match the command name: `/my-command` → `my-command.md`

## Best Practices

1. **Clear Purpose**: State what the command does upfront
2. **Step-by-Step**: Break complex workflows into steps
3. **Examples**: Provide concrete examples
4. **Validation**: Include verification steps
5. **Integration**: Document related commands
