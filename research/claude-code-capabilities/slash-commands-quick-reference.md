# Slash Commands Quick Reference

**Research Agent**: cc-agent-04-slash-commands
**Date**: 2025-12-27

---

## TL;DR

Slash commands are **markdown files** that expand into **prompts**. They live in `.claude/commands/*.md` and use YAML frontmatter + `$variable` substitution.

---

## File Structure

```
.claude/commands/
├── simple.md              # /simple
└── namespace/
    └── command.md         # /namespace/command
```

---

## Minimal Command

```markdown
---
description: What this command does
---

# Command Title

Prompt content that gets expanded when invoked.
```

**Usage**: `/simple`

---

## Command with Arguments

```markdown
---
description: Review code for quality
arguments:
  - name: file
    description: File to review
    required: true
  - name: focus
    description: Focus area
    required: false
    default: 'general'
---

# Review: $file

Focus on $focus aspects.

Steps:
1. Read $file
2. Analyze for $focus
3. Report findings
```

**Usage**: `/review src/auth.js security`

**Expansion**:
```
# Review: src/auth.js

Focus on security aspects.
[rest of prompt...]
```

---

## Frontmatter Schema

| Field | Required | Type | Purpose |
|-------|----------|------|---------|
| `description` | Yes | string | Help text |
| `arguments` | No | array | Argument specs |
| `arguments[].name` | Yes* | string | Variable name |
| `arguments[].description` | Yes* | string | Arg help text |
| `arguments[].required` | No | boolean | Is mandatory |
| `arguments[].default` | No | string | Default value |

*If `arguments` is present

---

## Variable Substitution

- **Syntax**: `$argname` in command body
- **Type**: All arguments are strings
- **Defaults**: Applied if not provided
- **Required**: Command fails if missing

---

## Built-in Commands

- `/help` - Show available commands
- `/clear` - Clear conversation
- `/rewind` - Rewind to checkpoint
- `/model` - Change model
- `/bashes` - Background tasks

---

## Research Commands (NEW)

| Command | Purpose | Args |
|---------|---------|------|
| `/research/explore` | Explore capability | topic, depth? |
| `/research/validate` | Validate claim | claim |
| `/research/measure` | Measure metric | metric, target? |
| `/research/document` | Document finding | finding, category? |
| `/research/synthesize` | Synthesize results | scope? |

**Example**:
```
/research/explore "slash commands" deep
/research/validate "Commands can spawn agents"
/research/measure coverage src/
```

---

## Composition Patterns

### Sequential

```markdown
Execute in order:
1. /command-a
2. /command-b
3. /command-c
```

### Conditional

```markdown
If $condition is "high":
  Execute /critical-workflow
Else:
  Execute /normal-workflow
```

### Agent Delegation

```markdown
Spawn agent:
```javascript
Task("researcher", "Research $topic", "agent-1")
```
```

### Workflow Chain

```markdown
1. /verify-tests
2. /eliminate-muda
3. /acp (git commit)
```

---

## Best Practices

### DO ✅

- Clear, descriptive names
- Comprehensive descriptions
- Defaults for optional args
- Examples in command body
- Namespaces for organization
- Single responsibility
- Error handling
- Document prerequisites

### DON'T ❌

- >500 lines per command
- >7 arguments
- Assume state maintained
- Skip confirmation for destructive ops
- Hardcode values
- Ignore error codes

---

## Command Templates

### Simple Workflow

```markdown
---
description: Simple task
---

# Task Title

1. Step 1
2. Step 2
3. Validate
```

### Parameterized

```markdown
---
description: Analyze target
arguments:
  - name: target
    required: true
---

# Analyze: $target

1. Read $target
2. Analyze
3. Report
```

### Multi-Agent

```markdown
---
description: Orchestrate task
arguments:
  - name: goal
    required: true
---

# Orchestrate: $goal

```javascript
Task("agent-1", "Task 1", "a1")
Task("agent-2", "Task 2", "a2")
```

Wait and synthesize.
```

### Validation Pipeline

```markdown
---
description: Validate target
arguments:
  - name: target
    required: true
---

# Validate: $target

Stage 1: Lint
```bash
timeout 5s npm run lint $target
```

Stage 2: Test
```bash
timeout 10s npm test $target
```

Report: PASS/FAIL
```

---

## Error Handling

### Pre-flight Validation

```markdown
## Pre-flight

BEFORE executing:
1. [ ] Check A
2. [ ] Check B

If ANY fail, ABORT.
```

### Graceful Degradation

```markdown
Try to read $file:

```bash
if [ -f "$file" ]; then
  cat "$file"
else
  echo "ERROR: Not found"
  exit 1
fi
```

If fails, list alternatives.
```

### Rollback

```markdown
Create checkpoint:

```bash
git commit -m "Checkpoint"
CHECKPOINT=$(git rev-parse HEAD)
```

If fails:
```bash
git reset --hard $CHECKPOINT
```
```

---

## Advanced Patterns

### Versioning

```
commands/
├── deploy-v1.md
└── deploy-v2.md  ← current
```

### Aliases

**Main**: `analyze/code-quality.md`
**Alias**: `analyze/quality.md` → references main

### Inheritance

**Base**: `workflows/base-test.md`
**Specific**: `test/unit.md` → references base

### Dynamic Generation

```markdown
Scan modules.
For each, create command file.
```

---

## Performance

- **Command Size**: <500 lines (token budget)
- **Arguments**: ≤7 (usability)
- **Config Files**: For complex params
- **Timeouts**: Always use with Bash commands

---

## Security

- **No built-in permissions**
- **Trust boundary**: File creator
- **Workarounds**:
  - User confirmation
  - Environment gating
  - Validation in prompt

---

## Discovery

- `/help` - List all commands
- File scan - Auto-discovery
- `README.md` - Category docs
- Description - Shown in help

---

## Key Insights

1. **Commands are prompts**, not functions
2. **Frontmatter drives discovery**
3. **Composition via references**, not calls
4. **No type system** (strings only)
5. **Namespace via directories**
6. **Security via trust boundary**

---

## Full Documentation

- **Architecture**: `slash-command-system-architecture.md`
- **Deliverables**: `agent-04-deliverables.md`
- **Commands**: `.claude/commands/research/*.md`

---

## Usage Flow

```
User types: /namespace/command arg1 arg2
           ↓
Claude Code scans: .claude/commands/namespace/command.md
           ↓
Parse YAML frontmatter
           ↓
Substitute: $arg1 → "arg1", $arg2 → "arg2"
           ↓
Expand full markdown as prompt
           ↓
Execute as Claude Code instruction
```

---

## Example Session

```
> /research/explore "hooks" deep

Claude Code expands:
# Research Exploration: hooks
## Depth Level: deep
Execute systematic exploration protocol...
[Full command expansion]

> /research/validate "Hooks can govern tool calls"

Claude Code expands:
# Claim Validation: Hooks can govern tool calls
## Adversarial Protocol
Execute rigorous validation...
[Full command expansion]

> /research/synthesize

Claude Code expands:
# Research Synthesis
## Scope: full-session
Synthesize all findings...
[Full command expansion]
```

---

**Status**: Research commands ready for production use

**Next**: Try `/research/explore "your-topic"` to test the system!
