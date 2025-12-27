# Tutorial: Your First Slash Command

Learn how to create custom slash commands in Claude Code.

## Prerequisites

- Claude Code CLI installed
- Project directory with `.claude/` folder

## What You'll Learn

- Slash command file structure
- Frontmatter syntax
- Argument passing
- Command invocation

## Step 1: Create Command Directory

```bash
mkdir -p .claude/commands
```

## Step 2: Create a Simple Command

Create `.claude/commands/greet.md`:

```markdown
---
description: Greet the user with a custom message
---

# Greeting Command

Say hello to the user in a friendly way. Introduce yourself and ask how you can help today.
```

## Step 3: Test Your Command

In Claude Code:

```
/greet
```

Claude will expand the command to the prompt content.

## Step 4: Add Arguments

Create `.claude/commands/review.md`:

```markdown
---
description: Review a specific file for code quality
arguments:
  - name: file
    description: The file to review
    required: true
  - name: focus
    description: What to focus on (security, performance, style)
    required: false
    default: general
---

# Code Review: $file

Review the file at `$file` with a focus on $focus aspects.

## Review Checklist

- [ ] Code follows project conventions
- [ ] No obvious bugs or issues
- [ ] Error handling is appropriate
- [ ] Performance is acceptable

Provide specific feedback with line numbers.
```

## Step 5: Use Arguments

```
/review src/auth.js security
```

Arguments are substituted:

- `$file` → `src/auth.js`
- `$focus` → `security`

## Step 6: Create Namespaced Commands

Organize related commands:

```
.claude/commands/
├── research/
│   ├── explore.md      # /research/explore
│   ├── document.md     # /research/document
│   └── validate.md     # /research/validate
└── quality/
    ├── review.md       # /quality/review
    └── test.md         # /quality/test
```

## Complete Example: Test Command

`.claude/commands/test.md`:

```markdown
---
description: Run tests for a specific area
arguments:
  - name: area
    description: Test area (unit, integration, e2e)
    required: true
  - name: pattern
    description: File pattern to match
    required: false
    default: '*'
---

# Run Tests: $area

Execute $area tests matching pattern "$pattern".

## Steps

1. Identify test files matching the pattern
2. Run the appropriate test command
3. Report results with pass/fail counts
4. Highlight any failures with details

Use `npm test` or the project's configured test runner.
```

## Built-in Commands Reference

Common built-in commands:

- `/help` - Show help
- `/clear` - Clear conversation
- `/rewind` - Checkpoint rewind
- `/model` - Change model

## Try It Yourself

1. Create `.claude/commands/hello.md` with a simple greeting
2. Run `/hello` in Claude Code
3. Add an argument and test `/hello YourName`

## Next Steps

- [Tutorial: Command Workflows](./command-workflows.md)
- [Reference: Command Syntax](../reference/slash-commands.md)
- [How-to: Build Workflow Commands](../how-to/command-workflows.md)

## Key Takeaways

1. Commands live in `.claude/commands/*.md`
2. **Frontmatter** defines metadata and arguments
3. **$variables** are substituted from arguments
4. **Namespacing** via subdirectories (e.g., `/dir/command`)
