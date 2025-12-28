# Agent Template

Use this template when creating new agent definitions in `.claude/agents/`.

## Template Structure

```markdown
---
name: agent-name
description: Brief description of agent purpose
tools:
  - Tool1
  - Tool2
  - Bash
  - Read
  - Write
  - Edit
  - Glob
  - Grep
---

# Agent Name

[Detailed description of what this agent does and when to use it]

## Capabilities

- Capability 1
- Capability 2
- Capability 3

## Use Cases

- Use case 1
- Use case 2
- Use case 3

## Workflow

1. Step 1
2. Step 2
3. Step 3

## Quality Requirements

- [ ] Requirement 1
- [ ] Requirement 2
- [ ] Requirement 3

## Example Prompts

### Basic Usage
```
[Example prompt for basic usage]
```

### Advanced Usage
```
[Example prompt for advanced usage]
```

## Integration

Works well with:
- agent-1
- agent-2

## Output Format

[Description of expected output format]
```

## Naming Conventions

- Use kebab-case: `my-agent-name.md`
- Group related agents in subdirectories
- Use descriptive names that indicate purpose

## Directory Structure

```
.claude/agents/
├── core/           # Core development agents
├── analysis/       # Code analysis agents
├── testing/        # Testing specialists
├── github/         # GitHub integration
├── consensus/      # Distributed systems
├── hive-mind/      # Swarm coordination
└── templates/      # Agent templates
```

## Best Practices

1. **Specific Purpose**: Each agent should have ONE clear purpose
2. **Quality Bar**: Always include explicit quality requirements
3. **Verification**: Include how to verify agent output
4. **Integration**: Document how agent works with others
5. **Examples**: Provide concrete usage examples
