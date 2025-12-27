# Agent 7: Technical Notes - Citty API and UNRDF Integration

## Citty Framework

**Citty** is a lightweight CLI framework optimized for defining nested command trees.

- **Repository**: github.com/unjs/citty
- **Version Used**: ^0.1.6
- **License**: MIT
- **Purpose**: Building command-line interfaces with noun-verb structure

### Core API

#### `defineCommand(definition: Object)`

Creates a command definition object.

**Definition Structure:**

```javascript
{
  meta: {
    name: string,              // Command name
    description: string,       // Short description
    version?: string,          // Command version (optional)
  },
  args: {
    [argName]: {
      type: string,            // 'positional', 'string', 'boolean'
      description: string,     // Help text
      required?: boolean,      // Required for positional args
      default?: any,           // Default value
      alias?: string,          // Short alias (e.g., 'f' for --format)
    },
    // ... more args
  },
  subcommands?: {
    [nounName]: defineCommand({...}),
    // ... nested commands
  },
  run?: async (ctx: Context) => void,  // Handler function
}
```

#### `runMain(command: Object | {command, args})`

Executes the command tree.

**Usage Patterns:**

```javascript
// Simple: just pass the command
runMain(main);

// With options: pass command and args
runMain({
  command: main,
  args: process.argv.slice(2), // or custom args
});
```

### Context Object (in run handler)

Passed to the `run(ctx)` handler:

```javascript
{
  args: {
    [argName]: value,
    // All arguments parsed and validated
  },
  rawArgs: string[],           // Raw unparsed arguments (optional)
  // Other properties depend on citty version
}
```

### Argument Types

| Type         | Examples                  | Notes                                  |
| ------------ | ------------------------- | -------------------------------------- |
| `positional` | `node cli.mjs file.txt`   | First unnamed arg, required by default |
| `string`     | `--format json` `-f json` | Named option with string value         |
| `boolean`    | `--strict` `--no-color`   | Boolean flag, no value needed          |

### Subcommands

Nested command trees create the noun-verb pattern:

```javascript
defineCommand({
  meta: { name: 'rdf' },
  subcommands: {
    load: defineCommand({...}),
    query: defineCommand({...}),
  }
})
```

**Accessing subcommands:**

- `cli rdf --help` - Shows verbs under rdf noun
- `cli rdf load --help` - Shows help for rdf load
- `cli rdf load file.ttl` - Executes rdf load with file

## UNRDF CLI Integration

### File Locations

**Package Locations:**

- `/home/user/unrdf/packages/cli/` - Main UNRDF CLI (@unrdf/cli)
- `/home/user/unrdf/packages/kgc-cli/` - KGC extension registry CLI (@unrdf/kgc-cli)

**Source Files:**

- `/home/user/unrdf/packages/cli/src/cli.mjs` - Entry point
- `/home/user/unrdf/packages/cli/src/cli/main.mjs` - Main command definition
- `/home/user/unrdf/packages/cli/src/commands/` - Command implementations

**Commands Structure:**

```
src/cli/
├── main.mjs                   # Root command definition
└── commands/
    ├── graph.mjs              # 'graph' noun and subcommands
    ├── query.mjs              # 'query' noun
    ├── context.mjs            # 'context' noun
    ├── convert.mjs            # 'convert' noun
    └── graph/                 # Nested graph commands
        ├── create.mjs
        ├── list.mjs
        ├── get.mjs
        ├── delete.mjs
        ├── update.mjs
        ├── describe.mjs
        ├── export.mjs
        └── validate.mjs
```

### Example: Graph Create Command

**File:** `/home/user/unrdf/packages/cli/src/commands/graph/create.mjs`

**Pattern:**

1. Define argument schema with zod validation
2. Export a `createCommand` with defineCommand()
3. In run handler, validate args, execute logic, output results

```javascript
import { defineCommand } from 'citty';
import { z } from 'zod';

// Schema for argument validation
const createArgsSchema = z.object({
  name: z.string(),
  'base-iri': z.string().optional().default('http://example.org/'),
});

export const createCommand = defineCommand({
  meta: {
    name: 'create',
    description: 'Create a new RDF named graph',
  },
  args: {
    name: {
      type: 'positional',
      description: 'Name of the graph to create',
      required: true,
    },
    'base-iri': {
      type: 'string',
      description: 'Base IRI for the graph',
      default: 'http://example.org/',
      alias: 'b',
    },
  },
  async run(ctx) {
    const args = createArgsSchema.parse(ctx.args);
    // ... implementation
  },
});
```

### KGC CLI Pattern

**File:** `/home/user/unrdf/packages/kgc-cli/src/cli.mjs`

**Key Features:**

1. **Extension Registry** - Loads noun/verb definitions from manifest
2. **Deterministic Loading** - All packages in order
3. **JSON Envelope Output** - Structured success/error responses
4. **Contract Validation** - Validates all commands before running

**Pattern:**

```javascript
import { defineCommand, runMain } from 'citty';
import { Registry } from './lib/registry.mjs';
import { loadManifest } from './manifest/extensions.mjs';

async function main() {
  const registry = new Registry();
  await loadManifest(registry); // Load all extensions

  const tree = registry.buildCommandTree(); // Build citty tree
  const command = buildCittyTree(registry, tree);

  await runMain({
    command,
    args: process.argv.slice(2),
  });
}
```

### Main CLI Pattern

**File:** `/home/user/unrdf/packages/cli/src/cli/main.mjs`

**Pattern:**

```javascript
import { defineCommand, runMain } from 'citty';
import { graphCommand } from './commands/graph.mjs';
import { queryCommand } from './commands/query.mjs';

const main = defineCommand({
  meta: {
    name: 'unrdf',
    version: '5.0.0',
    description: 'UNRDF CLI - Command-line tools for RDF operations',
  },
  subcommands: {
    graph: graphCommand,
    query: queryCommand,
    context: contextCommand,
    // ... more nouns
  },
});

runMain(main);
```

## UNRDF-Specific Patterns

### 1. Noun Grouping

Commands are grouped by domain:

- **graph** - Graph operations (create, list, get, delete, update, describe, export, validate)
- **query** - SPARQL queries
- **context** - Context management
- **convert** - Format conversion
- **kgc** (in kgc-cli) - Extension registry operations

### 2. Error Handling

Consistent pattern across all commands:

```javascript
async run(ctx) {
  try {
    // Validate arguments
    const args = schema.parse(ctx.args);

    // Execute operation
    const result = await operation(args);

    // Output result
    if (ctx.args.json) {
      console.log(JSON.stringify(result));
    } else {
      console.log(formatHumanReadable(result));
    }
  } catch (error) {
    console.error(`❌ Operation failed: ${error.message}`);
    process.exit(1);
  }
}
```

### 3. JSON Output Mode

Commands support `--json` flag for machine-readable output:

```javascript
if (ctx.args.json) {
  console.log(
    JSON.stringify(
      {
        success: true,
        data: result,
      },
      null,
      2
    )
  );
} else {
  // Human-readable output with formatting
}
```

### 4. Argument Aliases

Short aliases for commonly used options:

```javascript
args: {
  'base-iri': {
    type: 'string',
    alias: 'b',  // Enables --base-iri or -b
  },
}
```

### 5. Dry-Run Mode

Many operations support `--dry-run` for preview:

```javascript
args: {
  'dry-run': {
    type: 'boolean',
    alias: 'd',
    default: false,
  },
}
```

## Integration Recommendations

### For Agent 7 Exploration

1. **Extend RDF Commands:**
   - Integrate with @unrdf/core for actual store operations
   - Use @unrdf/streaming for I/O (Agent 2)
   - Connect to @unrdf/knowledge-engine for derivations (Agent 3)
   - Use @unrdf/hooks for policies (Agent 6)

2. **Add New Nouns:**
   - **store** - Store management (create, list, info, backup)
   - **index** - Indexing operations (build, optimize, stats)
   - **federation** - Federated operations (join, sync)
   - **derivation** - Inference and reasoning

3. **Common Command Patterns:**
   - Load data: `noun load <file> [options]`
   - Query: `noun query --<format> <query>`
   - Validate: `noun validate [--shapes file]`
   - Export: `noun export <output> [--format]`

### Publishing as Package

To turn agent-7 into a publishable CLI package:

1. Create `packages/exploration-cli/package.json`
2. Add citty and zod as dependencies
3. Add `bin` entry: `"exploration-cli": "./src/cli.mjs"`
4. Export commands from `src/index.mjs`
5. Publish to npm with `@unrdf/exploration-cli`

**Example package.json:**

```json
{
  "name": "@unrdf/exploration-cli",
  "version": "1.0.0",
  "description": "UNRDF Exploration CLI - Substrate investigation tool",
  "type": "module",
  "bin": {
    "exploration-cli": "./src/cli.mjs"
  },
  "main": "src/index.mjs",
  "exports": {
    ".": "./src/index.mjs",
    "./commands": "./src/commands/index.mjs"
  },
  "dependencies": {
    "citty": "^0.1.6",
    "zod": "^3.23.0"
  }
}
```

## Comparison: Citty vs Alternatives

| Feature               | Citty              | Yargs               | Commander          |
| --------------------- | ------------------ | ------------------- | ------------------ |
| **Nested Commands**   | Native subcommands | Via yargs.command() | Via .command()     |
| **Learning Curve**    | Minimal            | Moderate            | Moderate           |
| **Bundle Size**       | <5KB               | ~30KB               | ~10KB              |
| **Type Safety**       | Manual (with Zod)  | Manual              | Manual             |
| **Use in UNRDF**      | Yes (primary)      | No                  | No                 |
| **Noun-Verb Pattern** | Natural fit        | Requires structure  | Requires structure |

**Why Citty in UNRDF:**

- Lightweight and focused
- Natural noun-verb structure
- Compatible with Node.js ESM (default in @unrdf packages)
- Minimal dependencies align with UNRDF values

## Testing Commands

### Proof Commands

```bash
# Test CLI runs without error
node /home/user/unrdf/exploration/agents/agent-7/index.mjs --help

# Test noun-verb discovery
node /home/user/unrdf/exploration/agents/agent-7/index.mjs rdf --help

# Test specific command help
node /home/user/unrdf/exploration/agents/agent-7/index.mjs rdf load --help

# Test command execution
node /home/user/unrdf/exploration/agents/agent-7/index.mjs rdf load --dry-run

# Test JSON output
node /home/user/unrdf/exploration/agents/agent-7/index.mjs rdf load --json
```

### Expected Outputs

```
# --help output shows all nouns
exploration-cli
  rdf - RDF graph operations
  policy - Policy and enforcement operations

# rdf --help shows all verbs
  load - Load RDF file from disk
  query - Execute SPARQL query
  validate - Validate RDF graph
  canonicalize - Canonicalize RDF graph

# rdf load --help shows full command usage
Options:
  --format, -f - RDF format (ttl, nt, jsonld, rdfxml)
  --base-iri, -b - Base IRI for resolving relative IRIs
  --dry-run, -d - Show what would be loaded without loading
  --json - Output in JSON format
```

## Future Extensions

### Phase 1 (Current)

- Explore CLI framework and patterns
- Document citty API usage
- Demonstrate noun-verb pattern

### Phase 2

- Integrate with real UNRDF store
- Connect I/O and derivation agents
- Add more nouns (store, federation, etc.)

### Phase 3

- Publish as separate CLI package
- Add interactive mode
- Add shell completion (bash/zsh)
- Add remote command execution

### Phase 4

- Desktop GUI wrapper
- Web-based CLI interface
- Multi-command batching
- Workflow recording/replay

## References

- Citty GitHub: https://github.com/unjs/citty
- @unrdf/cli: `/home/user/unrdf/packages/cli/`
- @unrdf/kgc-cli: `/home/user/unrdf/packages/kgc-cli/`
- UNRDF Documentation: `/home/user/unrdf/docs/`
