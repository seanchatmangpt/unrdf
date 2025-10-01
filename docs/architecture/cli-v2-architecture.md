# UNRDF CLI v2.0 - Dark Matter 80/20 Architecture

## Executive Summary

This document defines the architecture for UNRDF CLI v2.0, incorporating **citty-test-utils patterns** and following the **80/20 principle** to deliver maximum value through focused development.

## Design Principles

1. **80/20 Focus**: 20% of commands deliver 80% of value
2. **Citty-First**: Leverage citty for command structure and testing
3. **Modular Design**: Clear separation of concerns
4. **Test-Driven**: citty-test-utils integration for comprehensive testing
5. **Performance**: Meet KGC performance targets

## Core Commands (The 20%)

### Value Distribution

| Command | Value % | Rationale | Priority |
|---------|---------|-----------|----------|
| **hook** | 25% | Core functionality for Knowledge Hooks - reactive intelligence | P0 |
| **query** | 20% | Essential SPARQL operations - data access | P0 |
| **parse** | 15% | RDF data ingestion - entry point | P0 |
| **validate** | 15% | Data quality gates - SHACL validation | P1 |
| **init** | 10% | Project scaffolding - developer onboarding | P1 |
| **store** | 10% | Store management - operational needs | P1 |
| **delta** | 5% | Dataset comparison - change detection | P2 |

**Total Core Value**: 100% from 7 command groups (vs 15+ in v1.0)

## Module Structure

```
src/cli/
├── index.mjs                    # Main CLI entry point
├── commands/
│   ├── hook.mjs                 # 25% value - Knowledge Hooks
│   ├── query.mjs                # 20% value - SPARQL queries
│   ├── parse.mjs                # 15% value - RDF parsing
│   ├── validate.mjs             # 15% value - Validation
│   ├── init.mjs                 # 10% value - Scaffolding
│   ├── store.mjs                # 10% value - Store ops
│   └── delta.mjs                #  5% value - Comparison
├── utils/
│   ├── context.mjs              # CLI context management
│   ├── output.mjs               # Output formatting
│   └── config.mjs               # Configuration loading
└── test-utils/
    └── index.mjs                # CLI testing utilities

test/cli/
├── commands/                     # Unit tests per command
├── integration/                  # Integration tests
├── e2e/                         # End-to-end scenarios
└── performance/                  # Performance validation
```

## Command Architecture

### 1. Hook Commands (25% Value)

**Purpose**: Knowledge Hook management and evaluation

**Subcommands**:
```javascript
hook
├── eval <hook>              # Evaluate hook on data
├── create <name> <type>     # Create from template
├── validate <hook>          # Validate hook definition
├── list                     # List stored hooks
├── save <file>              # Save to storage
├── load <id>                # Load and evaluate
├── delete <id>              # Delete stored hook
├── history <id>             # Show eval history
├── plan <hook>              # Show execution plan
└── stats                    # Show statistics
```

**Module**: `src/cli/commands/hook.mjs`

**Dependencies**:
- `knowledge-hook-manager.mjs`
- `storage-utils.mjs`
- `define-hook.mjs`

**Testing**: citty-test-utils with mock contexts

### 2. Query Commands (20% Value)

**Purpose**: SPARQL query execution

**Subcommands**:
```javascript
query
├── select <query> <data>    # Execute SELECT query
├── ask <query> <data>       # Execute ASK query
├── construct <query> <data> # Execute CONSTRUCT query
└── describe <uri> <data>    # Execute DESCRIBE query
```

**Module**: `src/cli/commands/query.mjs`

**Dependencies**:
- `use-graph.mjs`
- `use-turtle.mjs`

**Output Formats**: json, table, csv, turtle

### 3. Parse Commands (15% Value)

**Purpose**: RDF data ingestion

**Subcommands**:
```javascript
parse
├── turtle <file>            # Parse Turtle
├── nquads <file>            # Parse N-Quads
├── jsonld <file>            # Parse JSON-LD
└── rdfxml <file>            # Parse RDF/XML
```

**Module**: `src/cli/commands/parse.mjs`

**Dependencies**:
- `use-turtle.mjs`
- `use-store-context.mjs`

**Performance Target**: < 500ms for 10k triples

### 4. Validate Commands (15% Value)

**Purpose**: Data quality validation

**Subcommands**:
```javascript
validate
├── shacl <data> <shapes>    # SHACL validation
├── zod <data> <schema>      # Zod validation
└── integrity <data>         # Integrity checks
```

**Module**: `src/cli/commands/validate.mjs`

**Dependencies**:
- `use-validator.mjs`
- `use-zod.mjs`

**Output**: Validation report with violations

### 5. Init Commands (10% Value)

**Purpose**: Project scaffolding

**Subcommands**:
```javascript
init
├── project <name>           # Initialize project
├── hook <name> <type>       # Scaffold hook
├── policy <name>            # Scaffold policy pack
└── config                   # Generate config file
```

**Module**: `src/cli/commands/init.mjs`

**Dependencies**:
- `fs/promises`
- Template files

**Templates**:
- Project template
- Hook templates (sparql-ask, shacl, delta)
- Policy pack template

### 6. Store Commands (10% Value)

**Purpose**: Store management

**Subcommands**:
```javascript
store
├── stats <file>             # Show statistics
├── export <file> <output>   # Export store
├── import <file>            # Import to store
└── clear                    # Clear store
```

**Module**: `src/cli/commands/store.mjs`

**Dependencies**:
- `use-store-context.mjs`

### 7. Delta Commands (5% Value)

**Purpose**: Dataset comparison

**Subcommands**:
```javascript
delta
├── diff <source> <target>   # Compare datasets
└── patch <source> <delta>   # Apply delta
```

**Module**: `src/cli/commands/delta.mjs`

**Dependencies**:
- `use-delta.mjs`

## CLI Context Management

### Context Schema

```javascript
import { z } from 'zod';

export const CLIContextSchema = z.object({
  // Configuration
  baseIRI: z.string().url(),
  prefixes: z.record(z.string()),

  // Store
  store: z.any(), // N3 Store instance

  // Components
  manager: z.any().optional(), // KnowledgeHookManager
  validator: z.any().optional(), // Validator

  // Options
  verbose: z.boolean().default(false),
  quiet: z.boolean().default(false),
  format: z.enum(['json', 'table', 'csv', 'turtle']).default('table'),

  // Performance
  timeout: z.number().default(30000),
  maxConcurrency: z.number().default(10)
});
```

### Context Management

```javascript
// src/cli/utils/context.mjs
import { createContext } from 'unctx';
import { initStore } from '../../index.mjs';

const cliContext = createContext('unrdf-cli');

export async function withCLIContext(config, fn) {
  const runApp = initStore([], { baseIRI: config.baseIRI });
  return await runApp(() => {
    return cliContext.callAsync({
      ...config,
      store: useStoreContext()
    }, fn);
  });
}

export function useCLIContext() {
  return cliContext.use();
}
```

## Testing Strategy

### Test Types

1. **Unit Tests** (commands/\*.test.mjs)
   - Test each command in isolation
   - Mock dependencies
   - Validate input/output
   - Use citty-test-utils

2. **Integration Tests** (integration/\*.test.mjs)
   - Test command composition
   - Real store interactions
   - Multiple commands in sequence
   - Workflow validation

3. **E2E Tests** (e2e/\*.test.mjs)
   - Full CLI execution
   - Real file system
   - Complete workflows
   - Performance validation

4. **Performance Tests** (performance/\*.test.mjs)
   - Validate performance targets
   - Benchmark critical paths
   - Load testing
   - Memory profiling

### citty-test-utils Integration

```javascript
// test/cli/commands/hook.test.mjs
import { describe, it, expect } from 'vitest';
import { runCLI, mockContext, assertOutput } from '../test-utils/index.mjs';

describe('hook command', () => {
  it('should evaluate hook successfully', async () => {
    const ctx = mockContext({
      store: createTestStore(),
      manager: createTestManager()
    });

    const result = await runCLI('hook eval test-hook.json', ctx);

    assertOutput(result, {
      exitCode: 0,
      stdout: /Evaluation Result: FIRED/
    });
  });

  it('should create hook from template', async () => {
    const result = await runCLI('hook create health-check sparql-ask');

    assertOutput(result, {
      exitCode: 0,
      stdout: /Generated hook files/
    });

    // Verify files created
    expect(result.files).toContain('hooks/health-check/health-check.mjs');
  });
});
```

### Test Utilities

```javascript
// src/cli/test-utils/index.mjs
import { execa } from 'execa';

export async function runCLI(command, context = {}) {
  const [cmd, ...args] = command.split(' ');
  const result = await execa('node', ['src/cli.mjs', cmd, ...args], {
    env: { ...process.env, ...context.env },
    cwd: context.cwd || process.cwd(),
    reject: false
  });

  return {
    exitCode: result.exitCode,
    stdout: result.stdout,
    stderr: result.stderr,
    files: context.files || []
  };
}

export function mockContext(overrides = {}) {
  return {
    store: new Store(),
    baseIRI: 'http://example.org/',
    ...overrides
  };
}

export function assertOutput(result, expectations) {
  if (expectations.exitCode !== undefined) {
    expect(result.exitCode).toBe(expectations.exitCode);
  }
  if (expectations.stdout) {
    expect(result.stdout).toMatch(expectations.stdout);
  }
  if (expectations.stderr) {
    expect(result.stderr).toMatch(expectations.stderr);
  }
}
```

## Output Formatting

### Formatters

```javascript
// src/cli/utils/output.mjs
export const formatters = {
  json: (data) => JSON.stringify(data, null, 2),

  table: (data) => {
    // ASCII table formatting
    const headers = Object.keys(data[0]);
    const colWidths = headers.map(h =>
      Math.max(h.length, ...data.map(r => String(r[h] || '').length))
    );

    return [
      headers.map((h, i) => h.padEnd(colWidths[i])).join(' | '),
      headers.map((_, i) => '-'.repeat(colWidths[i])).join('-+-'),
      ...data.map(row =>
        headers.map((h, i) => String(row[h] || '').padEnd(colWidths[i])).join(' | ')
      )
    ].join('\n');
  },

  csv: (data) => {
    const headers = Object.keys(data[0]);
    return [
      headers.join(','),
      ...data.map(row => headers.map(h => row[h] || '').join(','))
    ].join('\n');
  },

  turtle: async (quads) => {
    const turtle = await useTurtle();
    return await turtle.serialize();
  }
};
```

## Performance Targets

| Operation | Target | Rationale |
|-----------|--------|-----------|
| Command startup | < 100ms | Responsive CLI experience |
| Parse 10k triples | < 500ms | Efficient data ingestion |
| Hook evaluation | < 2ms p99 | Meet KGC performance targets |
| SPARQL query (simple) | < 50ms | Interactive query experience |
| Validation (SHACL) | < 200ms | Fast feedback loop |

## Design Decisions

### ADR-001: Use citty for Command Structure

**Decision**: Adopt citty for all command definitions

**Rationale**:
- Type-safe command definitions
- Built-in argument parsing
- Composable command structure
- Excellent testing utilities
- Active maintenance

**Tradeoffs**:
- Dependency on citty framework
- Learning curve for contributors
- Migration effort from current CLI

**Alternatives Considered**:
- yargs: More features, but less type-safe
- commander: Popular, but less composable
- Custom: Full control, but more maintenance

### ADR-002: 80/20 Command Prioritization

**Decision**: Focus on 7 core command groups delivering 80% of value

**Rationale**:
- Concentrated development effort
- Faster time to production
- Higher quality for critical commands
- Clear prioritization

**Tradeoffs**:
- Some commands less polished initially
- May need future expansion
- Requires discipline to maintain focus

### ADR-003: citty-test-utils for CLI Testing

**Decision**: Integrate citty-test-utils for comprehensive CLI testing

**Rationale**:
- Proven patterns for CLI testing
- Programmatic command execution
- Output capture and validation
- Mock context support

**Tradeoffs**:
- Additional test framework
- Learning curve
- Test complexity

### ADR-004: unctx for Context Management

**Decision**: Use unctx for CLI context management

**Rationale**:
- Consistent with existing codebase
- Clean async context handling
- No global state pollution
- Good TypeScript support

**Tradeoffs**:
- Requires careful context lifecycle management
- Debugging can be tricky

### ADR-005: Zod for Input Validation

**Decision**: Validate all command inputs with Zod schemas

**Rationale**:
- Runtime type safety
- Clear error messages
- Schema reuse
- Validation consistency

**Tradeoffs**:
- Additional validation overhead
- Schema maintenance
- Learning curve

## Implementation Roadmap

### Phase 1: Core Commands (Weeks 1-2)

**Deliverables**:
- ✅ hook commands (25% value)
- ✅ query commands (20% value)
- ✅ parse commands (15% value)
- ✅ CLI context management
- ✅ Basic output formatting

**Success Criteria**:
- All P0 commands functional
- Unit tests passing
- Performance targets met
- Documentation complete

### Phase 2: Enhancement Commands (Weeks 3-4)

**Deliverables**:
- ✅ validate commands (15% value)
- ✅ init commands (10% value)
- ✅ store commands (10% value)
- ✅ delta commands (5% value)
- ✅ Advanced output formatting

**Success Criteria**:
- All P1 commands functional
- Integration tests passing
- UX polish complete
- Examples documented

### Phase 3: Testing & Polish (Weeks 5-6)

**Deliverables**:
- ✅ Comprehensive test suite with citty-test-utils
- ✅ Performance optimization
- ✅ Documentation
- ✅ Error handling and UX polish
- ✅ CI/CD integration

**Success Criteria**:
- 90%+ test coverage
- All performance targets met
- Production-ready
- Stakeholder approval

## Migration Strategy

### From v1.0 to v2.0

1. **Parallel Development**: Build v2.0 alongside v1.0
2. **Gradual Migration**: Move commands one at a time
3. **Backward Compatibility**: Maintain v1.0 CLI during transition
4. **User Communication**: Clear migration guide
5. **Deprecation Timeline**: 3 month transition period

### Migration Path

```bash
# Phase 1: Install v2.0 alongside v1.0
npm install unrdf@2.0.0

# Phase 2: Use v2.0 commands
unrdf hook eval ...  # v2.0 command

# Phase 3: Full migration
npm uninstall unrdf@1.0.0
```

## Success Metrics

### Functional Success
- ✅ All core commands (80% value) working
- ✅ All tests passing
- ✅ Documentation complete
- ✅ Examples validated

### Performance Success
- ✅ Startup < 100ms
- ✅ Parse 10k triples < 500ms
- ✅ Hook eval < 2ms p99
- ✅ Query < 50ms
- ✅ Validate < 200ms

### Operational Success
- ✅ CI/CD pipeline green
- ✅ Production deployment ready
- ✅ Monitoring integrated
- ✅ Support procedures defined

## Conclusion

The UNRDF CLI v2.0 architecture delivers maximum value through focused development on the 20% of commands that provide 80% of functionality. By leveraging citty patterns and comprehensive testing with citty-test-utils, we achieve a production-ready, maintainable, and performant CLI that meets all KGC requirements.

The modular design with clear separation of concerns ensures long-term maintainability, while the 80/20 approach enables rapid delivery of high-value features.

**Status**: ✅ **ARCHITECTURE COMPLETE** - Ready for implementation
