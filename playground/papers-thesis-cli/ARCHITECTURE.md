# Papers-Thesis CLI Architecture

## System Overview

This document defines the complete architecture for converting the Rust playground CLI (clap-noun-verb v5.1.0) to a JavaScript/Node.js system using citty, nunjucks, and unrdf.

## 1. Architectural Principles

### 1.1 Design Philosophy

| Principle | Implementation |
|-----------|---------------|
| **Separation of Concerns** | CLI parsing -> Domain logic -> Integration adapters |
| **Dependency Inversion** | Domain depends on abstractions, not concrete implementations |
| **Single Responsibility** | Each module handles one concern |
| **Testability First** | Pure functions in domain, side effects isolated in integration |
| **SPARC Compliance** | Follows Specification-Pseudocode-Architecture-Refinement-Completion |

### 1.2 Layer Architecture

```
+------------------------------------------------------------------+
|                        CLI LAYER (citty)                          |
|  - Command parsing                                                |
|  - Argument validation                                            |
|  - Global flags (--quiet, --format, --output)                    |
|  - Shell completions                                              |
+------------------------------------------------------------------+
                              |
                              v
+------------------------------------------------------------------+
|                      DOMAIN LAYER (pure)                          |
|  - Paper/Thesis models                                            |
|  - Business rules                                                 |
|  - Formatters (json, yaml, table)                                |
|  - Validation logic                                               |
+------------------------------------------------------------------+
                              |
                              v
+------------------------------------------------------------------+
|                    INTEGRATION LAYER                              |
|  - Nunjucks template engine                                       |
|  - UNRDF knowledge graph                                          |
|  - File I/O operations                                            |
|  - SPARQL query execution                                         |
+------------------------------------------------------------------+
```

## 2. Component Architecture

### 2.1 CLI Layer (Citty)

```javascript
// Entry point: src/cli/index.mjs
defineCommand({
  meta: { name: 'playground', version: '1.0.0' },
  args: {
    quiet: { type: 'boolean', alias: 'q', global: true },
    format: { type: 'string', alias: 'f', global: true, default: 'table' },
    output: { type: 'string', alias: 'o', global: true }
  },
  subCommands: {
    papers: papersCommand,
    thesis: thesisCommand,
    config: configCommand,
    meta: metaCommand
  }
});
```

### 2.2 Domain Layer

#### Paper Model

```javascript
// Domain model: src/domain/models/paper.mjs
/**
 * @typedef {Object} Paper
 * @property {string} id - Unique identifier (URI)
 * @property {string} title - Paper title
 * @property {string} family - Paper family (imrad, dsr, argument, etc.)
 * @property {string} abstract - Paper abstract
 * @property {Section[]} sections - Paper sections
 * @property {Author[]} authors - Paper authors
 * @property {Date} createdAt - Creation timestamp
 */
```

#### Thesis Model

```javascript
// Domain model: src/domain/models/thesis.mjs
/**
 * @typedef {Object} Thesis
 * @property {string} id - Unique identifier (URI)
 * @property {string} title - Thesis title
 * @property {ThesisType} type - Type (monograph, narrative, contribution)
 * @property {Schedule} schedule - Defense schedule
 * @property {Chapter[]} chapters - Thesis chapters
 * @property {Author} author - Thesis author
 */
```

### 2.3 Integration Layer

#### Template Engine

```javascript
// Integration: src/integration/templates.mjs
import nunjucks from 'nunjucks';

export function createTemplateEngine(templatesDir) {
  const env = nunjucks.configure(templatesDir, {
    autoescape: false, // LaTeX needs raw output
    trimBlocks: true,
    lstripBlocks: true
  });

  // Custom filters
  env.addFilter('texescape', texEscape);
  env.addFilter('bibtexkey', toBibtexKey);

  return {
    render: (template, context) => env.render(`${template}.tex.njk`, context),
    renderString: (str, context) => env.renderString(str, context)
  };
}
```

#### Knowledge Graph

```javascript
// Integration: src/integration/knowledge-graph.mjs
import { KnowledgeHookManager, defineHook, parseTurtle, query } from 'unrdf/knowledge-engine';

export function createKnowledgeGraph(ontologyPath) {
  const manager = new KnowledgeHookManager();

  return {
    loadOntology: async () => { /* ... */ },
    query: async (sparql) => { /* ... */ },
    addPaper: async (paper) => { /* ... */ },
    addThesis: async (thesis) => { /* ... */ },
    export: async (format) => { /* ... */ }
  };
}
```

## 3. Command Registry

### 3.1 Papers Commands

| Command | Verb | Description | Args |
|---------|------|-------------|------|
| `playground papers generate` | generate | Generate paper from template | `[family]`, `--title`, `--author` |
| `playground papers list` | list | List available paper families | `--verbose` |
| `playground papers validate` | validate | Validate paper structure | `[path]`, `--strict` |

### 3.2 Thesis Commands

| Command | Verb | Description | Args |
|---------|------|-------------|------|
| `playground thesis generate` | generate | Generate thesis structure | `[type]`, `--title`, `--author` |
| `playground thesis list` | list | List thesis types | `--verbose` |
| `playground thesis schedule list` | schedule list | Show schedule | -- |
| `playground thesis schedule set` | schedule set | Configure schedule | `--defense`, `--milestone` |

### 3.3 Config Commands

| Command | Verb | Description | Args |
|---------|------|-------------|------|
| `playground config set` | set | Set configuration | `[key]`, `[value]` |
| `playground config get` | get | Get configuration | `[key]` |
| `playground config list` | list | List all config | -- |
| `playground config reset` | reset | Reset to defaults | `--confirm` |

### 3.4 Meta Commands

| Command | Verb | Description | Args |
|---------|------|-------------|------|
| `playground meta introspect` | introspect | Machine-grade introspection | `--format` |
| `playground meta ontology list` | ontology list | List RDF classes | `--verbose` |
| `playground meta sparql` | sparql | Execute SPARQL query | `[query]`, `--file` |
| `playground meta completions` | completions | Generate shell completions | `[shell]` |
| `playground meta middleware list` | middleware list | List middleware | -- |
| `playground meta telemetry` | telemetry | Export execution metrics | `[format]` |

## 4. Data Flow Architecture

### 4.1 Paper Generation Flow

```
+----------+     +----------+     +------------+     +----------+
|  CLI     | --> |  Domain  | --> | Integration| --> |  Output  |
| (citty)  |     | (models) |     | (nunjucks) |     |  (.tex)  |
+----------+     +----------+     +------------+     +----------+
     |                |                 |
     v                v                 v
  Parse args     Build Paper      Render template
  Validate       Apply rules      Write to disk
  Route cmd      Transform        Record in KG
```

### 4.2 SPARQL Query Flow

```
+----------+     +----------+     +------------+     +----------+
|  CLI     | --> |  Domain  | --> | Integration| --> |  Output  |
| (citty)  |     | (query)  |     | (unrdf)    |     | (result) |
+----------+     +----------+     +------------+     +----------+
     |                |                 |
     v                v                 v
  Parse query    Validate SPARQL   Execute query
  Set format     Optimize          Format results
  Route cmd      Transform         Return bindings
```

## 5. RDF Ontology Schema

### 5.1 Namespace Declarations

```turtle
@prefix pt: <http://papers-thesis.org/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dc: <http://purl.org/dc/terms/> .
```

### 5.2 Class Hierarchy

```turtle
# Core Classes
pt:AcademicWork a rdfs:Class ;
    rdfs:label "Academic Work" ;
    rdfs:comment "Base class for all academic documents" .

pt:Paper a rdfs:Class ;
    rdfs:subClassOf pt:AcademicWork ;
    rdfs:label "Research Paper" .

pt:Thesis a rdfs:Class ;
    rdfs:subClassOf pt:AcademicWork ;
    rdfs:label "Thesis" .

pt:Section a rdfs:Class ;
    rdfs:label "Document Section" .

pt:Author a rdfs:Class ;
    rdfs:subClassOf foaf:Person ;
    rdfs:label "Author" .

# Paper Family Classes
pt:IMRADPaper a rdfs:Class ;
    rdfs:subClassOf pt:Paper ;
    rdfs:label "IMRAD Paper" .

pt:DSRPaper a rdfs:Class ;
    rdfs:subClassOf pt:Paper ;
    rdfs:label "Design Science Research Paper" .

pt:ArgumentPaper a rdfs:Class ;
    rdfs:subClassOf pt:Paper ;
    rdfs:label "Argument-based Paper" .

# Thesis Type Classes
pt:Monograph a rdfs:Class ;
    rdfs:subClassOf pt:Thesis ;
    rdfs:label "Monograph Thesis" .

pt:NarrativeThesis a rdfs:Class ;
    rdfs:subClassOf pt:Thesis ;
    rdfs:label "Narrative Thesis" .

pt:ContributionThesis a rdfs:Class ;
    rdfs:subClassOf pt:Thesis ;
    rdfs:label "Contribution-based Thesis" .
```

### 5.3 Property Definitions

```turtle
# Academic Work Properties
pt:hasTitle a rdf:Property ;
    rdfs:domain pt:AcademicWork ;
    rdfs:range xsd:string .

pt:hasAbstract a rdf:Property ;
    rdfs:domain pt:AcademicWork ;
    rdfs:range xsd:string .

pt:hasSection a rdf:Property ;
    rdfs:domain pt:AcademicWork ;
    rdfs:range pt:Section .

pt:hasAuthor a rdf:Property ;
    rdfs:domain pt:AcademicWork ;
    rdfs:range pt:Author .

pt:createdAt a rdf:Property ;
    rdfs:domain pt:AcademicWork ;
    rdfs:range xsd:dateTime .

# Paper Properties
pt:paperFamily a rdf:Property ;
    rdfs:domain pt:Paper ;
    rdfs:range xsd:string .

# Thesis Properties
pt:thesisType a rdf:Property ;
    rdfs:domain pt:Thesis ;
    rdfs:range xsd:string .

pt:hasSchedule a rdf:Property ;
    rdfs:domain pt:Thesis ;
    rdfs:range pt:Schedule .

pt:defenseDate a rdf:Property ;
    rdfs:domain pt:Schedule ;
    rdfs:range xsd:date .

# Section Properties
pt:sectionHeading a rdf:Property ;
    rdfs:domain pt:Section ;
    rdfs:range xsd:string .

pt:sectionContent a rdf:Property ;
    rdfs:domain pt:Section ;
    rdfs:range xsd:string .

pt:sectionOrder a rdf:Property ;
    rdfs:domain pt:Section ;
    rdfs:range xsd:integer .

# Author Properties
pt:authorName a rdf:Property ;
    rdfs:domain pt:Author ;
    rdfs:range xsd:string .

pt:authorAffiliation a rdf:Property ;
    rdfs:domain pt:Author ;
    rdfs:range xsd:string .

pt:authorRole a rdf:Property ;
    rdfs:domain pt:Author ;
    rdfs:range xsd:string .
```

## 6. File Organization

```
playground/papers-thesis-cli/
|-- src/
|   |-- cli/
|   |   |-- commands/
|   |   |   |-- papers.mjs        # Papers noun handler
|   |   |   |-- thesis.mjs        # Thesis noun handler
|   |   |   |-- config.mjs        # Config noun handler
|   |   |   |-- meta.mjs          # Meta noun handler
|   |   |-- middleware/
|   |   |   |-- logging.mjs       # Request logging
|   |   |   |-- profiling.mjs     # Performance profiling
|   |   |   |-- rate-limit.mjs    # Rate limiting
|   |   |   |-- telemetry.mjs     # OTEL integration
|   |   |-- index.mjs             # Citty CLI setup
|   |-- domain/
|   |   |-- models/
|   |   |   |-- paper.mjs         # Paper class + Zod schema
|   |   |   |-- thesis.mjs        # Thesis class + Zod schema
|   |   |   |-- section.mjs       # Section class + Zod schema
|   |   |   |-- author.mjs        # Author class + Zod schema
|   |   |   |-- config.mjs        # Config class + Zod schema
|   |   |-- formatters/
|   |   |   |-- json.mjs          # JSON formatter
|   |   |   |-- yaml.mjs          # YAML formatter
|   |   |   |-- table.mjs         # Table formatter
|   |   |   |-- latex.mjs         # LaTeX formatter
|   |   |-- constants.mjs         # Paper families, thesis types
|   |   |-- index.mjs             # Domain exports
|   |-- integration/
|   |   |-- templates.mjs         # Nunjucks engine
|   |   |-- knowledge-graph.mjs   # UNRDF setup
|   |   |-- sparql.mjs            # Query execution
|   |   |-- file-io.mjs           # File operations
|   |   |-- index.mjs             # Integration exports
|   |-- index.mjs                 # Entry point
|-- templates/
|   |-- imrad.tex.njk             # IMRAD structure
|   |-- argument.tex.njk          # Argument structure
|   |-- contribution.tex.njk      # Contribution structure
|   |-- dsr.tex.njk               # Design Science Research
|   |-- monograph.tex.njk         # Academic monograph
|   |-- narrative.tex.njk         # Narrative thesis
|   |-- paper.tex.njk             # Generic paper
|-- ontologies/
|   |-- papers-thesis.ttl         # RDF ontology in Turtle
|   |-- examples.ttl              # Example instances
|-- output/                       # Generated papers
|-- test/
|   |-- cli/                      # CLI tests
|   |-- domain/                   # Domain tests
|   |-- integration/              # Integration tests
|-- package.json                  # Node.js dependencies
|-- vitest.config.mjs             # Test config
```

## 7. Citty Command Configuration

### 7.1 Global Options Schema

```javascript
// src/cli/index.mjs
export const globalArgs = {
  quiet: {
    type: 'boolean',
    alias: 'q',
    description: 'Suppress non-essential output',
    default: false
  },
  format: {
    type: 'string',
    alias: 'f',
    description: 'Output format (json, yaml, table, latex)',
    default: 'table'
  },
  output: {
    type: 'string',
    alias: 'o',
    description: 'Output file path'
  },
  verbose: {
    type: 'boolean',
    alias: 'v',
    description: 'Enable verbose output',
    default: false
  },
  config: {
    type: 'string',
    alias: 'c',
    description: 'Path to config file'
  }
};
```

### 7.2 Papers Command Configuration

```javascript
// src/cli/commands/papers.mjs
import { defineCommand } from 'citty';

export const papersCommand = defineCommand({
  meta: {
    name: 'papers',
    description: 'Manage research papers'
  },
  subCommands: {
    generate: defineCommand({
      meta: {
        name: 'generate',
        description: 'Generate paper from template'
      },
      args: {
        family: {
          type: 'positional',
          description: 'Paper family (imrad, dsr, argument, contribution)',
          required: false,
          default: 'imrad'
        },
        title: {
          type: 'string',
          alias: 't',
          description: 'Paper title',
          required: true
        },
        author: {
          type: 'string',
          alias: 'a',
          description: 'Author name',
          required: true
        },
        affiliation: {
          type: 'string',
          description: 'Author affiliation'
        },
        abstract: {
          type: 'string',
          description: 'Paper abstract'
        },
        sections: {
          type: 'string',
          description: 'Custom sections (JSON array)'
        }
      },
      async run(ctx) {
        // Implementation
      }
    }),
    list: defineCommand({
      meta: {
        name: 'list',
        description: 'List available paper families'
      },
      args: {
        verbose: {
          type: 'boolean',
          alias: 'v',
          description: 'Show detailed information',
          default: false
        }
      },
      async run(ctx) {
        // Implementation
      }
    }),
    validate: defineCommand({
      meta: {
        name: 'validate',
        description: 'Validate paper structure'
      },
      args: {
        path: {
          type: 'positional',
          description: 'Path to paper file',
          required: true
        },
        strict: {
          type: 'boolean',
          description: 'Enable strict validation',
          default: false
        }
      },
      async run(ctx) {
        // Implementation
      }
    })
  }
});
```

## 8. Integration Points

### 8.1 Citty to Domain

```javascript
// CLI layer receives args, calls domain functions
async function handlePaperGenerate(ctx) {
  const { family, title, author, affiliation, abstract } = ctx.args;

  // Domain function (pure, testable)
  const paper = createPaper({
    family,
    title,
    authors: [{ name: author, affiliation }],
    abstract
  });

  // Integration function (side effects)
  const latex = await renderTemplate(paper);
  await writePaper(latex, ctx.args.output);
  await recordInKnowledgeGraph(paper);

  return paper;
}
```

### 8.2 Domain to Integration

```javascript
// Domain requests templates from integration
export async function generatePaper(paper) {
  // Validate domain model
  const validated = PaperSchema.parse(paper);

  // Get template for family
  const templateName = FAMILY_TEMPLATES[validated.family];

  // Integration renders template
  return templateEngine.render(templateName, {
    paper: validated,
    date: new Date().toISOString()
  });
}
```

### 8.3 Integration to External

```javascript
// Integration handles external I/O
export async function writePaper(latex, outputPath) {
  const finalPath = outputPath || generateDefaultPath();
  await fs.writeFile(finalPath, latex, 'utf-8');
  return finalPath;
}

export async function queryKnowledgeGraph(sparql) {
  const results = await query(sparql, knowledgeStore);
  return formatResults(results);
}
```

## 9. Key Differences from Rust Version

| Aspect | Rust (clap-noun-verb) | JavaScript (citty) |
|--------|----------------------|-------------------|
| **CLI Framework** | clap-noun-verb v5.1.0 | citty v0.1.6 |
| **Templating** | Tera 1.20 | Nunjucks 3.x |
| **RDF Store** | Oxigraph 0.5.1 | UNRDF knowledge-engine |
| **Async Model** | Sync + Tokio | Promise-based |
| **Type System** | Rust structs | JSDoc + Zod |
| **Serialization** | Serde | JSON.stringify |
| **Error Handling** | anyhow::Error | Custom error classes |
| **Shell Completions** | clap_complete | Manual generation |
| **Middleware** | Tower-like | Function composition |

## 10. Implementation Phases

### Phase 1: Foundation (Week 1)
- Project setup and dependencies
- Domain models with Zod schemas
- Citty CLI skeleton
- Basic command routing

### Phase 2: Templates (Week 2)
- Nunjucks engine setup
- Template conversion (Tera -> Nunjucks)
- LaTeX escaping filters
- File I/O operations

### Phase 3: Knowledge Graph (Week 3)
- RDF ontology definition
- UNRDF integration
- SPARQL query execution
- Knowledge hook setup

### Phase 4: Commands (Week 4)
- All CLI commands implemented
- Middleware (logging, profiling)
- Shell completions
- Telemetry integration

### Phase 5: Polish (Week 5)
- Output formatters (json, yaml, table)
- Error handling refinement
- Documentation
- Test coverage

## 11. Testing Strategy

### 11.1 Unit Tests

```javascript
// test/domain/models/paper.test.mjs
import { describe, it, expect } from 'vitest';
import { createPaper, PaperSchema } from '../../../src/domain/models/paper.mjs';

describe('Paper Model', () => {
  it('should create valid IMRAD paper', () => {
    const paper = createPaper({
      family: 'imrad',
      title: 'Test Paper',
      authors: [{ name: 'Alice', affiliation: 'MIT' }]
    });

    expect(PaperSchema.safeParse(paper).success).toBe(true);
    expect(paper.family).toBe('imrad');
  });
});
```

### 11.2 Integration Tests

```javascript
// test/integration/templates.test.mjs
import { describe, it, expect } from 'vitest';
import { createTemplateEngine } from '../../src/integration/templates.mjs';

describe('Template Engine', () => {
  it('should render IMRAD template', async () => {
    const engine = createTemplateEngine('./templates');
    const latex = await engine.render('imrad', {
      paper: { title: 'Test', authors: [{ name: 'Alice' }] }
    });

    expect(latex).toContain('\\title{Test}');
    expect(latex).toContain('\\author{Alice}');
  });
});
```

### 11.3 E2E Tests

```javascript
// test/e2e/cli.test.mjs
import { describe, it, expect } from 'vitest';
import { execa } from 'execa';

describe('CLI E2E', () => {
  it('should generate paper via CLI', async () => {
    const { stdout } = await execa('node', [
      './src/index.mjs',
      'papers', 'generate',
      '--title', 'Test Paper',
      '--author', 'Alice'
    ]);

    expect(stdout).toContain('Paper generated');
  });
});
```

## 12. Observability Integration

### 12.1 OpenTelemetry Spans

```javascript
// src/cli/middleware/telemetry.mjs
import { createObservabilityManager } from 'unrdf/knowledge-engine';

const obs = createObservabilityManager({
  serviceName: 'papers-thesis-cli',
  enableTracing: true,
  enableMetrics: true
});

export function withTelemetry(command) {
  return async (ctx) => {
    const span = obs.startSpan(`cli.${ctx.command}`);
    try {
      const result = await command(ctx);
      span.setStatus({ code: 'OK' });
      return result;
    } catch (error) {
      span.setStatus({ code: 'ERROR', message: error.message });
      throw error;
    } finally {
      span.end();
    }
  };
}
```

## 13. Security Considerations

1. **Input Validation**: All user input validated through Zod schemas
2. **File Path Sanitization**: Prevent directory traversal attacks
3. **SPARQL Injection**: Parameterized queries only
4. **Secrets Management**: No hardcoded credentials
5. **Template Escaping**: LaTeX special characters escaped

## 14. Performance Targets

| Metric | Target |
|--------|--------|
| CLI startup time | < 100ms |
| Paper generation | < 500ms |
| SPARQL query (simple) | < 50ms |
| SPARQL query (complex) | < 200ms |
| Template rendering | < 100ms |
| Knowledge graph load | < 1s |

## 15. Dependencies

```json
{
  "dependencies": {
    "citty": "^0.1.6",
    "nunjucks": "^3.2.4",
    "unrdf": "^4.0.0",
    "zod": "^3.22.0",
    "yaml": "^2.3.0"
  },
  "devDependencies": {
    "vitest": "^1.0.0",
    "@vitest/coverage-v8": "^1.0.0",
    "execa": "^8.0.0"
  }
}
```
