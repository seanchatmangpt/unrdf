# @unrdf/yawl-langchain

**LangChain Integration for YAWL Workflow Engine**

AI-powered workflow orchestration combining LangChain's agent framework with YAWL's robust workflow patterns and RDF knowledge graph integration.

## Overview

This package provides a seamless bridge between LangChain (AI agent framework) and YAWL (workflow engine), enabling:

- **AI Workflow Orchestration**: Use LangChain agents as executable workflow tasks
- **RDF Context Injection**: Automatically inject knowledge graph context into LLM prompts via SPARQL
- **Knowledge Graph Population**: Store LLM outputs as RDF triples for full semantic integration
- **Prompt Engineering Policies**: Use YAWL hooks to enforce prompt engineering best practices
- **Auditable AI Workflows**: Leverage YAWL's receipt chain for cryptographic audit trails

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     YAWL Workflow Engine                        │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐         │
│  │   Task A     │─▶│   Task B     │─▶│   Task C     │         │
│  │  (Analyzer)  │  │  (Reviewer)  │  │ (Suggester)  │         │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘         │
│         │                 │                 │                  │
└─────────┼─────────────────┼─────────────────┼──────────────────┘
          │                 │                 │
          ▼                 ▼                 ▼
┌─────────────────────────────────────────────────────────────────┐
│              YAWLLangChainAdapter (Bridge Layer)                │
│  • RDF Context Extraction (SPARQL)                              │
│  • Prompt Building (Template + Context + Input)                 │
│  • Agent Invocation (LangChain)                                 │
│  • RDF Triple Generation (Outputs → Knowledge Graph)            │
└─────────────────────────────────────────────────────────────────┘
          │                 │                 │
          ▼                 ▼                 ▼
┌─────────────────────────────────────────────────────────────────┐
│                    LangChain Agents Layer                       │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐         │
│  │   ChatGPT    │  │   Claude     │  │  Custom LLM  │         │
│  │  Agent 1     │  │  Agent 2     │  │  Agent 3     │         │
│  └──────────────┘  └──────────────┘  └──────────────┘         │
└─────────────────────────────────────────────────────────────────┘
          │                 │                 │
          ▼                 ▼                 ▼
┌─────────────────────────────────────────────────────────────────┐
│                   RDF Knowledge Graph Store                     │
│  • Input Context (Code, Data, Facts)                            │
│  • Agent Outputs (Analysis, Reviews, Suggestions)               │
│  • Execution Metadata (Timestamps, Tokens, Performance)         │
└─────────────────────────────────────────────────────────────────┘
```

## Key Integration Points

### 1. **Task Wrapping**
```javascript
const adapter = new YAWLLangChainAdapter({
  taskId: 'analyze-code',
  agent: new ChatOpenAI({ modelName: 'gpt-4' }),
  contextQuery: 'SELECT ?code WHERE { ?file :hasContent ?code }',
  rdfPredicate: 'http://example.org/codeAnalysis',
});

const taskDef = adapter.createTaskDefinition();
// taskDef is a standard YAWL TaskDefinition
```

### 2. **RDF Context → LLM Prompt**
```javascript
// SPARQL query extracts context from knowledge graph
contextQuery: `
  SELECT ?code ?language ?author WHERE {
    ?file :hasContent ?code ;
          :language ?language ;
          :author ?author .
  }
`

// Context automatically injected into prompt:
// "Knowledge Graph Context:
//  [{"code": "...", "language": "javascript", "author": "..."}]
//
//  Analyze this code: {input}"
```

### 3. **LLM Output → RDF Triples**
```javascript
// Agent output automatically stored as RDF:
<task-uri> <http://example.org/codeAnalysis> "Analysis: ..." .
<task-uri> <yawl-lc:agentOutput> "Analysis: ..." .
<task-uri> <yawl-lc:executedAt> "2025-12-25T12:00:00Z" .
<task-uri> <yawl-lc:executionTime> "2500000000" .
```

### 4. **Hook-Based Prompt Engineering**
```javascript
const promptHook = createPromptEngineeringHook(async (prompt, context) => {
  // Add domain-specific instructions
  return `You are a senior code reviewer. ${prompt}\n\n` +
         `Guidelines: Focus on security, performance, and maintainability.`;
});

taskDef.preCondition = promptHook;
```

## Installation

```bash
pnpm add @unrdf/yawl-langchain
```

**Dependencies**:
- `@unrdf/yawl` - YAWL workflow engine
- `@unrdf/oxigraph` - RDF triple store
- `langchain` - LangChain framework
- `@langchain/openai` - OpenAI integration (or other LLM providers)

## Quick Start

### Basic Example

```javascript
import { ChatOpenAI } from '@langchain/openai';
import { YAWLLangChainAdapter } from '@unrdf/yawl-langchain';
import { Workflow, WorkflowEngine } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

// 1. Create LangChain agent
const agent = new ChatOpenAI({
  modelName: 'gpt-4',
  temperature: 0,
  openAIApiKey: process.env.OPENAI_API_KEY,
});

// 2. Wrap as YAWL task
const adapter = new YAWLLangChainAdapter({
  taskId: 'summarize-text',
  agent,
  promptTemplate: 'Summarize this text in 3 sentences: {text}',
  rdfPredicate: 'http://example.org/summary',
});

// 3. Create workflow
const workflow = new Workflow({
  id: 'text-processing',
  tasks: [adapter.createTaskDefinition()],
});

// 4. Execute
const engine = new WorkflowEngine();
engine.registerWorkflow(workflow);
const workflowCase = engine.createCase('text-processing', {
  text: 'Long document text here...',
});

// Task executes via LangChain, stores result in RDF
```

### Advanced Example: AI Code Review

See [`examples/code-review-workflow.mjs`](./examples/code-review-workflow.mjs) for a complete 3-agent workflow:

1. **Code Analyzer** - Analyzes complexity and patterns
2. **Security Reviewer** - Identifies vulnerabilities
3. **Suggestion Generator** - Generates improvement recommendations

Each agent uses RDF context from previous steps and stores outputs as RDF triples.

```bash
# Run the example
pnpm run example:code-review
```

## API Reference

### `YAWLLangChainAdapter`

Main adapter class for wrapping LangChain agents as YAWL tasks.

#### Constructor

```javascript
new YAWLLangChainAdapter(config)
```

**Config Options**:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `taskId` | `string` | ✅ | Unique task identifier |
| `taskName` | `string` | | Human-readable name (defaults to taskId) |
| `agent` | `Runnable` | ✅ | LangChain agent/chain |
| `inputMapping` | `Record<string, string>` | | Map YAWL input → LangChain input |
| `outputField` | `string` | | Field to extract from LLM response (default: 'output') |
| `contextQuery` | `string` | | SPARQL query for RDF context injection |
| `promptTemplate` | `string` | | Prompt template with placeholders |
| `rdfPredicate` | `string` | | RDF predicate for storing output |
| `preHook` | `Function` | | Pre-execution hook (prompt engineering) |
| `postHook` | `Function` | | Post-execution hook (output validation) |
| `timeout` | `number` | | Timeout in milliseconds (default: 30000) |

#### Methods

##### `createTaskDefinition(options)`
Creates a YAWL TaskDefinition for this agent.

**Returns**: `TaskDefinition`

##### `execute(taskInstance, caseData)`
Executes the LangChain agent for a task instance.

**Parameters**:
- `taskInstance`: YAWL TaskInstance
- `caseData`: Case-level data (may include RDF store)

**Returns**: `Promise<{ output, rdfTriples, executionTime, metadata }>`

##### `getRDFStore()`
Gets the internal RDF store containing agent outputs.

**Returns**: `OxigraphStore`

##### `getExecutionHistory()`
Gets execution history for all invocations.

**Returns**: `Array<{ taskId, timestamp, executionTime, prompt, output, rdfTriples }>`

##### `exportAsTurtle()`
Exports RDF store as Turtle format.

**Returns**: `string`

### Helper Functions

#### `createLangChainTaskExecutor(config)`
Creates a task executor function compatible with YAWL engines.

```javascript
const executor = createLangChainTaskExecutor({
  taskId: 'my-task',
  agent: myAgent,
});

// Use with YAWL engine
engine.registerTaskExecutor('my-task', executor);
```

#### `createPromptEngineeringHook(promptModifier)`
Creates a YAWL hook for prompt engineering policies.

```javascript
const hook = createPromptEngineeringHook(async (prompt, context) => {
  return `Enhanced: ${prompt}`;
});

taskDef.preCondition = hook;
```

## RDF Ontology

### Namespace

```
@prefix yawl-lc: <http://unrdf.dev/yawl/langchain#> .
```

### Classes

- `yawl-lc:Agent` - LangChain agent execution

### Properties

| Property | Domain | Range | Description |
|----------|--------|-------|-------------|
| `yawl-lc:agentOutput` | Task | `xsd:string` | LLM output text |
| `yawl-lc:agentInput` | Task | `xsd:string` | Input data (JSON) |
| `yawl-lc:executedAt` | Task | `xsd:dateTime` | Execution timestamp |
| `yawl-lc:executionTime` | Task | `xsd:integer` | Duration in nanoseconds |
| `yawl-lc:modelName` | Task | `xsd:string` | LLM model identifier |
| `yawl-lc:tokenUsage` | Task | `xsd:integer` | Tokens consumed |
| `yawl-lc:rdfContext` | Task | `xsd:string` | Context used (JSON) |
| `yawl-lc:promptUsed` | Task | `xsd:string` | Final prompt sent to LLM |

## Use Cases

### 1. **Automated Code Review**
Chain multiple AI agents to analyze code quality, security, and suggest improvements. Each agent uses outputs from previous agents via RDF context.

### 2. **Document Processing Pipeline**
Extract → Summarize → Translate → Tag documents using LangChain agents orchestrated by YAWL workflow patterns.

### 3. **Knowledge Graph Enrichment**
Use LLMs to extract entities, relationships, and facts from text, storing results as RDF triples for semantic search.

### 4. **Multi-Agent Research Assistant**
Coordinate multiple specialized agents (researcher, critic, synthesizer) with YAWL control flow patterns.

### 5. **Compliance Review Workflows**
Automate regulatory compliance checks using domain-specific LLMs with YAWL's audit trail and receipt verification.

## Testing

```bash
# Run tests
pnpm test

# Watch mode
pnpm test:watch

# Coverage
pnpm test:coverage
```

## Development

```bash
# Install dependencies
pnpm install

# Lint
pnpm lint

# Type check
pnpm typecheck

# Validate (lint + typecheck + test)
pnpm validate
```

## Design Principles

### 1. **Separation of Concerns**
- **YAWL**: Workflow orchestration, control flow, state management
- **LangChain**: AI agent execution, prompt engineering
- **RDF**: Knowledge representation, semantic integration

### 2. **Pure Functional Integration**
- Adapters are pure functions: `(TaskInstance, CaseData) → Result`
- No side effects in business logic (RDF storage is output, not side effect)
- Zod validation at boundaries

### 3. **Knowledge Graph First**
- All agent inputs/outputs flow through RDF
- SPARQL enables semantic querying across workflow executions
- Traceability via RDF provenance chains

### 4. **Auditable AI**
- YAWL receipts provide cryptographic proof of execution
- RDF graphs enable temporal queries (who ran what, when)
- Full observability via OTEL integration (from YAWL)

## Performance Considerations

- **RDF Queries**: SPARQL queries run on in-memory store (fast)
- **LLM Latency**: Typical 1-5s per agent invocation
- **Memory**: RDF store grows with workflow history (use time-travel snapshots)
- **Concurrency**: YAWL supports parallel agent execution (AND-split pattern)

## Limitations

- **LangChain Version**: Tested with LangChain 0.3.x
- **RDF Store**: Uses Oxigraph (embedded), not distributed
- **Agent Types**: Supports Runnables, Chains, Agents (not Tools directly)
- **Context Size**: Large RDF contexts may exceed LLM token limits

## Roadmap

- [ ] Vector store integration for RAG workflows
- [ ] Multi-modal agent support (vision, audio)
- [ ] Distributed RDF store backend (GraphDB, Stardog)
- [ ] LangGraph integration for complex agent graphs
- [ ] Streaming LLM responses via YAWL event bus

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for development guidelines.

## License

MIT - See [LICENSE](../../LICENSE)

## Related Projects

- [@unrdf/yawl](../yawl) - YAWL workflow engine
- [@unrdf/oxigraph](../oxigraph) - RDF triple store
- [LangChain](https://github.com/langchain-ai/langchainjs) - AI agent framework

## Citation

If you use this package in research, please cite:

```bibtex
@software{unrdf_yawl_langchain,
  title = {YAWL-LangChain Integration: AI-Powered Workflow Orchestration with RDF},
  author = {UNRDF Project},
  year = {2025},
  url = {https://github.com/unrdf/yawl-langchain}
}
```

---

**Built with ❤️ by the UNRDF Project**
