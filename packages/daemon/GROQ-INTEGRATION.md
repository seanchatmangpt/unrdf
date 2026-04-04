# Groq LLM Integration with UNRDF Daemon

The UNRDF daemon is fully wired with Groq LLM support via the `@ai-sdk/groq` provider.

## Configuration

### 1. Environment Variables

```bash
export GROQ_API_KEY="your-groq-api-key"
export GROQ_MODEL="openai/gpt-oss-20b"  # Default model (optional)
export GROQ_SERVICE_TIER="on_demand"    # on_demand, performance, flex, auto
export LOG_LEVEL="info"                 # debug, info, warn, error
export DAEMON_PORT="3000"               # Daemon HTTP port
```

### 2. Configuration File (.unrdf.toml)

The project includes a root-level `.unrdf.toml` file with default settings:

```toml
[groq]
model = "openai/gpt-oss-20b"
structuredOutputs = true
strictJsonSchema = true
parallelToolCalls = true
serviceTier = "on_demand"
timeout = 30000

[daemon]
port = 3000
securityMiddleware = true

[mcp]
transport = "stdio"
autoStart = true
```

## Usage

### Basic LLM Generation

```javascript
import { getGroqProvider } from '@unrdf/daemon';
import { generateText } from 'ai';

// Initialize at daemon startup
const provider = getGroqProvider();
const model = provider.getDefaultModel();

// Generate text
const result = await generateText({
  model,
  prompt: 'Analyze this RDF knowledge graph: ...',
});

console.log(result.text);
```

### With Configuration Loading

```javascript
import { loadConfig, initializeGroqProvider, getGroqProvider } from '@unrdf/daemon';
import { generateText } from 'ai';

// Load config from .unrdf.toml and environment
const config = loadConfig();

// Initialize Groq with loaded config
initializeGroqProvider(config.groq);

const provider = getGroqProvider();
const model = provider.getModel(config.groq?.model);

// Use for generation
const result = await generateText({
  model,
  prompt: 'Query the RDF graph for entities...',
});
```

### Tool Use (Parallel Function Calling)

```javascript
import { getGroqProvider } from '@unrdf/daemon';
import { generateText } from 'ai';

const provider = getGroqProvider();
const model = provider.getDefaultModel();

const result = await generateText({
  model,
  tools: {
    queryGraph: {
      description: 'Query the RDF graph',
      parameters: {
        type: 'object',
        properties: {
          query: { type: 'string' },
        },
        required: ['query'],
      },
    },
    executeHooks: {
      description: 'Execute knowledge hooks',
      parameters: {
        type: 'object',
        properties: {
          hooks: { type: 'array' },
        },
        required: ['hooks'],
      },
    },
  },
  toolChoice: 'required',
  prompt: 'Analyze the RDF graph and apply transformation rules...',
  providerOptions: {
    groq: provider.getProviderOptions(),
  },
});
```

### Structured Outputs with Zod Schema

```javascript
import { getGroqProvider } from '@unrdf/daemon';
import { generateObject } from 'ai';
import { z } from 'zod';

const provider = getGroqProvider();
const model = provider.getDefaultModel();

const schema = z.object({
  entities: z.array(
    z.object({
      id: z.string(),
      label: z.string(),
      type: z.string(),
    })
  ),
  relationships: z.array(
    z.object({
      source: z.string(),
      target: z.string(),
      predicate: z.string(),
    })
  ),
});

const result = await generateObject({
  model,
  schema,
  prompt: 'Extract entities and relationships from this RDF graph...',
  providerOptions: {
    groq: provider.getProviderOptions(),
  },
});

console.log(JSON.stringify(result.object, null, 2));
```

### Reasoning Models

For reasoning models like `qwen-qwq-32b`:

```javascript
import { getGroqProvider } from '@unrdf/daemon';
import { generateText } from 'ai';

const provider = createGroqProvider({
  model: 'qwen-qwq-32b',
  reasoningFormat: 'parsed',
  reasoningEffort: 'default',
});

const model = provider.getDefaultModel();

const result = await generateText({
  model,
  prompt: 'Reason about the consistency of this RDF ontology...',
});

console.log(result.text);
```

### Browser Search (OSS Models Only)

```javascript
import { getGroqProvider, groq } from '@unrdf/daemon';
import { generateText } from 'ai';

const model = groq('openai/gpt-oss-120b');

const result = await generateText({
  model,
  tools: {
    browser_search: groq.tools.browserSearch({}),
  },
  toolChoice: 'required',
  prompt: 'Search for latest developments in RDF and semantic web technologies',
});

console.log(result.text);
```

## Available Models

### High-Performance Models

- **openai/gpt-oss-120b** - Largest OSS model (120B params)
- **openai/gpt-oss-20b** - Default (20B params)
- **meta-llama/llama-4-scout-17b-16e-instruct** - Fast multimodal
- **meta-llama/llama-4-maverick-17b-128e-instruct** - Mixture of experts

### Reasoning Models

- **qwen-qwq-32b** - Strong reasoning capability
- **deepseek-r1-distill-llama-70b** - Reasoning + language
- **deepseek-r1-distill-qwen-32b** - Efficient reasoning

### Specialized Models

- **llama-3.3-70b-versatile** - General purpose
- **gemma2-9b-it** - Instruction tuned
- **moonshotai/kimi-k2-instruct-0905** - Multilingual

## Service Tiers

Configure performance characteristics via `serviceTier`:

- **on_demand** (default) - Consistent performance and fairness
- **performance** - Prioritized for latency-sensitive work
- **flex** - 10x rate limits for bursty workloads (higher failure rate)
- **auto** - Falls back to flex tier if on_demand limits exceeded

## Configuration Options

| Option              | Type    | Default              | Description                      |
| ------------------- | ------- | -------------------- | -------------------------------- |
| `apiKey`            | string  | `GROQ_API_KEY` env   | Groq API authentication key      |
| `model`             | string  | `openai/gpt-oss-20b` | Default model ID                 |
| `serviceTier`       | enum    | `on_demand`          | Request priority tier            |
| `structuredOutputs` | boolean | `true`               | Enable JSON schema outputs       |
| `strictJsonSchema`  | boolean | `true`               | Enforce strict schema validation |
| `parallelToolCalls` | boolean | `true`               | Enable parallel function calling |
| `reasoningFormat`   | enum    | —                    | Reasoning: parsed, raw, hidden   |
| `reasoningEffort`   | enum    | default              | Reasoning: low, medium, high     |
| `timeout`           | number  | `30000`              | Request timeout (ms)             |

## Integration with Daemon Components

### Self-Play Autonomics

Use Groq to enhance the knowledge hooks self-play loop:

```javascript
import { loadConfig, initializeGroqProvider, getGroqProvider } from '@unrdf/daemon';
import { runHooksAutonomics } from '@unrdf/hooks/self-play-autonomics';

const config = loadConfig();
initializeGroqProvider(config.groq);

const store = new Store();
const hooks = [];

// Use Groq to reason about hook execution strategy
const result = await runHooksAutonomics(store, hooks, {
  goalCondition: async (store, previousResult) => {
    // Use Groq to evaluate if goal is reached
    const provider = getGroqProvider();
    const model = provider.getDefaultModel();

    const analysis = await generateText({
      model,
      prompt: `Analyze hook execution results: ${JSON.stringify(previousResult)}. Goal reached?`,
    });

    return analysis.text.toLowerCase().includes('yes');
  },
  episodeCount: 3,
});
```

### MCP Tools

Register Groq as an MCP tool:

```javascript
import { startMCPServer } from '@unrdf/daemon';
import { getGroqProvider } from '@unrdf/daemon';

const provider = getGroqProvider();

const server = await startMCPServer({
  tools: {
    generateWithGroq: {
      description: 'Generate text using Groq',
      inputSchema: {
        type: 'object',
        properties: {
          prompt: { type: 'string' },
          model: { type: 'string', default: provider.getConfig().model },
        },
        required: ['prompt'],
      },
      handler: async input => {
        const { text } = await generateText({
          model: provider.getModel(input.model),
          prompt: input.prompt,
        });
        return { text };
      },
    },
  },
});
```

## See Also

- [Groq AI SDK Documentation](https://sdk.vercel.ai/docs/reference/ai-sdk-core/generate-text)
- [UNRDF Daemon Documentation](./README.md)
- [Knowledge Hooks Self-Play Autonomics](../hooks/docs/SELF-PLAY-AUTONOMICS.md)
- [MCP Integration Guide](./MCP-SELF-PLAY.md)
