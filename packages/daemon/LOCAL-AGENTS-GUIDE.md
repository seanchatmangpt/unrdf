# Building Local AI Agents with Groq and UNRDF

Learn how to build, run, and scale AI agents locally using Groq LLM and the UNRDF daemon's MCP server. This guide covers calling LLMs, defining tools, creating multi-step agents, and managing local deployments.

## Overview

An AI agent is a system that observes its environment, makes decisions, and takes actions to achieve goals. You can think of an agent as a loop that:

1. Receives input or context
2. Calls an LLM (in this case, Groq with `openai/gpt-oss-20b`)
3. The LLM decides which tool to use (if any)
4. Executes the chosen tool
5. Feeds the result back to the LLM before the next decision
6. Repeats until reaching a stopping condition or goal

This guide uses the **AI SDK** from Vercel (which supports Groq) and the **UNRDF daemon** for task orchestration and RDF knowledge management.

---

## Prerequisites

Before you begin, make sure you have:

- **Node.js** ≥ 18.0.0
- **pnpm** package manager
- A **Groq API key** from https://console.groq.com
- Basic knowledge of JavaScript/ESM and RDF (optional but helpful)

---

## Step 1: Set Up Your Environment

### Install Dependencies

```bash
# In your project directory
pnpm add ai @ai-sdk/groq zod n3

# Or, if using UNRDF daemon
pnpm add @unrdf/daemon
```

### Configure Groq

Create a `.env.local` file with your Groq credentials:

```bash
GROQ_API_KEY="your-groq-api-key-here"
GROQ_MODEL="openai/gpt-oss-20b"  # Default open-source model
```

Or configure via `.unrdf.toml`:

```toml
[groq]
apiKey = "your-groq-api-key-here"
model = "openai/gpt-oss-20b"
serviceTier = "on_demand"
timeout = 30000
```

---

## Step 2: Call a Groq LLM

The simplest agent is a single LLM call that generates text based on a prompt.

```javascript
import { generateText } from 'ai';
import { getGroqProvider } from '@unrdf/daemon';

// Get the Groq provider (uses config from .unrdf.toml + env)
const provider = getGroqProvider();
const model = provider.getDefaultModel();

// Call the LLM
const result = await generateText({
  model,
  prompt: 'What is the capital of France?',
  maxTokens: 100,
});

console.log(result.text); // Output: "The capital of France is Paris."
```

**Key Points:**

- `generateText()` is the simplest API for text generation
- It handles API calls, token counting, and response streaming
- The Groq provider manages authentication automatically from environment or config
- Returns a result object with `.text` property

---

## Step 3: Define Tools for Your Agent

Tools are functions the agent can call to extend its capabilities. Define tools using `tool()` from the AI SDK:

```javascript
import { generateText, tool } from 'ai';
import { getGroqProvider } from '@unrdf/daemon';
import { z } from 'zod';

const provider = getGroqProvider();
const model = provider.getDefaultModel();

// Define a simple weather tool
const tools = {
  get_weather: tool({
    description: 'Get the current weather in a location',
    inputSchema: z.object({
      location: z.string().describe('City name'),
      unit: z.enum(['celsius', 'fahrenheit']).default('celsius'),
    }),
    execute: async ({ location, unit }) => {
      // In production, call a real weather API
      const temperature = 72 + Math.floor(Math.random() * 20) - 10;
      const tempValue = unit === 'fahrenheit' ? temperature : (temperature - 32) * (5 / 9);
      return {
        location,
        temperature: Math.round(tempValue),
        unit,
        condition: 'Partly Cloudy',
      };
    },
  }),

  get_time: tool({
    description: 'Get the current time in a timezone',
    inputSchema: z.object({
      timezone: z.string().describe('IANA timezone (e.g., "America/New_York")'),
    }),
    execute: async ({ timezone }) => {
      const now = new Date();
      return {
        timezone,
        time: now.toLocaleString('en-US', { timeZone: timezone }),
        unix_timestamp: now.getTime() / 1000,
      };
    },
  }),
};

// Call the agent with tools
const result = await generateText({
  model,
  prompt: 'What is the weather in San Francisco and what time is it there?',
  tools,
  toolChoice: 'auto', // Let the model decide which tools to use
  maxTokens: 500,
});

console.log(result.text);
// Output might include tool calls and results synthesized into a response
```

**Key Points:**

- Tools are defined with a description, `inputSchema` (Zod), and `execute` function
- Descriptions guide the LLM on when and how to use each tool
- Return values are automatically added to the conversation history
- `toolChoice: 'auto'` lets Groq decide if tools are needed
- The LLM can use multiple tools in sequence

---

## Step 4: Create a Multi-Step Agent

A multi-step agent loops through tool calls until it reaches a stopping condition.

```javascript
import { generateText, tool, stepCountIs } from 'ai';
import { getGroqProvider } from '@unrdf/daemon';
import { z } from 'zod';

const provider = getGroqProvider();
const model = provider.getDefaultModel();

const tools = {
  query_knowledge_graph: tool({
    description: 'Query the RDF knowledge graph',
    inputSchema: z.object({
      query: z.string().describe('SPARQL SELECT query'),
    }),
    execute: async ({ query }) => {
      // In production, run against a real SPARQL endpoint
      return {
        query,
        results: [
          { entity: 'Alice', type: 'Person' },
          { entity: 'Bob', type: 'Person' },
        ],
      };
    },
  }),

  reason_about_data: tool({
    description: 'Apply reasoning rules to data',
    inputSchema: z.object({
      data: z.string().describe('JSON data to reason about'),
      rule: z.string().describe('Reasoning rule to apply'),
    }),
    execute: async ({ data, rule }) => {
      // Apply a knowledge hook or transformation
      return {
        rule,
        inference: 'Applied successfully',
        result: { enriched: true },
      };
    },
  }),
};

// Create a multi-step agent
const result = await generateText({
  model,
  prompt: `Analyze the knowledge graph and answer:
  1. How many entities exist?
  2. What type are they?
  3. Can you suggest an enhancement?`,
  tools,
  toolChoice: 'auto',
  stopWhen: stepCountIs(5), // Allow up to 5 decision steps
  maxTokens: 1000,
});

console.log('Final answer:', result.text);
```

**Key Points:**

- `stepCountIs(n)` sets the maximum number of tool-calling steps (not API calls)
- The agent automatically loops: LLM → tool → result → LLM → next decision
- Each step appends tool calls and results to the conversation history
- Stops when the LLM generates text without a tool call OR when max steps is reached

---

## Step 5: Structure Complex Workflows

For knowledge-driven agents, structure workflows to reason about RDF data:

```javascript
import { generateText, tool, stepCountIs } from 'ai';
import { getGroqProvider } from '@unrdf/daemon';
import { z } from 'zod';
import { Store } from 'n3';

const provider = getGroqProvider();
const model = provider.getDefaultModel();

// Share state across tool calls
let store = new Store();

const tools = {
  describe_graph: tool({
    description: 'Analyze the current RDF graph structure',
    inputSchema: z.object({}),
    execute: async () => {
      const size = store.size;
      const subjects = new Set(Array.from(store).map(q => q.subject.value));
      return {
        triple_count: size,
        unique_subjects: subjects.size,
        sample_subjects: Array.from(subjects).slice(0, 5),
      };
    },
  }),

  suggest_enrichment: tool({
    description: 'Suggest RDF triples to add for enrichment',
    inputSchema: z.object({
      enrichment: z.string().describe('Enrichment suggestion'),
    }),
    execute: async ({ enrichment }) => {
      return {
        suggestion: enrichment,
        confidence: 0.85,
        ready_to_apply: true,
      };
    },
  }),

  apply_enhancement: tool({
    description: 'Add suggested triples to the graph',
    inputSchema: z.object({
      triple: z.string().describe('RDF triple in N-Triples format'),
    }),
    execute: async ({ triple }) => {
      // Parse and add to store (simplified)
      return {
        added: true,
        new_size: store.size + 1,
      };
    },
  }),
};

// Autonomous enrichment loop
const result = await generateText({
  model,
  prompt: `You are a knowledge engineer. Your job is to enrich this RDF graph:

1. First, describe the current graph (use describe_graph)
2. Based on what you find, suggest enhancements (use suggest_enrichment)
3. If the suggestion is good (confidence > 0.8), apply it (use apply_enhancement)
4. Describe the enriched graph

Be concise. Focus on adding one meaningful triple.`,
  tools,
  stopWhen: stepCountIs(6),
  maxTokens: 500,
});

console.log('Enrichment complete:', result.text);
```

**Key Points:**

- Tools can share mutable state (like the RDF store)
- Structure prompts to guide the agent's decision-making
- Each tool call updates shared state for the next decision
- The agent learns from tool results to refine its strategy

---

## Step 6: Deploy Locally

### Option A: Node.js Script

Create `agent.mjs`:

```javascript
import { generateText, tool, stepCountIs } from 'ai';
import { getGroqProvider, loadConfig } from '@unrdf/daemon';
import { z } from 'zod';

const config = loadConfig();
const provider = getGroqProvider();
const model = provider.getDefaultModel();

const tools = {
  // Define your tools here
};

async function runAgent(userInput) {
  const result = await generateText({
    model,
    prompt: userInput,
    tools,
    stopWhen: stepCountIs(10),
    maxTokens: 2000,
  });
  return result.text;
}

// Run the agent
const answer = await runAgent('Analyze the RDF graph and suggest improvements');
console.log(answer);
```

Run it:

```bash
export GROQ_API_KEY="your-key"
node agent.mjs
```

### Option B: HTTP Server (Express/Fastify)

Create `server.mjs`:

```javascript
import Fastify from 'fastify';
import { generateText, tool, stepCountIs } from 'ai';
import { getGroqProvider } from '@unrdf/daemon';
import { z } from 'zod';

const app = Fastify();
const provider = getGroqProvider();
const model = provider.getDefaultModel();

const tools = {
  // Define your tools here
};

app.post('/agent', async (request, reply) => {
  const { prompt } = request.body;

  if (!prompt) {
    return reply.code(400).send({ error: 'Prompt required' });
  }

  try {
    const result = await generateText({
      model,
      prompt,
      tools,
      stopWhen: stepCountIs(10),
      maxTokens: 2000,
    });

    return {
      response: result.text,
      steps: result.steps,
      toolCalls: result.toolCalls || [],
    };
  } catch (error) {
    return reply.code(500).send({ error: error.message });
  }
});

app.listen({ port: 3000 }, err => {
  if (err) {
    console.error('Failed to start server:', err);
    process.exit(1);
  }
  console.log('Agent server listening on http://localhost:3000');
});
```

Run it:

```bash
pnpm add fastify
export GROQ_API_KEY="your-key"
node server.mjs
```

Test it:

```bash
curl -X POST http://localhost:3000/agent \
  -H "Content-Type: application/json" \
  -d '{"prompt":"What is the weather in Tokyo?"}'
```

### Option C: Daemon Integration

Use the UNRDF daemon for task orchestration and scheduling:

```javascript
import { Daemon } from '@unrdf/daemon';
import { getGroqProvider } from '@unrdf/daemon';

const daemon = new Daemon({
  daemonId: 'agent-daemon-1',
  name: 'Local AI Agent',
});

// Register an agent operation
daemon.registerOperation({
  id: 'analyze-graph',
  description: 'Analyze RDF graph with Groq',
  handler: async payload => {
    const provider = getGroqProvider();
    const model = provider.getDefaultModel();

    const result = await generateText({
      model,
      prompt: `Analyze this knowledge graph: ${JSON.stringify(payload.graph)}`,
      maxTokens: 500,
    });

    return { analysis: result.text };
  },
});

// Schedule or trigger operations
await daemon.run('analyze-graph', {
  graph: {
    /* RDF data */
  },
});
```

---

## Understanding maxDuration (Timeouts)

For longer-running agents, you may need to increase timeouts:

```javascript
// Timeout in milliseconds
const AGENT_TIMEOUT = 30000; // 30 seconds for simple agents
const COMPLEX_TIMEOUT = 120000; // 2 minutes for multi-step agents

const result = await Promise.race([
  generateText({
    model,
    prompt,
    tools,
    stopWhen: stepCountIs(10),
    maxTokens: 2000,
  }),
  new Promise((_, reject) => setTimeout(() => reject(new Error('Agent timeout')), AGENT_TIMEOUT)),
]);
```

Choose a duration based on agent complexity:

| Type                           | Duration | Use Case                               |
| ------------------------------ | -------- | -------------------------------------- |
| Simple (1-3 tool calls)        | 10-30s   | Single query, fact lookup              |
| Moderate (multiple sequential) | 30-120s  | Multi-step reasoning, graph enrichment |
| Complex (extensive reasoning)  | 120-600s | Full knowledge graph analysis          |

---

## Observing Your Agent

### Console Logging

Add logging to understand agent decisions:

```javascript
const result = await generateText({
  model,
  prompt,
  tools,
  stopWhen: stepCountIs(10),
});

console.log('=== AGENT TRACE ===');
result.steps?.forEach((step, i) => {
  console.log(`\n[Step ${i}]`);
  console.log('Role:', step.stepType);
  if (step.toolCalls?.length) {
    console.log(
      'Tool calls:',
      step.toolCalls.map(tc => tc.toolName)
    );
  }
  if (step.text) {
    console.log('Response:', step.text.substring(0, 100) + '...');
  }
});
```

### OTEL Observability

Integrate with OpenTelemetry for production monitoring:

```javascript
import { NodeTracerProvider } from '@opentelemetry/node';
import { ConsoleSpanExporter, SimpleSpanProcessor } from '@opentelemetry/tracing';

const tracerProvider = new NodeTracerProvider();
tracerProvider.addSpanProcessor(new SimpleSpanProcessor(new ConsoleSpanExporter()));

// Your agent code will now emit traces
```

---

## Extending with Hooks

Combine agents with UNRDF knowledge hooks for reactive enrichment:

```javascript
import { KnowledgeHookEngine } from '@unrdf/hooks';
import { generateText } from 'ai';
import { getGroqProvider } from '@unrdf/daemon';

const engine = new KnowledgeHookEngine();
const provider = getGroqProvider();
const model = provider.getDefaultModel();

// Define a hook condition: "if no foaf:name property exists"
const condition = {
  type: 'sparql-ask',
  query: `ASK { ?person a foaf:Person . FILTER NOT EXISTS { ?person foaf:name ?name } }`,
};

// Define a hook effect: use Groq to suggest a name
const effect = {
  type: 'sparql-construct',
  query: async store => {
    // Ask Groq for suggestions
    const suggestion = await generateText({
      model,
      prompt: 'Based on the RDF store, what property should I add?',
      maxTokens: 100,
    });

    return `CONSTRUCT { ?person foaf:name "${suggestion.text}" } WHERE { ?person a foaf:Person }`;
  },
};

// Execute the hook
const result = await engine.execute(store, [{ condition, effect }]);
```

---

## Performance Tips

1. **Batch Tool Calls**: Let Groq decide which tools to use (avoid forcing tool choice)
2. **Cache Responses**: Store LLM responses for common queries
3. **Use Smaller Models**: `openai/gpt-oss-20b` is fast; try `meta-llama/llama-4-scout-17b` for speed
4. **Limit Steps**: Keep `stepCountIs()` low (3-5) for fast agents
5. **Stream Responses**: Use `streamText()` instead of `generateText()` for long outputs

---

## See Also

- [Groq Integration Guide](./GROQ-INTEGRATION.md)
- [UNRDF Daemon Documentation](./README.md)
- [Knowledge Hooks Documentation](../hooks/README.md)
- [AI SDK Reference](https://sdk.vercel.ai/)
- [Groq API Docs](https://console.groq.com/docs)
