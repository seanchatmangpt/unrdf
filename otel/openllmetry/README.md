# OpenLLMetry Instrumentation

OpenTelemetry instrumentation for AI/LLM calls in the UNRDF stack.

Provides structured tracing with GenAI semantic conventions for all LLM interactions, enabling end-to-end visibility from user request through model response.

## Quick Start

```javascript
import { withLLMSpan, getAISDKTelemetryConfig } from './otel/openllmetry/instrumentation.mjs';

// Wrap any LLM call with tracing
const result = await withLLMSpan({
  provider: 'groq',
  model: 'llama-3.3-70b-versatile',
  operation: 'generate',
  handler: async span => {
    const response = await groq.chat.completions.create({
      model: 'llama-3.3-70b-versatile',
      messages: [{ role: 'user', content: 'Hello' }],
    });
    span.setAttributes({ 'gen_ai.request.max_tokens': 1024 });
    return {
      model: response.model,
      usage: response.usage,
      finish_reason: response.choices[0].finish_reason,
    };
  },
});
```

## AI SDK Integration

The Vercel AI SDK (`ai` package) has built-in telemetry support. Use the helper to configure it:

```javascript
import { generateText } from 'ai';
import { getAISDKTelemetryConfig } from './otel/openllmetry/instrumentation.mjs';

const result = await generateText({
  model: groq('llama-3.3-70b-versatile'),
  prompt: 'Analyze this knowledge graph...',
  experimental_telemetry: getAISDKTelemetryConfig({
    functionId: 'kg-reasoning',
    metadata: { graphId: 'entity-store' },
  }),
});
```

## Instrumenting Tool Calls

For AI SDK tool calls, use the tool call wrapper:

```javascript
import { instrumentAIToolCall } from './otel/openllmetry/instrumentation.mjs';
import { tool } from 'ai';

const weatherTool = tool({
  description: 'Get weather for a location',
  parameters: z.object({ location: z.string() }),
  execute: instrumentAIToolCall('get_weather', async ({ location }) => {
    // Tool implementation
    return { temperature: 72, condition: 'sunny' };
  }),
});
```

## Semantic Conventions

Spans use GenAI semantic conventions from the OpenTelemetry specification:

| Attribute                        | Example                       |
| -------------------------------- | ----------------------------- |
| `gen_ai.system`                  | `groq`, `openai`, `anthropic` |
| `gen_ai.request.model`           | `llama-3.3-70b-versatile`     |
| `gen_ai.response.model`          | `llama-3.3-70b-versatile`     |
| `gen_ai.usage.input_tokens`      | `142`                         |
| `gen_ai.usage.output_tokens`     | `89`                          |
| `gen_ai.response.finish_reasons` | `['stop']`                    |

## API Reference

### `getLLMTracer(name?)`

Returns an OpenTelemetry tracer for LLM operations. Defaults to `unrdf-llm`.

### `withLLMSpan({ provider, model, operation, handler, attributes? })`

Wraps an LLM call in an OpenTelemetry span with GenAI attributes. Automatically records token usage, finish reasons, and errors.

### `getAISDKTelemetryConfig({ functionId?, metadata? })`

Returns the `experimental_telemetry` config object for the Vercel AI SDK.

### `instrumentAIToolCall(toolName, handler, options?)`

Wraps an AI SDK tool handler with per-call tracing.

### `isInstrumented()` / `setInstrumented()`

Health check helpers for verifying instrumentation status.

## Integration Points

This module is used by:

- **`packages/daemon/src/autonomous-agent.mjs`** — Knowledge agent LLM reasoning
- **`packages/daemon/src/autonomous-refinement-engine.mjs`** — Graph refinement
- **`packages/docs/server/api/chat.post.ts`** — Chat API endpoints
- **`packages/docs/server/api/chats/[id].post.ts`** — Chat continuation

## Backend Requirements

The OpenTelemetry SDK must be initialized before using this module. The daemon's existing OTEL setup (`packages/daemon/src/otel-sdk.mjs`) configures the OTLP exporter to `localhost:4317`.

For standalone usage:

```javascript
import { NodeSDK } from '@opentelemetry/sdk-node';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-grpc';
import { setInstrumented } from './otel/openllmetry/instrumentation.mjs';

const sdk = new NodeSDK({
  traceExporter: new OTLPTraceExporter({ url: 'http://localhost:4317' }),
});
await sdk.start();
setInstrumented();
```
