#!/usr/bin/env node
/**
 * @file 10-second Groq RDF Generation with OpenTelemetry Tracing
 * @description Uses OTEL to prove Groq API calls are actually being made
 */

import { createStore, namedNode, literal, quad, getQuads } from '../packages/core/src/index.mjs';
import { NodeTracerProvider } from '@opentelemetry/sdk-trace-node';
import opentelemetryResources from '@opentelemetry/resources';
import { SemanticResourceAttributes } from '@opentelemetry/semantic-conventions';
import { SpanStatusCode } from '@opentelemetry/api';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-http';

// Configuration
const DURATION_SECONDS = 10;
const MAX_ITERATIONS = Infinity;
const GROQ_API_URL = 'https://api.groq.com/openai/v1/chat/completions';

// Initialize OpenTelemetry
const resource = opentelemetryResources.resourceFromAttributes({
  [SemanticResourceAttributes.SERVICE_NAME]: 'unrdf-groq-test',
  [SemanticResourceAttributes.SERVICE_VERSION]: '26.4.5',
});

const traceExporter = new OTLPTraceExporter({
  url: 'http://localhost:4318/v1/traces', // Default OTEL collector endpoint
});

const provider = new NodeTracerProvider({
  resource,
  traceExporter,
});

provider.register();
const tracer = provider.getTracer('groq-rdf-generation');

/**
 * Groq LLM RDF generation with OTEL tracing
 */
async function groqRdfGenerationTraced(store) {
  const apiKey = process.env.GROQ_API_KEY;

  if (!apiKey) {
    return {
      hookId: 'groq-rdf-traced',
      success: false,
      result: { error: 'GROQ_API_KEY not set' },
      storeChanged: false,
    };
  }

  // Create a span for the entire Groq operation
  return await tracer.startActiveSpan('groq.rdf.generation', async (span) => {
    try {
      // Add attributes to the span
      span.setAttribute('groq.model', 'llama-3.3-70b-versatile');
      span.setAttribute('groq.api_endpoint', GROQ_API_URL);

      const startTime = Date.now();

      const prompt = `You are an RDF knowledge graph generator. Create 1-3 realistic RDF triples.

Generate triples in this exact JSON format (no markdown):
{
  "triples": [
    {"subject": "http://example.org/resource1", "predicate": "http://purl.org/dc/terms/title", "object": "Example Resource 1"},
    {"subject": "http://example.org/resource2", "predicate": "http://purl.org/dc/terms/description", "object": "Example Description 2"}
  ]
}

Only respond with valid JSON. No other text.`;

      // Create a span for the HTTP request
      const response = await tracer.startActiveSpan('groq.http.request', async (httpSpan) => {
        try {
          httpSpan.setAttribute('http.method', 'POST');
          httpSpan.setAttribute('http.url', GROQ_API_URL);

          const fetchStart = Date.now();

          const res = await fetch(GROQ_API_URL, {
            method: 'POST',
            headers: {
              'Authorization': `Bearer ${apiKey}`,
              'Content-Type': 'application/json',
            },
            body: JSON.stringify({
              model: 'llama-3.3-70b-versatile',
              messages: [
                { role: 'user', content: prompt }
              ],
              temperature: 0.7,
              max_tokens: 500,
            }),
          });

          const fetchDuration = Date.now() - fetchStart;
          httpSpan.setAttribute('http.response_code', res.status);
          httpSpan.setAttribute('http.request_duration_ms', fetchDuration);

          if (!res.ok) {
            httpSpan.setStatus({
              code: SpanStatusCode.ERROR,
              message: `HTTP ${res.status}`,
            });
            throw new Error(`HTTP ${res.status}`);
          }

          return res;
        } catch (error) {
          httpSpan.recordException(error);
          throw error;
        }
      });

      const data = await response.json();
      const content = data.choices[0].message.content.trim();

      // Create a span for JSON parsing
      const parsed = await tracer.startActiveSpan('json.parse', async (parseSpan) => {
        try {
          const jsonMatch = content.match(/```(?:json)?\s*(\{[\s\S]*?\})\s*```/) || content.match(/(\{[\s\S]*\})/);
          if (!jsonMatch) {
            throw new Error('No JSON in response');
          }
          return JSON.parse(jsonMatch[1]);
        } catch (error) {
          parseSpan.recordException(error);
          throw error;
        }
      });

      let quadsAdded = 0;

      // Create a span for RDF operations
      await tracer.startActiveSpan('rdf.store.add', async (rdfSpan) => {
        for (const triple of parsed.triples || []) {
          const q = quad(
            namedNode(triple.subject),
            namedNode(triple.predicate),
            literal(String(triple.object))
          );
          store.add(q);
          quadsAdded++;
        }
        rdfSpan.setAttribute('rdf.quads_added', quadsAdded);
      });

      const totalTime = Date.now() - startTime;

      span.setAttribute('groq.triples_generated', quadsAdded);
      span.setAttribute('groq.total_duration_ms', totalTime);
      span.setStatus({ code: SpanStatusCode.OK });

      return {
        hookId: 'groq-rdf-traced',
        success: true,
        result: {
          triplesGenerated: quadsAdded,
          durationMs: totalTime,
          model: 'llama-3.3-70b-versatile'
        },
        storeChanged: quadsAdded > 0,
      };

    } catch (error) {
      span.recordException(error);
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
      throw error;
    }
  });
}

/**
 * Increment counter hook
 */
async function incrementCounterHook(store) {
  const currentQuads = getQuads(
    store,
    namedNode('http://example.org/counter'),
    namedNode('http://example.org/iterations')
  );

  const currentValue = currentQuads.length > 0
    ? parseInt(currentQuads[0].object.value)
    : 0;
  const newValue = currentValue + 1;

  for (const q of currentQuads) {
    store.delete(q);
  }

  const newQuad = quad(
    namedNode('http://example.org/counter'),
    namedNode('http://example.org/iterations'),
    literal(String(newValue))
  );
  store.add(newQuad);

  return {
    hookId: 'counter-increment',
    success: true,
    result: { oldValue: currentValue, newValue },
    storeChanged: true,
  };
}

/**
 * Compute hash of store state
 */
function computeStoreHash(store) {
  const quads = getQuads(store);
  if (quads.length === 0) return 'empty';

  const quadStrings = quads.map(q =>
    `${q.subject.value}:${q.predicate.value}:${q.object.value}`
  ).join('|');

  let hash = 0;
  for (let i = 0; i < quadStrings.length; i++) {
    const char = quadStrings.charCodeAt(i);
    hash = (hash << 5) - hash + char;
    hash = hash & hash;
  }

  return Math.abs(hash).toString(16);
}

async function main() {
  console.log('🚀 Starting 10-second Groq RDF Generation with OTEL Tracing');
  console.log(`Duration: ${DURATION_SECONDS} seconds`);
  console.log(`OTEL Exporter: http://localhost:4318/v1/traces`);
  console.log('');

  // Check for API key
  if (!process.env.GROQ_API_KEY) {
    console.error('❌ GROQ_API_KEY environment variable not set');
    console.error('Get your key at: https://console.groq.com/keys');
    process.exit(1);
  }

  try {
    console.log('📦 Initializing knowledge substrate...');
    const store = createStore();

    console.log('📊 Initializing counter...');
    const counterQuad = quad(
      namedNode('http://example.org/counter'),
      namedNode('http://example.org/iterations'),
      literal('0')
    );
    store.add(counterQuad);

    console.log('✅ Loop initialized successfully');
    console.log('🏃 Starting Groq-powered RDF generation with OTEL tracing...');
    console.log('');

    const startTime = Date.now();
    const endTime = startTime + (DURATION_SECONDS * 1000);

    let iteration = 0;
    let converged = false;
    let previousHash = computeStoreHash(store);

    // Track Groq statistics
    let totalTriplesGenerated = 0;
    let successfulCalls = 0;
    let totalGroqDuration = 0;

    // Main loop
    while (Date.now() < endTime && !converged && iteration < MAX_ITERATIONS) {
      iteration++;

      const hooks = [
        incrementCounterHook,
        groqRdfGenerationTraced,
      ];

      let storeChanged = false;
      let hooksExecuted = 0;

      for (const hook of hooks) {
        try {
          const result = await hook(store);
          if (result.storeChanged) {
            storeChanged = true;
          }
          hooksExecuted++;

          if (result.hookId === 'groq-rdf-traced' && result.success) {
            totalTriplesGenerated += result.result.triplesGenerated || 0;
            successfulCalls++;
            totalGroqDuration += result.result.durationMs || 0;
          }

        } catch (err) {
          console.error(`   Hook ${hook.name} failed:`, err.message);
        }
      }

      const elapsed = Date.now() - startTime;
      const remaining = endTime - Date.now();
      const progress = (elapsed / (DURATION_SECONDS * 1000)) * 100;

      const currentHash = computeStoreHash(store);

      const counterQuads = getQuads(
        store,
        namedNode('http://example.org/counter'),
        namedNode('http://example.org/iterations')
      );
      const counterValue = counterQuads.length > 0
        ? counterQuads[0].object.value
        : 0;

      const avgDuration = successfulCalls > 0 ? (totalGroqDuration / successfulCalls).toFixed(0) : 0;

      console.log(
        `[${new Date().toISOString()}] Iteration ${iteration}: ` +
        `changed=${storeChanged}, ` +
        `hooks=${hooksExecuted}, ` +
        `counter=${counterValue}, ` +
        `hash=${currentHash.substring(0, 8)}..., ` +
        `groq=${totalTriplesGenerated} triples, ` +
        `calls=${successfulCalls}, ` +
        `avg=${avgDuration}ms, ` +
        `progress=${progress.toFixed(1)}%, ` +
        `remaining=${Math.ceil(remaining / 1000)}s`
      );

      if (currentHash === previousHash) {
        console.log('');
        console.log('✅ Graph converged - stopping early');
        converged = true;
        break;
      }

      previousHash = currentHash;
    }

    // Flush spans before exiting
    console.log('');
    console.log('📊 Flushing OTEL spans...');
    await provider.shutdown().catch((err) => console.warn('Failed to shutdown tracer:', err.message));

    const totalDuration = Date.now() - startTime;
    console.log('');
    console.log('🎉 Test complete!');
    console.log(`   Total iterations: ${iteration}`);
    console.log(`   Total duration: ${(totalDuration / 1000).toFixed(1)} seconds`);
    console.log(`   Converged: ${converged}`);
    console.log(`   Groq calls: ${successfulCalls} successful`);
    console.log(`   Total Groq duration: ${totalGroqDuration.toFixed(0)}ms`);
    console.log(`   Average per call: ${(totalGroqDuration / successfulCalls).toFixed(0)}ms`);
    console.log(`   Triples generated: ${totalTriplesGenerated}`);
    console.log(`   Final store size: ${getQuads(store).length} quads`);
    console.log('');
    console.log('📈 OTEL Spans:');
    console.log('   Service: unrdf-groq-test');
    console.log('   Span names: groq.rdf.generation, groq.http.request, json.parse, rdf.store.add');
    console.log('   View in Jaeger: http://localhost:16686');
    console.log('');

  } catch (error) {
    console.error('❌ Fatal error:', error);
    await provider.shutdown().catch(() => {});
    process.exit(1);
  }
}

main().catch(error => {
  console.error('Unhandled error:', error);
  process.exit(1);
});
