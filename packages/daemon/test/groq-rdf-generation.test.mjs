#!/usr/bin/env node
/**
 * @file 10-second Groq LLM RDF Generation Test
 * @description Uses Vercel AI SDK + Groq to generate RDF triples from ontology patterns
 */

import { createStore, namedNode, literal, quad, getQuads } from '../packages/core/src/index.mjs';

// Simple Groq client (no Vercel AI SDK needed for basic usage)
const GROQ_API_URL = 'https://api.groq.com/openai/v1/chat/completions';

// Configuration
const DURATION_SECONDS = 10;
const MAX_ITERATIONS = Infinity;

// Ontology patterns for RDF generation
const ONTOLOGY_PATTERNS = [
  {
    name: 'dc',
    prefix: 'http://purl.org/dc/terms/',
    predicates: ['title', 'description', 'creator', 'date', 'source', 'subject']
  },
  {
    name: 'rdf',
    prefix: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    predicates: ['type', 'value', 'subject', 'predicate', 'object']
  },
  {
    name: 'rdfs',
    prefix: 'http://www.w3.org/2000/01/rdf-schema#',
    predicates: ['label', 'comment', 'domain', 'range', 'subClassOf']
  },
  {
    name: 'foaf',
    prefix: 'http://xmlns.com/foaf/0.1/',
    predicates: ['name', 'knows', 'mbox', 'homepage', 'Person']
  },
  {
    name: 'schema',
    prefix: 'https://schema.org/',
    predicates: ['name', 'description', 'url', 'author', 'datePublished']
  }
];

/**
 * Generate RDF triples using Groq LLM
 */
async function groqRdfGenerationHook(store) {
  const apiKey = process.env.GROQ_API_KEY;

  if (!apiKey) {
    return {
      hookId: 'groq-rdf-generation',
      success: false,
      result: { error: 'GROQ_API_KEY not set' },
      storeChanged: false,
    };
  }

  // Select random ontology pattern
  const ontology = ONTOLOGY_PATTERNS[Math.floor(Math.random() * ONTOLOGY_PATTERNS.length)];
  const predicate = ontology.predicates[Math.floor(Math.random() * ontology.predicates.length)];

  try {
    const prompt = `You are an RDF knowledge graph generator. Create 1-3 realistic RDF triples using the ${ontology.name} ontology.

Pattern: ${ontology.prefix}
Predicate: ${ontology.prefix}${predicate}

Generate triples in this exact JSON format (no markdown, no explanation):
{
  "triples": [
    {"subject": "http://example.org/resource1", "predicate": "${ontology.prefix}${predicate}", "object": "Example value 1"},
    {"subject": "http://example.org/resource2", "predicate": "${ontology.prefix}${predicate}", "object": "Example value 2"}
  ]
}

Only respond with valid JSON. No other text.`;

    const response = await fetch(GROQ_API_URL, {
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

    if (!response.ok) {
      return {
        hookId: 'groq-rdf-generation',
        success: false,
        result: { error: `HTTP ${response.status}` },
        storeChanged: false,
      };
    }

    const data = await response.json();
    const content = data.choices[0].message.content.trim();

    // Parse JSON from LLM response (handle markdown code blocks)
    const jsonMatch = content.match(/```(?:json)?\s*(\{[\s\S]*?\})\s*```/) || content.match(/(\{[\s\S]*\})/);
    if (!jsonMatch) {
      return {
        hookId: 'groq-rdf-generation',
        success: false,
        result: { error: 'No JSON in response', content: content.substring(0, 100) },
        storeChanged: false,
      };
    }

    const parsed = JSON.parse(jsonMatch[1]);
    let quadsAdded = 0;

    // Add triples to store
    for (const triple of parsed.triples || []) {
      const q = quad(
        namedNode(triple.subject),
        namedNode(triple.predicate),
        literal(String(triple.object))
      );
      store.add(q);
      quadsAdded++;
    }

    return {
      hookId: 'groq-rdf-generation',
      success: true,
      result: {
        ontology: ontology.name,
        predicate: predicate,
        triplesGenerated: quadsAdded
      },
      storeChanged: quadsAdded > 0,
    };

  } catch (error) {
    return {
      hookId: 'groq-rdf-generation',
      success: false,
      result: { error: error.message },
      storeChanged: false,
    };
  }
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
  console.log('🚀 Starting 10-second Groq LLM RDF Generation Test');
  console.log(`Duration: ${DURATION_SECONDS} seconds`);
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
    console.log('🏃 Starting Groq-powered RDF generation...');
    console.log('');

    const startTime = Date.now();
    const endTime = startTime + (DURATION_SECONDS * 1000);

    let iteration = 0;
    let converged = false;
    let previousHash = computeStoreHash(store);

    // Track Groq statistics
    let totalTriplesGenerated = 0;
    let successfulCalls = 0;

    // Main loop
    while (Date.now() < endTime && !converged && iteration < MAX_ITERATIONS) {
      iteration++;

      const hooks = [
        incrementCounterHook,
        groqRdfGenerationHook,
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

          if (result.hookId === 'groq-rdf-generation' && result.success) {
            totalTriplesGenerated += result.result.triplesGenerated || 0;
            successfulCalls++;
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

      console.log(
        `[${new Date().toISOString()}] Iteration ${iteration}: ` +
        `changed=${storeChanged}, ` +
        `hooks=${hooksExecuted}, ` +
        `counter=${counterValue}, ` +
        `hash=${currentHash.substring(0, 8)}..., ` +
        `groq=${totalTriplesGenerated} triples, ` +
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

    const totalDuration = Date.now() - startTime;
    console.log('');
    console.log('🎉 Test complete!');
    console.log(`   Total iterations: ${iteration}`);
    console.log(`   Total duration: ${(totalDuration / 1000).toFixed(1)} seconds`);
    console.log(`   Converged: ${converged}`);
    console.log(`   Groq calls: ${successfulCalls} successful`);
    console.log(`   Triples generated: ${totalTriplesGenerated}`);
    console.log(`   Final store size: ${getQuads(store).length} quads`);
    console.log('');

  } catch (error) {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  }
}

main().catch(error => {
  console.error('Unhandled error:', error);
  process.exit(1);
});
