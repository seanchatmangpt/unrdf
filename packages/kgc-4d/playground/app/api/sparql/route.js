/**
 * @file packages/kgc-4d/playground/app/api/sparql/route.js
 * @description SPARQL endpoint for KGC-4D Universe
 *
 * POST /api/sparql - Execute SPARQL queries against the Universe
 */

import { NextResponse } from 'next/server';
import { getUniverse } from '../../../lib/server/universe.mjs';

export async function POST(request) {
  try {
    const { query, variables = {} } = await request.json();

    if (!query || typeof query !== 'string') {
      return NextResponse.json(
        { error: 'Missing or invalid SPARQL query' },
        { status: 400 }
      );
    }

    // Get the Universe instance
    const universe = await getUniverse();

    // Replace variables in query (simple implementation)
    let processedQuery = query;
    for (const [key, value] of Object.entries(variables)) {
      const placeholder = `$${key}`;
      processedQuery = processedQuery.replaceAll(placeholder, value);
    }

    // Execute SPARQL query
    // Note: This is a simplified implementation
    // In production, use universe.store.query() or similar

    // For demo purposes, we'll do simple pattern matching
    const results = executeSimpleSPARQL(universe, processedQuery);

    return NextResponse.json({
      success: true,
      results,
      query: processedQuery,
      count: results.length,
      executionTime: 0 // TODO: Add timing
    });

  } catch (error) {
    console.error('[SPARQL] Error:', error);
    return NextResponse.json(
      { error: error.message },
      { status: 500 }
    );
  }
}

/**
 * Simple SPARQL executor (80/20 implementation)
 *
 * Supports basic SELECT queries only
 * TODO: Integrate with Oxigraph SPARQL engine
 */
function executeSimpleSPARQL(universe, query) {
  // Extract SELECT variables
  const selectMatch = query.match(/SELECT\s+(.*?)\s+WHERE/i);
  if (!selectMatch) {
    throw new Error('Only SELECT queries supported');
  }

  const variables = selectMatch[1]
    .split(/\s+/)
    .filter(v => v.startsWith('?'))
    .map(v => v.substring(1));

  // Get all quads from universe
  const shard = universe.getCurrentShard();
  const quads = shard?.quads || [];

  // Simple pattern matching
  // This is 80/20 - works for basic queries
  const results = [];

  // Check for common patterns
  if (query.includes('foaf:Person')) {
    // Find all people
    const people = quads.filter(q =>
      q.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
      q.object.value === 'http://xmlns.com/foaf/0.1/Person'
    );

    for (const typeQuad of people) {
      const subject = typeQuad.subject.value;
      const personQuads = quads.filter(q => q.subject.value === subject);

      const result = {};
      for (const q of personQuads) {
        const predName = q.predicate.value.split('/').pop().split('#').pop();
        if (variables.includes(predName) || variables.includes('*')) {
          result[predName] = q.object.value;
        }
      }

      if (Object.keys(result).length > 0) {
        result.subject = subject;
        results.push(result);
      }
    }
  } else {
    // Generic query - return all matching patterns
    const resultMap = new Map();

    for (const quad of quads) {
      const subject = quad.subject.value;
      if (!resultMap.has(subject)) {
        resultMap.set(subject, { subject });
      }

      const predName = quad.predicate.value.split('/').pop().split('#').pop();
      resultMap.get(subject)[predName] = quad.object.value;
    }

    results.push(...Array.from(resultMap.values()));
  }

  return results;
}
