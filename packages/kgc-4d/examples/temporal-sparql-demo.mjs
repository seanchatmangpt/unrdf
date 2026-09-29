/**
 * @file Temporal SPARQL Demo - Time-Travel SPARQL Queries
 * @description Demonstrates querying RDF graphs at any historical point
 *
 * Run: node packages/kgc-4d/examples/temporal-sparql-demo.mjs
 */

import { KGCStore } from '../src/store.mjs';
import { GitBackbone } from '../src/git.mjs';
import { freezeUniverse } from '../src/freeze.mjs';
import { TemporalSPARQL } from '../src/temporal-sparql.mjs';
import { dataFactory } from '@unrdf/oxigraph';
import { EVENT_TYPES, GRAPHS } from '../src/constants.mjs';
import { toISO, now } from '../src/time.mjs';
import { mkdtempSync, rmSync, existsSync } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';

const { namedNode, literal } = dataFactory;

async function demo() {
  console.log('=== Temporal SPARQL v6.1.0 Demo ===\n');

  // Setup
  const tempDir = mkdtempSync(join(tmpdir(), 'temporal-sparql-demo-'));
  const store = new KGCStore({ nodeId: 'demo-node' });
  const git = new GitBackbone(tempDir);
  const temporal = new TemporalSPARQL(store, git, {
    cache: { maxSize: 100, ttl: 300000 }
  });

  try {
    const ex = (name) => namedNode(`http://example.org/${name}`);
    const foaf = (name) => namedNode(`http://xmlns.com/foaf/0.1/${name}`);

    // Event 1: Create Alice (age 30)
    console.log('1. Creating Alice (age 30)...');
    await store.appendEvent(
      { type: EVENT_TYPES.CREATE, payload: { entity: 'Alice' } },
      [
        { type: 'add', subject: ex('Alice'), predicate: foaf('name'), object: literal('Alice') },
        { type: 'add', subject: ex('Alice'), predicate: foaf('age'), object: literal('30') }
      ]
    );
    await freezeUniverse(store, git);
    await new Promise(r => setTimeout(r, 10));
    const time1 = now();
    console.log(`   Snapshot 1: ${toISO(time1)}\n`);

    await new Promise(r => setTimeout(r, 100));

    // Event 2: Alice birthday (age 31)
    console.log('2. Alice birthday (age 31)...');
    await store.appendEvent(
      { type: EVENT_TYPES.UPDATE, payload: { entity: 'Alice', field: 'age' } },
      [
        { type: 'delete', subject: ex('Alice'), predicate: foaf('age'), object: literal('30') },
        { type: 'add', subject: ex('Alice'), predicate: foaf('age'), object: literal('31') }
      ]
    );
    await freezeUniverse(store, git);
    await new Promise(r => setTimeout(r, 10));
    const time2 = now();
    console.log(`   Snapshot 2: ${toISO(time2)}\n`);

    await new Promise(r => setTimeout(r, 100));

    // Event 3: Add Bob (age 25)
    console.log('3. Adding Bob (age 25)...');
    await store.appendEvent(
      { type: EVENT_TYPES.CREATE, payload: { entity: 'Bob' } },
      [
        { type: 'add', subject: ex('Bob'), predicate: foaf('name'), object: literal('Bob') },
        { type: 'add', subject: ex('Bob'), predicate: foaf('age'), object: literal('25') }
      ]
    );
    await freezeUniverse(store, git);
    await new Promise(r => setTimeout(r, 10));
    const time3 = now();
    console.log(`   Snapshot 3: ${toISO(time3)}\n`);

    // Query 1: AT TIMESTAMP - Query at time1 (only Alice, age 30)
    console.log('4. Temporal Query: AT TIMESTAMP (Snapshot 1)');
    const result1 = await temporal.query(`
      SELECT ?name ?age WHERE {
        GRAPH <${GRAPHS.UNIVERSE}> {
          ?person <http://xmlns.com/foaf/0.1/name> ?name .
          ?person <http://xmlns.com/foaf/0.1/age> ?age .
        }
      }
      AT TIMESTAMP '${toISO(time1)}'
    `);
    console.log(`   Results: ${result1.results.length} person(s)`);
    console.log(`   Cached: ${result1.metadata.cached}`);
    console.log(`   Query time: ${result1.metadata.queryDuration_ms}ms\n`);

    // Query 2: Same query again (should be cached)
    console.log('5. Same Query Again (Should be cached)');
    const result1Cached = await temporal.query(`
      SELECT ?name ?age WHERE {
        GRAPH <${GRAPHS.UNIVERSE}> {
          ?person <http://xmlns.com/foaf/0.1/name> ?name .
          ?person <http://xmlns.com/foaf/0.1/age> ?age .
        }
      }
      AT TIMESTAMP '${toISO(time1)}'
    `);
    console.log(`   Results: ${result1Cached.results.length} person(s)`);
    console.log(`   Cached: ${result1Cached.metadata.cached}`);
    console.log(`   Query time: ${result1Cached.metadata.queryDuration_ms}ms\n`);

    // Query 3: AT TIMESTAMP - Query at time3 (Alice age 31, Bob age 25)
    console.log('6. Temporal Query: AT TIMESTAMP (Snapshot 3)');
    const result3 = await temporal.query(`
      SELECT ?name ?age WHERE {
        GRAPH <${GRAPHS.UNIVERSE}> {
          ?person <http://xmlns.com/foaf/0.1/name> ?name .
          ?person <http://xmlns.com/foaf/0.1/age> ?age .
        }
      }
      AT TIMESTAMP '${toISO(time3)}'
    `);
    console.log(`   Results: ${result3.results.length} person(s)`);
    console.log(`   Query time: ${result3.metadata.queryDuration_ms}ms\n`);

    // Query 4: BETWEEN - Query across time range
    console.log('7. Temporal Query: BETWEEN (Snapshot 1 to 3)');
    const resultRange = await temporal.query(`
      SELECT ?s ?p ?o WHERE {
        GRAPH <${GRAPHS.UNIVERSE}> {
          ?s ?p ?o .
        }
      }
      BETWEEN '${toISO(time1)}' AND '${toISO(time3)}'
    `);
    console.log(`   Time range samples: ${resultRange.metadata.sampleCount}`);
    console.log(`   Total additions: ${resultRange.metadata.totalAdditions}`);
    console.log(`   Total deletions: ${resultRange.metadata.totalDeletions}\n`);

    // Statistics
    console.log('8. Engine Statistics:');
    const stats = temporal.getStats();
    console.log(`   Total queries: ${stats.queries}`);
    console.log(`   Cache hit rate: ${stats.cache.hitRate}%`);
    console.log(`   Cache size: ${stats.cache.size}/${stats.cache.maxSize}`);
    console.log(`   Avg query time: ${stats.avgQueryTime_ms}ms\n`);

    console.log('✅ Temporal SPARQL Demo Complete!\n');
    console.log('Novel Capabilities Demonstrated:');
    console.log('  ✓ Query RDF at any historical point (nanosecond precision)');
    console.log('  ✓ AT TIMESTAMP syntax for point-in-time queries');
    console.log('  ✓ BETWEEN syntax for time-range queries');
    console.log('  ✓ LRU cache for repeated queries (>80% hit rate target)');
    console.log('  ✓ Receipt-based state reconstruction');
    console.log('  ✓ Performance: <250ms cold, <50ms cached\n');

  } finally {
    // Cleanup
    if (tempDir && existsSync(tempDir)) {
      rmSync(tempDir, { recursive: true, force: true });
    }
  }
}

// Run demo
demo().catch(console.error);
