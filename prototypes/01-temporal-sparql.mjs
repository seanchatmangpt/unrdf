/**
 * @file Prototype 1: Temporal SPARQL Queries
 * @description Query RDF graphs at any point in history with nanosecond precision
 *
 * Novel Pattern: Combine KGC-4D time-travel with SPARQL to enable
 * cross-temporal queries and historical analysis.
 *
 * Performance: O(n) snapshot load + O(k) delta replay
 *
 * Run: node prototypes/01-temporal-sparql.mjs
 */

import { KGCStore } from '../packages/kgc-4d/src/store.mjs';
import { GitBackbone } from '../packages/kgc-4d/src/git.mjs';
import { freezeUniverse, reconstructState } from '../packages/kgc-4d/src/freeze.mjs';
import { dataFactory } from '@unrdf/oxigraph';
import { GRAPHS, EVENT_TYPES } from '../packages/kgc-4d/src/constants.mjs';
import { now, toISO } from '../packages/kgc-4d/src/time.mjs';

const { namedNode, literal, quad } = dataFactory;

/**
 * Temporal SPARQL Query Engine
 *
 * Enables querying RDF at specific points in time
 */
class TemporalSPARQLEngine {
  constructor(store, git) {
    this.store = store;
    this.git = git;
  }

  /**
   * Query at a specific point in time
   *
   * @param {string} sparql - SPARQL query
   * @param {bigint} targetTime - Time in nanoseconds
   * @returns {Promise<Array>} Query results
   */
  async queryAtTime(sparql, targetTime) {
    const startTime = Date.now();

    // Reconstruct state at target time
    const pastStore = await reconstructState(this.store, this.git, targetTime);

    // Execute query on historical state
    const results = await pastStore.query(sparql);

    const duration = Date.now() - startTime;

    return {
      results,
      metadata: {
        targetTime: targetTime.toString(),
        targetTimeISO: toISO(targetTime),
        queryDuration_ms: duration,
        resultCount: results.length
      }
    };
  }

  /**
   * Query across multiple time points
   *
   * Returns time-series of query results
   *
   * @param {string} sparql - SPARQL query
   * @param {Array<bigint>} timePoints - Array of time points
   * @returns {Promise<Array>} Time-series results
   */
  async queryAcrossTime(sparql, timePoints) {
    const timeSeries = [];

    for (const timeNs of timePoints) {
      const { results, metadata } = await this.queryAtTime(sparql, timeNs);
      timeSeries.push({
        timestamp: metadata.targetTimeISO,
        timestamp_ns: timeNs,
        results,
        count: results.length
      });
    }

    return {
      timeSeries,
      metadata: {
        timePoints: timePoints.length,
        totalResults: timeSeries.reduce((sum, t) => sum + t.count, 0)
      }
    };
  }

  /**
   * Track entity evolution over time
   *
   * @param {string} entityUri - Entity to track
   * @param {Array<bigint>} timePoints - Time points to sample
   * @returns {Promise<Object>} Entity evolution
   */
  async trackEntityEvolution(entityUri, timePoints) {
    const evolution = [];

    for (const timeNs of timePoints) {
      const { results } = await this.queryAtTime(
        `
        SELECT ?p ?o WHERE {
          GRAPH <${GRAPHS.UNIVERSE}> {
            <${entityUri}> ?p ?o .
          }
        }
        ORDER BY ?p
        `,
        timeNs
      );

      const properties = {};
      for (const binding of results) {
        properties[binding.p.value] = binding.o.value;
      }

      evolution.push({
        timestamp: toISO(timeNs),
        timestamp_ns: timeNs,
        properties,
        propertyCount: Object.keys(properties).length
      });
    }

    return {
      entity: entityUri,
      evolution,
      snapshots: evolution.length
    };
  }

  /**
   * Diff between two time points
   *
   * @param {bigint} time1 - Earlier time
   * @param {bigint} time2 - Later time
   * @returns {Promise<Object>} Diff result
   */
  async diff(time1, time2) {
    // Reconstruct both states
    const state1 = await reconstructState(this.store, this.git, time1);
    const state2 = await reconstructState(this.store, this.git, time2);

    // Get all quads from both
    const quads1 = new Set([...state1.match(null, null, null, namedNode(GRAPHS.UNIVERSE))]
      .map(q => this.quadKey(q)));
    const quads2 = new Set([...state2.match(null, null, null, namedNode(GRAPHS.UNIVERSE))]
      .map(q => this.quadKey(q)));

    // Calculate additions and deletions
    const additions = [];
    const deletions = [];

    for (const key of quads2) {
      if (!quads1.has(key)) {
        additions.push(key);
      }
    }

    for (const key of quads1) {
      if (!quads2.has(key)) {
        deletions.push(key);
      }
    }

    return {
      time1: toISO(time1),
      time2: toISO(time2),
      additions: additions.length,
      deletions: deletions.length,
      additionsList: additions,
      deletionsList: deletions
    };
  }

  quadKey(quad) {
    return `${quad.subject.value}|${quad.predicate.value}|${quad.object.value}`;
  }
}

/**
 * Demo: Time-travel SPARQL queries
 */
async function demo() {
  console.log('=== Temporal SPARQL Prototype ===\n');

  // Initialize store and git
  const store = new KGCStore({ nodeId: 'temporal-demo' });
  const git = new GitBackbone('./temporal-demo-repo');

  const ex = (name) => namedNode(`http://example.org/${name}`);
  const foaf = (name) => namedNode(`http://xmlns.com/foaf/0.1/${name}`);

  console.log('1. Creating timeline with 3 snapshots...\n');

  // Event 1: Alice created (age 30)
  const time1 = now();
  await store.appendEvent(
    { type: EVENT_TYPES.CREATE, payload: { entity: 'Alice' } },
    [
      {
        type: 'add',
        subject: ex('Alice'),
        predicate: foaf('name'),
        object: literal('Alice Smith')
      },
      {
        type: 'add',
        subject: ex('Alice'),
        predicate: foaf('age'),
        object: literal('30')
      }
    ]
  );
  await freezeUniverse(store, git);
  console.log(`  Snapshot 1: Alice created (age 30) at ${toISO(time1)}`);

  // Wait 100ms
  await new Promise(r => setTimeout(r, 100));

  // Event 2: Alice birthday (age 31)
  const time2 = now();
  await store.appendEvent(
    { type: EVENT_TYPES.UPDATE, payload: { entity: 'Alice', field: 'age' } },
    [
      {
        type: 'delete',
        subject: ex('Alice'),
        predicate: foaf('age'),
        object: literal('30')
      },
      {
        type: 'add',
        subject: ex('Alice'),
        predicate: foaf('age'),
        object: literal('31')
      }
    ]
  );
  await freezeUniverse(store, git);
  console.log(`  Snapshot 2: Alice birthday (age 31) at ${toISO(time2)}`);

  // Wait 100ms
  await new Promise(r => setTimeout(r, 100));

  // Event 3: Add Bob (age 25)
  const time3 = now();
  await store.appendEvent(
    { type: EVENT_TYPES.CREATE, payload: { entity: 'Bob' } },
    [
      {
        type: 'add',
        subject: ex('Bob'),
        predicate: foaf('name'),
        object: literal('Bob Jones')
      },
      {
        type: 'add',
        subject: ex('Bob'),
        predicate: foaf('age'),
        object: literal('25')
      }
    ]
  );
  await freezeUniverse(store, git);
  console.log(`  Snapshot 3: Bob created (age 25) at ${toISO(time3)}\n`);

  // Create temporal query engine
  const engine = new TemporalSPARQLEngine(store, git);

  // Query 1: "Who existed at time1?"
  console.log('2. Query at Time 1 (only Alice, age 30):\n');
  const { results: r1, metadata: m1 } = await engine.queryAtTime(
    `
    SELECT ?person ?name ?age WHERE {
      GRAPH <${GRAPHS.UNIVERSE}> {
        ?person <http://xmlns.com/foaf/0.1/name> ?name .
        ?person <http://xmlns.com/foaf/0.1/age> ?age .
      }
    }
    `,
    time1
  );

  console.log(`  Found ${r1.length} person(s) at ${m1.targetTimeISO}:`);
  r1.forEach(p => {
    console.log(`    - ${p.name.value}, age ${p.age.value}`);
  });
  console.log();

  // Query 2: "Who existed at time3?"
  console.log('3. Query at Time 3 (Alice and Bob):\n');
  const { results: r3, metadata: m3 } = await engine.queryAtTime(
    `
    SELECT ?person ?name ?age WHERE {
      GRAPH <${GRAPHS.UNIVERSE}> {
        ?person <http://xmlns.com/foaf/0.1/name> ?name .
        ?person <http://xmlns.com/foaf/0.1/age> ?age .
      }
    }
    `,
    time3
  );

  console.log(`  Found ${r3.length} person(s) at ${m3.targetTimeISO}:`);
  r3.forEach(p => {
    console.log(`    - ${p.name.value}, age ${p.age.value}`);
  });
  console.log();

  // Query 3: Track Alice across time
  console.log('4. Track Alice evolution across all 3 snapshots:\n');
  const aliceEvolution = await engine.trackEntityEvolution(
    'http://example.org/Alice',
    [time1, time2, time3]
  );

  console.log(`  Alice's property changes:`);
  aliceEvolution.evolution.forEach((snapshot, idx) => {
    console.log(`    Snapshot ${idx + 1} (${snapshot.timestamp}):`);
    Object.entries(snapshot.properties).forEach(([prop, value]) => {
      const shortProp = prop.split('/').pop();
      console.log(`      ${shortProp}: ${value}`);
    });
  });
  console.log();

  // Query 4: Time-series analysis
  console.log('5. Count people at each snapshot:\n');
  const { timeSeries } = await engine.queryAcrossTime(
    `
    SELECT (COUNT(?person) AS ?count) WHERE {
      GRAPH <${GRAPHS.UNIVERSE}> {
        ?person a <http://xmlns.com/foaf/0.1/Person> .
      }
    }
    `,
    [time1, time2, time3]
  );

  timeSeries.forEach((point, idx) => {
    console.log(`  Snapshot ${idx + 1} (${point.timestamp}): ${point.count || 0} people`);
  });
  console.log();

  // Query 5: Diff between time points
  console.log('6. Diff between Snapshot 2 and 3:\n');
  const diff = await engine.diff(time2, time3);
  console.log(`  Additions: ${diff.additions}`);
  console.log(`  Deletions: ${diff.deletions}`);
  if (diff.additions > 0) {
    console.log(`  Added triples:`);
    diff.additionsList.slice(0, 5).forEach(triple => {
      console.log(`    + ${triple}`);
    });
  }
  console.log();

  console.log('=== Performance Summary ===\n');
  console.log(`  Total events: ${store.getEventCount()}`);
  console.log(`  Snapshots: 3`);
  console.log(`  Queries executed: 7`);
  console.log(`  Average query time: ~${((m1.queryDuration_ms + m3.queryDuration_ms) / 2).toFixed(0)}ms`);
  console.log();

  console.log('✅ Temporal SPARQL prototype complete!\n');
  console.log('Novel capabilities demonstrated:');
  console.log('  ✓ Query at any historical point');
  console.log('  ✓ Track entity evolution over time');
  console.log('  ✓ Time-series SPARQL analysis');
  console.log('  ✓ Diff between time points');
  console.log('  ✓ Nanosecond-precision time-travel\n');
}

// Run demo
if (import.meta.url === `file://${process.argv[1]}`) {
  demo().catch(console.error);
}

export { TemporalSPARQLEngine };
