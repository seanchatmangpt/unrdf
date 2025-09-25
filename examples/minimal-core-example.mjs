/**
 * @fileoverview Minimal Core Example - End-to-End (No AI)
 * 
 * Demonstrates the minimal, AI-free core system:
 * - Event-driven hooks on graph mutations
 * - Deterministic ingress/egress adapters
 * - Mandatory provenance on every write
 * - SHACL + reasoning optional but wired
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { RdfEngine } from "../src/engines/rdf-engine.mjs";
import { registerHook } from "../src/engines/minimal-hook-manager.mjs";
import { ingress, egress } from "../src/engines/deterministic-adapters.mjs";
import { writeWithProv } from "../src/engines/mandatory-provenance.mjs";
import { z } from "zod";

/**
 * Example: End-to-end minimal core system
 */
async function minimalCoreExample() {
  console.log("🚀 Minimal Core System - End-to-End Example");
  console.log("=" .repeat(50));

  // 1) Initialize engine
  const engine = new RdfEngine({ eventsEnabled: true });
  console.log("✅ Engine initialized with event system");

  // 2) Register hook: block export if graph too small
  const unregisterHook = registerHook(engine, {
    id: 'min-size',
    events: ['beforeSerialize'],
    predicates: [{ 
      kind: 'COUNT', 
      spec: { 
        query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s ?p ?o }', 
        operator: '>=', 
        value: 1 
      } 
    }],
    action: (_, ok) => { 
      if (!ok) throw new Error('Export blocked: empty graph'); 
    }
  });
  console.log("✅ Hook registered: min-size validation");

  // 3) Ingress JSON → RDF
  const User = z.object({ 
    name: z.string(), 
    age: z.number() 
  });
  
  const inRes = await ingress.fromJSON(
    User, 
    '{"name":"Ada","age":36}',
    { subject: 'urn:ingress:item' }
  );
  
  writeWithProv(engine, inRes.rdf, 'json');
  console.log("✅ JSON ingress completed with provenance");
  console.log(`   Data: ${JSON.stringify(inRes.data)}`);
  console.log(`   Quads: ${inRes.rdf.length}`);

  // 4) Egress RDF → JSON (governed by hook)
  const outRes = await egress.toJSON(
    User, 
    engine.store, 
    { subject: 'urn:ingress:item' }
  );
  
  console.log("✅ JSON egress completed");
  console.log(`   Output: ${outRes.output}`);

  // 5) Test hook blocking (try to serialize empty store)
  const emptyEngine = new RdfEngine({ eventsEnabled: true });
  
  // Register the same hook
  const unregisterEmptyHook = registerHook(emptyEngine, {
    id: 'min-size-empty',
    events: ['beforeSerialize'],
    predicates: [{ 
      kind: 'COUNT', 
      spec: { 
        query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s ?p ?o }', 
        operator: '>=', 
        value: 1 
      } 
    }],
    action: (_, ok) => { 
      if (!ok) throw new Error('Export blocked: empty graph'); 
    }
  });

  try {
    await emptyEngine.serializeTurtle();
    console.log("❌ Empty serialization should have been blocked");
  } catch (error) {
    console.log("✅ Empty serialization correctly blocked by hook");
    console.log(`   Error: ${error.message}`);
  }

  // 6) Test provenance
  console.log("\n📋 Provenance Check:");
  const provQuads = engine.store.getQuads(null, namedNode('http://ex/prov#source'), null, null);
  console.log(`   Provenance triples: ${provQuads.length}`);
  for (const q of provQuads) {
    console.log(`   ${q.subject.value} <- ${q.object.value}`);
  }

  // 7) Test event order
  console.log("\n🔄 Event Order Test:");
  const events = [];
  
  const unregisterEventHook = registerHook(engine, {
    id: 'event-monitor',
    events: ['beforeAddQuad', 'afterAddQuad', 'beforeSerialize', 'afterSerialize'],
    action: (payload, ok) => {
      events.push(payload.event || 'unknown');
      console.log(`   Event: ${payload.event || 'unknown'}`);
    }
  });

  // Add a quad to trigger events
  engine.store.addQuad(
    engine.namedNode('http://example.org/test'),
    engine.namedNode('http://example.org/type'),
    engine.literal('Test')
  );

  // Serialize to trigger more events
  await engine.serializeTurtle();

  console.log(`   Event sequence: ${events.join(' → ')}`);

  // Cleanup
  unregisterHook();
  unregisterEmptyHook();
  unregisterEventHook();

  console.log("\n✅ Minimal core example completed successfully!");
}

/**
 * Run the example
 */
if (import.meta.url === `file://${process.argv[1]}`) {
  minimalCoreExample().catch(console.error);
}

export { minimalCoreExample };
