/**
 * @fileoverview Git Hooks Lifecycle Example
 * 
 * Demonstrates a complete Git hooks lifecycle using the minimal core system:
 * - pre-commit: Validate staging area
 * - post-commit: Process committed changes
 * - pre-push: Validate push operations
 * - post-receive: Process received changes and trigger actions
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

// Git operation schemas
const GitCommit = z.object({
  hash: z.string(),
  author: z.string(),
  message: z.string(),
  timestamp: z.string(),
  files: z.array(z.string())
});

const GitFile = z.object({
  path: z.string(),
  content: z.string(),
  operation: z.enum(['added', 'modified', 'deleted']),
  size: z.number()
});

const GitPush = z.object({
  ref: z.string(),
  from: z.string(),
  to: z.string(),
  commits: z.array(z.string())
});

/**
 * Complete Git Hooks Lifecycle Example
 */
async function gitHooksLifecycleExample() {
  console.log("🚀 Git Hooks Lifecycle - Complete Example");
  console.log("=" .repeat(50));

  // Initialize engine
  const engine = new RdfEngine({ eventsEnabled: true });
  const lifecycleEvents = [];

  // Register all Git hooks
  console.log("\n📋 Registering Git Hooks...");

  const unregisterHooks = [
    // Pre-commit: Validate staging area
    registerHook(engine, {
      id: 'pre-commit-validation',
      events: ['beforeAddQuad'],
      predicates: [
        {
          kind: 'COUNT',
          spec: {
            query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://git/prov#source> "staging" }',
            operator: '>=',
            value: 1
          }
        }
      ],
      action: async (payload, ok) => {
        lifecycleEvents.push({ 
          phase: 'pre-commit', 
          allowed: ok, 
          timestamp: new Date().toISOString(),
          event: payload.event
        });
        
        if (!ok) {
          throw new Error('❌ Pre-commit failed: No staging data found');
        }
        
        console.log("✅ Pre-commit validation passed");
      }
    }),

    // Post-commit: Process committed changes
    registerHook(engine, {
      id: 'post-commit-processing',
      events: ['afterAddQuad'],
      predicates: [
        {
          kind: 'COUNT',
          spec: {
            query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://git/prov#source> "commit" }',
            operator: '>=',
            value: 1
          }
        }
      ],
      action: async (payload, ok) => {
        if (ok && payload.data && payload.data.predicate && payload.data.predicate.value === 'http://git/commit/hash') {
          lifecycleEvents.push({ 
            phase: 'post-commit', 
            allowed: ok, 
            timestamp: new Date().toISOString(),
            commitHash: payload.data.object.value
          });
          
          console.log(`✅ Post-commit processing: ${payload.data.object.value}`);
          
          // Generate commit metadata
          const metadata = {
            processed: true,
            timestamp: new Date().toISOString(),
            hook: 'post-commit',
            commitHash: payload.data.object.value
          };
          
          const metadataRes = await ingress.fromJSON(
            z.object({ 
              processed: z.boolean(), 
              timestamp: z.string(), 
              hook: z.string(),
              commitHash: z.string()
            }),
            JSON.stringify(metadata),
            { 
              base: 'http://git/metadata/',
              subject: `urn:git:metadata:${payload.data.object.value}`
            }
          );
          
          writeWithProv(engine, metadataRes.rdf, 'metadata', {
            operation: 'post-commit',
            user: 'system'
          });
        }
      }
    }),

    // Pre-push: Validate push operations
    registerHook(engine, {
      id: 'pre-push-validation',
      events: ['beforeUpdate'],
      predicates: [
        {
          kind: 'COUNT',
          spec: {
            query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://git/prov#source> "commit" }',
            operator: '>=',
            value: 1
          }
        }
      ],
      action: async (payload, ok) => {
        lifecycleEvents.push({ 
          phase: 'pre-push', 
          allowed: ok, 
          timestamp: new Date().toISOString(),
          event: payload.event
        });
        
        if (!ok) {
          throw new Error('❌ Pre-push failed: No commits to push');
        }
        
        // Check for dangerous operations
        const dangerousQuery = `
          ASK WHERE { 
            ?s <http://git/commit/message> ?msg .
            FILTER(CONTAINS(LCASE(?msg), "force") || CONTAINS(LCASE(?msg), "delete"))
          }
        `;
        
        const dangerousResult = await engine.query(dangerousQuery);
        if (dangerousResult.boolean) {
          throw new Error('❌ Pre-push blocked: Dangerous commit message detected');
        }
        
        console.log("✅ Pre-push validation passed");
      }
    }),

    // Post-receive: Process received changes
    registerHook(engine, {
      id: 'post-receive-processing',
      events: ['afterAddQuad'],
      predicates: [
        {
          kind: 'COUNT',
          spec: {
            query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://git/prov#source> "push" }',
            operator: '>=',
            value: 1
          }
        }
      ],
      action: async (payload, ok) => {
        if (ok && payload.data && payload.data.predicate && payload.data.predicate.value === 'http://git/push/ref') {
          lifecycleEvents.push({ 
            phase: 'post-receive', 
            allowed: ok, 
            timestamp: new Date().toISOString(),
            ref: payload.data.object.value
          });
          
          console.log(`✅ Post-receive processing: ${payload.data.object.value}`);
          
          // Trigger downstream actions
          const actions = [
            { type: 'deploy', target: 'staging', ref: payload.data.object.value },
            { type: 'notify', channel: '#deployments', message: `New push to ${payload.data.object.value}` },
            { type: 'backup', repository: 'main', timestamp: new Date().toISOString() }
          ];
          
          for (const action of actions) {
            const actionRes = await ingress.fromJSON(
              z.object({ 
                type: z.string(), 
                target: z.string().optional(), 
                ref: z.string().optional(), 
                channel: z.string().optional(), 
                message: z.string().optional(), 
                repository: z.string().optional(), 
                timestamp: z.string().optional() 
              }),
              JSON.stringify(action),
              { 
                base: 'http://git/action/',
                subject: `urn:git:action:${action.type}:${Date.now()}`
              }
            );
            
            writeWithProv(engine, actionRes.rdf, 'action', {
              operation: 'post-receive',
              user: 'system'
            });
          }
        }
      }
    })
  ];

  console.log("✅ All Git hooks registered");

  // Simulate complete Git workflow
  console.log("\n🔄 Simulating Git Workflow...");

  // 1. Stage files (pre-commit trigger)
  console.log("\n1️⃣ Staging files...");
  const stagedFiles = [
    { path: 'src/feature.js', content: 'export function feature() { return "hello"; }', operation: 'added', size: 50 },
    { path: 'README.md', content: '# My App\n\nNew feature added.', operation: 'modified', size: 30 }
  ];

  for (const file of stagedFiles) {
    const fileRes = await ingress.fromJSON(
      GitFile,
      JSON.stringify(file),
      { 
        base: 'http://git/file/',
        subject: `urn:git:staging:${file.path}`
      }
    );
    
    writeWithProv(engine, fileRes.rdf, 'staging', {
      operation: 'pre-commit',
      user: 'developer',
      session: 'commit-session-123'
    });
  }

  console.log(`   Staged ${stagedFiles.length} files`);

  // 2. Commit (post-commit trigger)
  console.log("\n2️⃣ Committing changes...");
  const commitData = {
    hash: 'abc123def456',
    author: 'developer@example.com',
    message: 'Add new feature with tests',
    timestamp: new Date().toISOString(),
    files: stagedFiles.map(f => f.path)
  };

  const commitRes = await ingress.fromJSON(
    GitCommit,
    JSON.stringify(commitData),
    { 
      base: 'http://git/commit/',
      subject: 'urn:git:commit:abc123def456'
    }
  );

  writeWithProv(engine, commitRes.rdf, 'commit', {
    operation: 'post-commit',
    user: 'developer',
    session: 'commit-session-123'
  });

  console.log(`   Committed: ${commitData.hash}`);

  // 3. Push (pre-push and post-receive triggers)
  console.log("\n3️⃣ Pushing to remote...");
  const pushData = {
    ref: 'refs/heads/main',
    from: 'old123',
    to: 'abc123def456',
    commits: ['abc123def456']
  };

  const pushRes = await ingress.fromJSON(
    GitPush,
    JSON.stringify(pushData),
    { 
      base: 'http://git/push/',
      subject: 'urn:git:push:main'
    }
  );

  writeWithProv(engine, pushRes.rdf, 'push', {
    operation: 'post-receive',
    user: 'developer'
  });

  console.log(`   Pushed to: ${pushData.ref}`);

  // Wait for all async processing
  console.log("\n⏳ Waiting for async processing...");
  await new Promise(resolve => setTimeout(resolve, 1000));

  // 4. Verify lifecycle
  console.log("\n4️⃣ Verifying Git Lifecycle...");
  console.log(`   Total events: ${lifecycleEvents.length}`);
  
  lifecycleEvents.forEach((event, index) => {
    console.log(`   ${index + 1}. ${event.phase}: ${event.allowed ? '✅' : '❌'} (${event.timestamp})`);
    if (event.commitHash) console.log(`      Commit: ${event.commitHash}`);
    if (event.ref) console.log(`      Ref: ${event.ref}`);
  });

  // 5. Export results
  console.log("\n5️⃣ Exporting Git data...");
  
  // Export commit as JSON
  const commitExport = await egress.toJSON(
    GitCommit,
    engine.store,
    { subject: 'urn:git:commit:abc123def456' }
  );
  
  console.log("   Commit JSON:");
  console.log(commitExport.output);

  // Export all data as Turtle
  const turtleExport = await egress.toTurtle(
    null,
    engine.store,
    engine
  );
  
  console.log("\n   Full Turtle export (first 500 chars):");
  console.log(turtleExport.output.substring(0, 500) + "...");

  // 6. Show provenance chain
  console.log("\n6️⃣ Provenance Chain:");
  const provenanceQuads = engine.store.getQuads(
    null,
    engine.namedNode('http://ex/prov#source'),
    null,
    null
  );

  const sources = [...new Set(provenanceQuads.map(q => q.object.value))];
  sources.forEach(source => {
    const count = provenanceQuads.filter(q => q.object.value === source).length;
    console.log(`   ${source}: ${count} operations`);
  });

  // 7. Show hook statistics
  console.log("\n7️⃣ Hook Statistics:");
  const hookStats = engine.store.getHookStats();
  console.log(`   Total hooks: ${hookStats.totalHooks}`);
  console.log(`   Events monitored: ${Object.keys(hookStats.events).length}`);
  Object.entries(hookStats.events).forEach(([event, count]) => {
    console.log(`   ${event}: ${count} hooks`);
  });

  // Cleanup
  unregisterHooks.forEach(unregister => unregister());

  console.log("\n✅ Git Hooks Lifecycle Example Completed Successfully!");
  console.log("=" .repeat(50));
}

/**
 * Run the example
 */
if (import.meta.url === `file://${process.argv[1]}`) {
  gitHooksLifecycleExample().catch(console.error);
}

export { gitHooksLifecycleExample };
