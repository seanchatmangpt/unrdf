/**
 * @fileoverview Git Hooks + Knowledge Hooks Integration
 *
 * Demonstrates how git hooks integrate with knowledge hooks in the synchronous architecture:
 * - Git operations trigger knowledge hooks automatically
 * - Knowledge hooks monitor git operations for compliance and quality
 * - Synchronous event-driven architecture
 *
 * @version 1.0.0
 * @author unrdf Team
 * @license MIT
 */

import { RdfEngine } from '../packages/knowledge-engine/src/engines/rdf-engine.mjs';
import { registerHook } from '../packages/knowledge-engine/src/engines/hook-manager.mjs';
import {
  defineHook,
  _evaluateHook,
  useKnowledgeHooks,
} from '../packages/composables/src/composables/use-knowledge-hooks.mjs';
import { initStore } from '../packages/composables/src/context/index.mjs';
import { EVENTS } from '../packages/knowledge-engine/src/engines/event-bus.mjs';
import { z } from 'zod';

// Git operation schemas
const _GitCommit = z.object({
  hash: z.string(),
  author: z.string(),
  message: z.string(),
  timestamp: z.string(),
  files: z.array(z.string()),
});

const _GitFile = z.object({
  path: z.string(),
  content: z.string(),
  operation: z.enum(['added', 'modified', 'deleted']),
  size: z.number(),
});

/**
 * Git + Knowledge Hooks Integration Example
 */
function gitKnowledgeHooksIntegration() {
  console.log('🚀 Git + Knowledge Hooks Integration');
  console.log('='.repeat(50));

  // Initialize engine with events enabled
  const engine = new RdfEngine({ eventsEnabled: true });
  const runApp = initStore();

  // Track lifecycle events
  const lifecycleEvents = [];
  const knowledgeEvents = [];

  // Initialize knowledge hooks
  const hooks = useKnowledgeHooks();

  // Define knowledge hooks for git operations
  console.log('\n📋 Defining Knowledge Hooks...');

  // 1. Code Quality Hook - monitors for test coverage
  const qualityHook = defineHook({
    id: 'git:code-quality',
    name: 'Code Quality Monitor',
    description: 'Ensures commits include tests and meet quality standards',
    select: `
      SELECT ?commit ?author ?message ?fileCount WHERE {
        ?commit rdf:type <http://git/Commit> ;
                <http://git/author> ?author ;
                <http://git/message> ?message ;
                <http://git/fileCount> ?fileCount .
      }
    `,
    predicates: [
      {
        kind: 'THRESHOLD',
        spec: {
          var: 'fileCount',
          op: '>=',
          value: 2,
        },
      },
      {
        kind: 'ASK',
        spec: {
          query:
            'ASK WHERE { ?commit <http://git/message> ?message . FILTER(CONTAINS(?message, "test")) }',
          expected: true,
        },
      },
    ],
    combine: 'AND',
    events: [EVENTS.AFTER_ADD_QUAD],
    options: {
      callback: (result, _payload) => {
        if (result.fired) {
          knowledgeEvents.push({
            type: 'quality-check',
            status: 'passed',
            timestamp: new Date().toISOString(),
            data: result.data,
          });
          console.log('✅ Code quality check passed');
        } else {
          knowledgeEvents.push({
            type: 'quality-check',
            status: 'failed',
            timestamp: new Date().toISOString(),
            reason: 'Missing tests or insufficient files',
          });
          console.log('❌ Code quality check failed');
        }
      },
    },
  });

  // 2. Security Hook - monitors for sensitive data
  const securityHook = defineHook({
    id: 'git:security-scan',
    name: 'Security Scanner',
    description: 'Scans commits for potential security issues',
    select: `
      SELECT ?file ?content WHERE {
        ?file rdf:type <http://git/File> ;
              <http://git/content> ?content .
      }
    `,
    predicates: [
      {
        kind: 'ASK',
        spec: {
          query:
            'ASK WHERE { ?file <http://git/content> ?content . FILTER(CONTAINS(?content, "password") || CONTAINS(?content, "secret") || CONTAINS(?content, "key")) }',
          expected: false,
        },
      },
    ],
    combine: 'AND',
    events: [EVENTS.AFTER_ADD_QUAD],
    options: {
      callback: (result, _payload) => {
        if (result.fired) {
          knowledgeEvents.push({
            type: 'security-scan',
            status: 'clean',
            timestamp: new Date().toISOString(),
          });
          console.log('✅ Security scan passed - no sensitive data found');
        } else {
          knowledgeEvents.push({
            type: 'security-scan',
            status: 'alert',
            timestamp: new Date().toISOString(),
            reason: 'Potential sensitive data detected',
          });
          console.log('🚨 Security alert - potential sensitive data detected');
        }
      },
    },
  });

  // 3. Compliance Hook - monitors for required documentation
  const complianceHook = defineHook({
    id: 'git:compliance-check',
    name: 'Compliance Monitor',
    description: 'Ensures commits meet compliance requirements',
    select: `
      SELECT ?commit ?author ?message WHERE {
        ?commit rdf:type <http://git/Commit> ;
                <http://git/author> ?author ;
                <http://git/message> ?message .
      }
    `,
    predicates: [
      {
        kind: 'THRESHOLD',
        spec: {
          var: 'message',
          op: '>=',
          value: 10,
        },
      },
      {
        kind: 'ASK',
        spec: {
          query:
            'ASK WHERE { ?commit <http://git/message> ?message . FILTER(REGEX(?message, "^[A-Z]")) }',
          expected: true,
        },
      },
    ],
    combine: 'AND',
    events: [EVENTS.AFTER_ADD_QUAD],
    options: {
      callback: (result, _payload) => {
        if (result.fired) {
          knowledgeEvents.push({
            type: 'compliance-check',
            status: 'compliant',
            timestamp: new Date().toISOString(),
          });
          console.log('✅ Compliance check passed');
        } else {
          knowledgeEvents.push({
            type: 'compliance-check',
            status: 'non-compliant',
            timestamp: new Date().toISOString(),
            reason: 'Commit message does not meet standards',
          });
          console.log('❌ Compliance check failed - commit message issues');
        }
      },
    },
  });

  // Register knowledge hooks
  const unregisterKnowledgeHooks = [
    hooks.registerKnowledgeHook(qualityHook),
    hooks.registerKnowledgeHook(securityHook),
    hooks.registerKnowledgeHook(complianceHook),
  ];

  console.log('✅ Knowledge hooks registered');

  // Register git hooks that work with knowledge hooks
  console.log('\n📋 Registering Git Hooks...');

  const unregisterGitHooks = [
    // Pre-commit: Validate staging area
    registerHook(engine, {
      id: 'git:pre-commit',
      events: [EVENTS.BEFORE_ADD_QUAD],
      predicates: [
        {
          kind: 'COUNT',
          spec: {
            query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://git/prov#source> "staging" }',
            operator: '>=',
            value: 1,
          },
        },
      ],
      action: (payload, ok) => {
        lifecycleEvents.push({
          phase: 'pre-commit',
          allowed: ok,
          timestamp: new Date().toISOString(),
          event: payload.event,
        });

        if (!ok) {
          console.log('❌ Pre-commit failed: No staging data found');
          return false;
        }

        console.log('✅ Pre-commit validation passed');
        return true;
      },
    }),

    // Post-commit: Process committed changes
    registerHook(engine, {
      id: 'git:post-commit',
      events: [EVENTS.AFTER_ADD_QUAD],
      predicates: [
        {
          kind: 'COUNT',
          spec: {
            query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s <http://git/prov#source> "commit" }',
            operator: '>=',
            value: 1,
          },
        },
      ],
      action: (payload, ok) => {
        if (ok) {
          lifecycleEvents.push({
            phase: 'post-commit',
            allowed: ok,
            timestamp: new Date().toISOString(),
            event: payload.event,
          });

          console.log('✅ Post-commit processing completed');

          // Knowledge hooks will automatically fire due to the quad addition
          // No need to manually trigger them
        }
        return ok;
      },
    }),
  ];

  console.log('✅ Git hooks registered');

  // Simulate git workflow with knowledge hooks
  console.log('\n🔄 Simulating Git Workflow with Knowledge Hooks...');

  runApp(() => {
    // 1. Stage files (triggers pre-commit hook)
    console.log('\n1️⃣ Staging files...');
    const stagedFiles = [
      {
        path: 'src/feature.js',
        content: 'export function feature() { return "hello"; }',
        operation: 'added',
        size: 50,
      },
      {
        path: 'tests/feature.test.js',
        content: 'test("feature works", () => { expect(feature()).toBe("hello"); });',
        operation: 'added',
        size: 80,
      },
      {
        path: 'README.md',
        content: '# My App\n\nNew feature added with tests.',
        operation: 'modified',
        size: 40,
      },
    ];

    // Add staged files to store
    for (const file of stagedFiles) {
      const fileSubject = engine.namedNode(`http://git/file/${file.path}`);
      const fileType = engine.namedNode('http://git/File');
      const fileContent = engine.literal(file.content);
      const fileSize = engine.literal(file.size.toString());
      const fileOperation = engine.literal(file.operation);
      const source = engine.literal('staging');

      // Add file triples
      engine.store.addQuad(
        fileSubject,
        engine.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        fileType
      );
      engine.store.addQuad(fileSubject, engine.namedNode('http://git/content'), fileContent);
      engine.store.addQuad(fileSubject, engine.namedNode('http://git/size'), fileSize);
      engine.store.addQuad(fileSubject, engine.namedNode('http://git/operation'), fileOperation);
      engine.store.addQuad(fileSubject, engine.namedNode('http://git/prov#source'), source);
    }

    console.log(`   Staged ${stagedFiles.length} files`);

    // 2. Commit (triggers post-commit hook and knowledge hooks)
    console.log('\n2️⃣ Committing changes...');
    const commitData = {
      hash: 'abc123def456',
      author: 'developer@example.com',
      message: 'Add new feature with comprehensive tests',
      timestamp: new Date().toISOString(),
      files: stagedFiles.map(f => f.path),
    };

    const commitSubject = engine.namedNode(`http://git/commit/${commitData.hash}`);
    const commitType = engine.namedNode('http://git/Commit');
    const commitAuthor = engine.literal(commitData.author);
    const commitMessage = engine.literal(commitData.message);
    const commitTimestamp = engine.literal(commitData.timestamp);
    const commitFileCount = engine.literal(stagedFiles.length.toString());
    const commitSource = engine.literal('commit');

    // Add commit triples
    engine.store.addQuad(
      commitSubject,
      engine.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      commitType
    );
    engine.store.addQuad(commitSubject, engine.namedNode('http://git/author'), commitAuthor);
    engine.store.addQuad(commitSubject, engine.namedNode('http://git/message'), commitMessage);
    engine.store.addQuad(commitSubject, engine.namedNode('http://git/timestamp'), commitTimestamp);
    engine.store.addQuad(commitSubject, engine.namedNode('http://git/fileCount'), commitFileCount);
    engine.store.addQuad(commitSubject, engine.namedNode('http://git/prov#source'), commitSource);

    console.log(`   Committed: ${commitData.message}`);

    // 3. Push (simulate push operation)
    console.log('\n3️⃣ Pushing to remote...');
    const pushSubject = engine.namedNode('http://git/push/main');
    const pushType = engine.namedNode('http://git/Push');
    const pushRef = engine.literal('refs/heads/main');
    const pushFrom = engine.literal('abc123def456');
    const pushTo = engine.literal('def456ghi789');

    engine.store.addQuad(
      pushSubject,
      engine.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      pushType
    );
    engine.store.addQuad(pushSubject, engine.namedNode('http://git/push/ref'), pushRef);
    engine.store.addQuad(pushSubject, engine.namedNode('http://git/push/from'), pushFrom);
    engine.store.addQuad(pushSubject, engine.namedNode('http://git/push/to'), pushTo);

    console.log('   Pushed to refs/heads/main');
  });

  // Display results
  console.log('\n📊 Results Summary');
  console.log('='.repeat(30));

  console.log('\n🔄 Git Lifecycle Events:');
  lifecycleEvents.forEach(event => {
    console.log(`   ${event.phase}: ${event.allowed ? '✅' : '❌'} (${event.timestamp})`);
  });

  console.log('\n🧠 Knowledge Hook Events:');
  knowledgeEvents.forEach(event => {
    console.log(`   ${event.type}: ${event.status} (${event.timestamp})`);
    if (event.reason) {
      console.log(`     Reason: ${event.reason}`);
    }
  });

  // Get hook statistics
  console.log('\n📈 Hook Statistics:');
  const gitStats = engine.hookRegistry?.stats || {
    totalHooks: 0,
    executedHooks: 0,
  };
  const knowledgeStats = hooks.getStats();

  console.log(
    `   Git Hooks: ${gitStats.totalHooks} registered, ${gitStats.executedHooks} executed`
  );
  console.log(
    `   Knowledge Hooks: ${knowledgeStats.totalHooks} registered, ${knowledgeStats.evaluatedHooks} evaluated`
  );

  // Cleanup
  console.log('\n🧹 Cleaning up...');
  unregisterGitHooks.forEach(unregister => unregister());
  unregisterKnowledgeHooks.forEach(unregister => unregister());

  console.log('✅ Git + Knowledge Hooks Integration Complete');

  return {
    lifecycleEvents,
    knowledgeEvents,
    gitStats,
    knowledgeStats,
  };
}

// Export for use in other modules
export { gitKnowledgeHooksIntegration };

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  gitKnowledgeHooksIntegration();
}
