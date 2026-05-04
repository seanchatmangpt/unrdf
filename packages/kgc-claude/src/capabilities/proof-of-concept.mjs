/**
 * Proof of Concept - Hyper-Advanced Checkpointing Capabilities
 *
 * Demonstrates:
 * 1. Named checkpoints with time-travel
 * 2. Execution branch forking and merging
 * 3. Persistent state across sessions
 *
 * Run: node proof-of-concept.mjs
 */

import { createStore } from '@unrdf/oxigraph';
import { GitBackbone } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import {
  createTimeTravelManager,
  createExecutionBranchManager,
  createMemoryPersistenceManager,
} from './index.mjs';

/**
 * Helper to add sample data to store
 */
function addSampleData(store, label) {
  const universeGraph = dataFactory.namedNode('http://kgc.io/Universe');
  const entity = dataFactory.namedNode(`http://example.org/entity-${label}`);
  const predicate = dataFactory.namedNode('http://example.org/name');
  const value = dataFactory.literal(`Entity ${label}`);

  store.add(dataFactory.quad(entity, predicate, value, universeGraph));
}

/**
 * Proof of Concept: Time Travel
 */
async function demonstrateTimeTravel() {
  console.log('='.repeat(80));
  console.log('DEMO 1: Time Travel with Named Checkpoints');
  console.log('='.repeat(80));
  console.log();

  // Setup
  const store = createStore();
  const git = new GitBackbone('./tmp/poc-time-travel');
  await git.init();

  const timeTravelManager = createTimeTravelManager({ store, git });

  // Create checkpoints with labels
  console.log('Step 1: Creating named checkpoints...');

  addSampleData(store, 'alpha');
  const checkpoint1 = await timeTravelManager.createCheckpoint('alpha-state', {
    description: 'State with alpha entity',
    tags: ['entity', 'alpha'],
  });
  console.log(`  ✓ Created checkpoint: ${checkpoint1.label} (${checkpoint1.universeSize} quads)`);

  addSampleData(store, 'beta');
  const checkpoint2 = await timeTravelManager.createCheckpoint('beta-state', {
    description: 'State with alpha + beta',
    tags: ['entity', 'beta'],
  });
  console.log(`  ✓ Created checkpoint: ${checkpoint2.label} (${checkpoint2.universeSize} quads)`);

  addSampleData(store, 'gamma');
  const checkpoint3 = await timeTravelManager.createCheckpoint('gamma-state', {
    description: 'State with alpha + beta + gamma',
    tags: ['entity', 'gamma'],
  });
  console.log(`  ✓ Created checkpoint: ${checkpoint3.label} (${checkpoint3.universeSize} quads)`);
  console.log();

  // List checkpoints
  console.log('Step 2: Listing checkpoints by tag...');
  const betaCheckpoints = timeTravelManager.listCheckpoints({ tag: 'beta' });
  console.log(`  Found ${betaCheckpoints.length} checkpoint(s) with tag 'beta':`);
  for (const cp of betaCheckpoints) {
    console.log(`    - ${cp.label}: ${cp.description}`);
  }
  console.log();

  // Diff checkpoints
  console.log('Step 3: Comparing checkpoints...');
  const diff = await timeTravelManager.diffCheckpoints('alpha-state', 'gamma-state');
  console.log(`  Diff from alpha-state to gamma-state:`);
  console.log(`    Added quads: ${diff.addedQuads}`);
  console.log(`    Removed quads: ${diff.removedQuads}`);
  console.log(`    Unchanged quads: ${diff.unchangedQuads}`);
  console.log(`    Time delta: ${Number(diff.timeDeltaNs) / 1e9}s`);
  console.log(`    Diff hash: ${diff.diffHash.slice(0, 16)}...`);
  console.log();

  // Restore to checkpoint
  console.log('Step 4: Restoring to beta-state...');
  const restored = await timeTravelManager.restoreToCheckpoint('beta-state');
  const universeGraph = dataFactory.namedNode('http://kgc.io/Universe');
  const restoredQuads = [...restored.match(null, null, null, universeGraph)];
  console.log(`  ✓ Restored to beta-state (${restoredQuads.length} quads)`);
  console.log();

  console.log('✅ Time Travel demo complete!');
  console.log();
}

/**
 * Proof of Concept: Execution Branches
 */
async function demonstrateExecutionBranches() {
  console.log('='.repeat(80));
  console.log('DEMO 2: Execution Branch Forking and Merging');
  console.log('='.repeat(80));
  console.log();

  // Setup
  const store = createStore();
  const git = new GitBackbone('./tmp/poc-branches');
  await git.init();

  const branchManager = createExecutionBranchManager({ store, git });

  // Add initial data
  console.log('Step 1: Setting up main branch...');
  addSampleData(store, 'main');
  await branchManager.advanceBranch({ label: 'main-v1' });
  console.log('  ✓ Main branch initialized');
  console.log();

  // Fork branch
  console.log('Step 2: Forking experimental branch...');
  const expBranch = await branchManager.forkBranch('experimental', {
    description: 'Experimental feature development',
  });
  console.log(`  ✓ Forked branch: ${expBranch.name}`);
  console.log(`    Fork point: ${expBranch.forkPoint.timestamp_iso}`);
  console.log();

  // Switch to experimental and make changes
  console.log('Step 3: Making changes on experimental branch...');
  await branchManager.switchBranch('experimental');
  addSampleData(store, 'experiment-1');
  await branchManager.advanceBranch({ label: 'exp-v1' });
  addSampleData(store, 'experiment-2');
  await branchManager.advanceBranch({ label: 'exp-v2' });
  console.log('  ✓ Advanced experimental branch (2 checkpoints)');
  console.log();

  // Switch back to main and make different changes
  console.log('Step 4: Making changes on main branch...');
  await branchManager.switchBranch('main');
  addSampleData(store, 'main-feature');
  await branchManager.advanceBranch({ label: 'main-v2' });
  console.log('  ✓ Advanced main branch');
  console.log();

  // Compare branches
  console.log('Step 5: Comparing branches...');
  const comparison = await branchManager.compareBranches('main', 'experimental');
  console.log(`  Branch comparison:`);
  console.log(`    Main ahead by: ${comparison.ahead} quads`);
  console.log(`    Main behind by: ${comparison.behind} quads`);
  console.log(`    Identical: ${comparison.identical}`);
  console.log();

  // Merge experimental into main
  console.log('Step 6: Merging experimental into main...');
  const mergeResult = await branchManager.mergeBranches('experimental', 'main', {
    strategy: 'theirs', // Accept experimental changes
  });
  console.log(`  Merge result:`);
  console.log(`    Success: ${mergeResult.success}`);
  console.log(`    Strategy: ${mergeResult.strategy}`);
  console.log(`    Conflicts: ${mergeResult.conflicts.length}`);
  console.log(`    Resolved: ${mergeResult.resolvedConflicts}`);
  console.log();

  // List branches
  console.log('Step 7: Listing all branches...');
  const branches = branchManager.listBranches();
  console.log(`  Total branches: ${branches.length}`);
  for (const branch of branches) {
    console.log(`    - ${branch.name} (${branch.status})`);
  }
  console.log();

  console.log('✅ Execution Branches demo complete!');
  console.log();
}

/**
 * Proof of Concept: State Persistence
 */
async function demonstrateStatePersistence() {
  console.log('='.repeat(80));
  console.log('DEMO 3: Persistent State with Migrations');
  console.log('='.repeat(80));
  console.log();

  // Setup (using memory backend for demo)
  const persistenceManager = createMemoryPersistenceManager({
    currentVersion: 2,
    compress: false, // Disable compression for demo clarity
  });

  // Register migration
  console.log('Step 1: Registering state migration...');
  persistenceManager.registerMigration(1, 2, (oldState) => {
    // Migrate v1 to v2: add timestamp field
    return {
      ...oldState,
      migratedAt: new Date().toISOString(),
    };
  }, {
    description: 'Add migratedAt timestamp',
  });
  console.log('  ✓ Migration registered (v1 → v2)');
  console.log();

  // Save state
  console.log('Step 2: Saving application state...');
  const appState = {
    user: 'alice',
    preferences: { theme: 'dark', lang: 'en' },
    session: { startedAt: new Date().toISOString() },
  };
  const receipt = await persistenceManager.saveState('app-state', appState, {
    metadata: { source: 'demo' },
  });
  console.log(`  ✓ State saved:`);
  console.log(`    Key: ${receipt.key}`);
  console.log(`    Hash: ${receipt.hash.slice(0, 16)}...`);
  console.log(`    Version: ${receipt.version}`);
  console.log();

  // List states
  console.log('Step 3: Listing stored states...');
  const keys = await persistenceManager.listStates();
  console.log(`  Found ${keys.length} state(s):`);
  for (const key of keys) {
    console.log(`    - ${key}`);
  }
  console.log();

  // Verify integrity
  console.log('Step 4: Verifying state integrity...');
  const verification = await persistenceManager.verifyState('app-state');
  console.log(`  Verification result:`);
  console.log(`    Valid: ${verification.valid}`);
  console.log(`    Version: ${verification.version}`);
  console.log(`    Hash: ${verification.hash?.slice(0, 16)}...`);
  console.log();

  // Load state
  console.log('Step 5: Loading state...');
  const loaded = await persistenceManager.loadState('app-state');
  console.log(`  ✓ State loaded:`);
  console.log(`    User: ${loaded.user}`);
  console.log(`    Theme: ${loaded.preferences.theme}`);
  if (loaded.migratedAt) {
    console.log(`    Migrated at: ${loaded.migratedAt}`);
  }
  console.log();

  // Export/Import
  console.log('Step 6: Exporting all states...');
  const exported = await persistenceManager.exportStates();
  console.log(`  ✓ Exported ${Object.keys(exported.states).length} state(s)`);
  console.log(`    Version: ${exported.version}`);
  console.log(`    Timestamp: ${exported.timestamp}`);
  console.log();

  console.log('✅ State Persistence demo complete!');
  console.log();
}

/**
 * Main demo orchestrator
 */
async function main() {
  console.log('\n');
  console.log('╔════════════════════════════════════════════════════════════════════════════╗');
  console.log('║     HYPER-ADVANCED CHECKPOINTING CAPABILITIES - PROOF OF CONCEPT           ║');
  console.log('║                                                                            ║');
  console.log('║  Agent 7 (α₇) - Checkpointing and Rewind Explorer                         ║');
  console.log('╚════════════════════════════════════════════════════════════════════════════╝');
  console.log('\n');

  try {
    await demonstrateTimeTravel();
    await demonstrateExecutionBranches();
    await demonstrateStatePersistence();

    console.log('='.repeat(80));
    console.log('All demos completed successfully! 🎉');
    console.log('='.repeat(80));
    console.log();
    console.log('Capabilities demonstrated:');
    console.log('  1. ✅ Named checkpoints with labels and tags');
    console.log('  2. ✅ Checkpoint diffing and comparison');
    console.log('  3. ✅ Time-travel restoration to specific points');
    console.log('  4. ✅ Execution branch forking');
    console.log('  5. ✅ Branch merging with conflict detection');
    console.log('  6. ✅ Persistent state with migrations');
    console.log('  7. ✅ State integrity verification');
    console.log('  8. ✅ Export/import capabilities');
    console.log();

  } catch (error) {
    console.error('❌ Demo failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { main };
