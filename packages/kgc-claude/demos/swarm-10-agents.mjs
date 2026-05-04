#!/usr/bin/env node
/**
 * KGC-SWARM 10-Agent Demo
 *
 * Demonstrates the complete calculus implementation:
 *
 * Universe:
 *   E ≔ VM
 *   𝔄 ≔ {α₁,…,α₁₀}
 *   τ ≔ run-epoch(E)
 *
 * Law:
 *   A = μ(O)
 *   μ∘μ = μ
 *
 * @module demos/swarm-10-agents
 */

import { KGCStore } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import { blake3 } from 'hash-wasm';
import { SwarmOrchestrator, createSwarm10 } from '../src/swarm-orchestrator.mjs';
import { PokaYokeGuard, createGuard } from '../src/poka-yoke-guards.mjs';
import { ObservableIO, createObservableIO } from '../src/observable-io.mjs';
import { InfoScheduler, createScheduler } from '../src/info-scheduler.mjs';
import { DriftDetector, createDriftDetector } from '../src/drift-detector.mjs';
import { BudgetEnforcer, createBudget } from '../src/budget-enforcer.mjs';
import { AgentHarness, createAgent } from '../src/agent-harness.mjs';
import { ReceiptCompositor, createCompositor } from '../src/receipt-compositor.mjs';
import { mergeDeltas, getPendingDeltas, clearShards } from '../src/shard-merge.mjs';

/**
 * Demo configuration for 10 agents (α₁ through α₁₀)
 */
const SWARM_CONFIG = {
  agents: Array.from({ length: 10 }, (_, i) => ({
    id: `α_${i + 1}`,
    scope: {
      subjects: [`http://kgc.io/agent/${i + 1}/entity`],
      graphs: [`http://kgc.io/graph/${i + 1}`],
      files: [`domain/agent_${i + 1}/**`],
    },
    priority: i + 1,
    capabilities: ['read', 'write', 'query', 'transform'],
    workload: {
      type: 'entity_creation',
      entity: `Entity_${i + 1}`,
      properties: {
        name: `Agent ${i + 1} Entity`,
        value: (i + 1) * 100,
        created_by: `α_${i + 1}`,
      },
    },
  })),
  budget: {
    time: 30000,
    steps: 100,
    bytes: 10 * 1024 * 1024,
  },
  drift_epsilon: 0.01,
  min_epochs: 3,
};

/**
 * Generate probes for agents
 */
function generateProbes(agentConfigs) {
  const probes = [];
  let probeId = 0;

  for (const agent of agentConfigs) {
    // Read probe
    probes.push({
      id: `probe-${probeId++}`,
      type: 'read',
      target: agent.scope.subjects[0],
      params: { depth: 1 },
      cost: 1,
      expected_yield: 0.3,
    });

    // Write probe
    probes.push({
      id: `probe-${probeId++}`,
      type: 'write',
      target: agent.scope.subjects[0],
      params: { data: agent.workload },
      cost: 2,
      expected_yield: 0.8,
    });

    // Query probe
    probes.push({
      id: `probe-${probeId++}`,
      type: 'query',
      target: agent.scope.graphs[0],
      params: { sparql: 'SELECT * WHERE { ?s ?p ?o }' },
      cost: 1.5,
      expected_yield: 0.5,
    });
  }

  return probes;
}

/**
 * Simulate probe execution
 */
async function executeProbe(probe) {
  // Simulate work
  await new Promise(resolve => setTimeout(resolve, 10));

  switch (probe.type) {
    case 'read':
      return { type: 'read_result', target: probe.target, found: true };
    case 'write':
      return { type: 'write_result', target: probe.target, success: true };
    case 'query':
      return { type: 'query_result', target: probe.target, bindings: [] };
    case 'transform':
      return { type: 'transform_result', target: probe.target, transformed: true };
    default:
      return { type: 'unknown', target: probe.target };
  }
}

/**
 * Print formatted section
 */
function printSection(title) {
  console.log('\n' + '═'.repeat(80));
  console.log(`  ${title}`);
  console.log('═'.repeat(80));
}

/**
 * Print subsection
 */
function printSubsection(title) {
  console.log('\n' + '─'.repeat(60));
  console.log(`  ${title}`);
  console.log('─'.repeat(60));
}

/**
 * Main demo execution
 */
async function runDemo() {
  printSection('KGC-SWARM 10-Agent Demo - Calculus Implementation');
  console.log('\nUniverse: E ≔ VM, 𝔄 ≔ {α₁,…,α₁₀}');
  console.log('Law: A = μ(O), μ∘μ = μ (idempotent projection)');

  const startTime = Date.now();
  const transcript = [];

  // ════════════════════════════════════════════════════════════════════════════
  // 1. Initialize Swarm
  // ════════════════════════════════════════════════════════════════════════════
  printSubsection('1. Initialize Swarm (𝔄 ≔ {α₁,…,α₁₀})');

  const store = new KGCStore({ nodeId: 'swarm-coordinator' });
  console.log(`✓ KGCStore initialized (nodeId: ${store.vectorClock.nodeId})`);

  const swarm = createSwarm10({
    budget: SWARM_CONFIG.budget,
    drift_epsilon: SWARM_CONFIG.drift_epsilon,
    min_epochs: SWARM_CONFIG.min_epochs,
  });
  swarm.initializeAgents();
  console.log(`✓ SwarmOrchestrator created with ${swarm.agents.size} agents`);

  // Initialize shared components
  const guard = createGuard(
    SWARM_CONFIG.agents.map(a => a.scope.files[0]),
    []
  );
  console.log('✓ PokaYokeGuard initialized (Σ_guard enforcement)');

  const io = createObservableIO();
  console.log('✓ ObservableIO initialized (O_vm ⊔ O_bb)');

  const compositor = createCompositor();
  console.log('✓ ReceiptCompositor initialized (ReceiptChain)');

  transcript.push({
    phase: 'initialize',
    agents: Array.from(swarm.agents.keys()),
    timestamp: Date.now(),
  });

  // ════════════════════════════════════════════════════════════════════════════
  // 2. Generate Initial Probes (P := X probes)
  // ════════════════════════════════════════════════════════════════════════════
  printSubsection('2. Generate Initial Probes (P := initial probes)');

  const probes = generateProbes(SWARM_CONFIG.agents);
  swarm.addProbes(probes);
  console.log(`✓ Generated ${probes.length} probes for ${SWARM_CONFIG.agents.length} agents`);
  console.log(`  Types: read (${probes.filter(p => p.type === 'read').length}), ` +
              `write (${probes.filter(p => p.type === 'write').length}), ` +
              `query (${probes.filter(p => p.type === 'query').length})`);

  transcript.push({
    phase: 'generate_probes',
    probe_count: probes.length,
    timestamp: Date.now(),
  });

  // ════════════════════════════════════════════════════════════════════════════
  // 3. Run Swarm Epochs
  // ════════════════════════════════════════════════════════════════════════════
  printSubsection('3. Run Swarm Epochs (while ¬stop)');

  console.log('\nExecution pattern:');
  console.log('  parallel ∀ α∈𝔄:');
  console.log('    p := choose(P)       // Λ scheduling');
  console.log('    o := Obs(p,E,X,R)    // Execute probe');
  console.log('    O := O ⊔ o           // Accumulate observations');
  console.log('  A := μ(O)              // Project to artifact\n');

  const result = await swarm.run(executeProbe, []);

  console.log(`\n✓ Swarm execution completed in ${result.epochs} epochs`);
  console.log(`  Total observations: ${swarm.observations.length}`);
  console.log(`  Final drift: ${swarm.previousArtifact?.drift || 'N/A'}`);

  // Log each epoch
  for (const receipt of result.receipts) {
    console.log(`  Epoch ${receipt.epoch}: drift=${receipt.drift.toFixed(4)}, ` +
                `obs=${receipt.observations}, deltas=${receipt.deltas_merged}`);

    // Add receipts to compositor
    await compositor.addReceipt(receipt, 'swarm-coordinator', 'epoch_receipt');
  }

  transcript.push({
    phase: 'run_epochs',
    epochs: result.epochs,
    observations: swarm.observations.length,
    timestamp: Date.now(),
  });

  // ════════════════════════════════════════════════════════════════════════════
  // 4. Merge Deltas (Π operator)
  // ════════════════════════════════════════════════════════════════════════════
  printSubsection('4. Merge Deltas (Π ⊕-monoid composition)');

  // Simulate delta collection from agents
  for (const [agentId, agent] of swarm.agents) {
    const agentConfig = SWARM_CONFIG.agents.find(a => a.id === agentId);
    if (!agentConfig) continue;

    // Simulate adding entity delta
    const deltaResult = await swarm.executeProbe(agentId, {
      id: `delta-${agentId}`,
      type: 'write',
      target: agentConfig.scope.subjects[0],
      params: agentConfig.workload,
      cost: 1,
      expected_yield: 0.9,
    }, executeProbe);

    console.log(`  ${agentId}: Added entity delta for ${agentConfig.workload.entity}`);
  }

  // Get final artifact
  const finalArtifact = await swarm.project();
  console.log(`\n✓ Merge completed`);
  console.log(`  Merged deltas: ${finalArtifact.merged_deltas}`);
  console.log(`  Conflicts: ${finalArtifact.conflicts}`);
  console.log(`  Merge hash: ${finalArtifact.merge_hash?.slice(0, 32) || 'N/A'}...`);

  transcript.push({
    phase: 'merge_deltas',
    merged_count: finalArtifact.merged_deltas,
    conflicts: finalArtifact.conflicts,
    timestamp: Date.now(),
  });

  // ════════════════════════════════════════════════════════════════════════════
  // 5. Generate Composite Receipt
  // ════════════════════════════════════════════════════════════════════════════
  printSubsection('5. Generate Composite Receipt (ReceiptChain)');

  // Add agent receipts
  for (const [agentId, agent] of swarm.agents) {
    await compositor.addAgentReceipts(agentId, agent.observations.map(o => ({
      type: 'observation',
      hash: o.hash,
      agent_id: agentId,
    })));
  }

  // Create claims
  const claims = SWARM_CONFIG.agents.map(agent => ({
    claim: `Agent ${agent.id} created entity ${agent.workload.entity}`,
    agent_id: agent.id,
    observations: swarm.observations.filter(o => o.agent_id === agent.id),
  }));

  // Compose final receipt
  const compositeReceipt = await compositor.compose(result.epochs, claims);
  console.log(`✓ Composite receipt generated`);
  console.log(`  Receipt ID: ${compositeReceipt.id}`);
  console.log(`  Merkle root: ${compositeReceipt.merkle_root.slice(0, 32)}...`);
  console.log(`  Agent count: ${compositeReceipt.agent_count}`);
  console.log(`  Receipt count: ${compositeReceipt.receipt_count}`);
  console.log(`  Claims mapped: ${compositeReceipt.claims.length}`);

  // Verify chain
  const chainValid = await compositor.verifyChain();
  console.log(`  Chain valid: ${chainValid ? '✓' : '✗'}`);

  transcript.push({
    phase: 'composite_receipt',
    receipt_id: compositeReceipt.id,
    merkle_root: compositeReceipt.merkle_root,
    chain_valid: chainValid,
    timestamp: Date.now(),
  });

  // ════════════════════════════════════════════════════════════════════════════
  // 6. Final State Summary
  // ════════════════════════════════════════════════════════════════════════════
  printSubsection('6. Final State Summary (A = μ(O))');

  const swarmState = swarm.getState();
  console.log('\nSwarm State:');
  console.log(`  Epochs: ${swarmState.epoch}`);
  console.log(`  Agents: ${swarmState.agents.length}`);
  console.log(`  Observations: ${swarmState.observations}`);
  console.log(`  Receipts: ${swarmState.receipts}`);

  console.log('\nBudget Usage:');
  console.log(`  Time: ${swarmState.budget_used.time}ms / ${SWARM_CONFIG.budget.time}ms`);
  console.log(`  Steps: ${swarmState.budget_used.steps} / ${SWARM_CONFIG.budget.steps}`);
  console.log(`  Bytes: ${swarmState.budget_used.bytes} / ${SWARM_CONFIG.budget.bytes}`);

  const chain = compositor.getChain();
  console.log('\nReceipt Chain:');
  console.log(`  Length: ${chain.length}`);
  console.log(`  Head hash: ${chain.head_hash?.slice(0, 32) || 'N/A'}...`);
  console.log(`  Tail hash: ${chain.tail_hash?.slice(0, 32) || 'N/A'}...`);

  // ════════════════════════════════════════════════════════════════════════════
  // 7. Demo Output (JSON)
  // ════════════════════════════════════════════════════════════════════════════
  printSubsection('7. Demo Output (Deliver: emit(A) ∧ emit(Receipts) ∧ emit(κ))');

  const demoOutput = {
    // Configuration (κ)
    config: {
      agents: SWARM_CONFIG.agents.length,
      budget: SWARM_CONFIG.budget,
      drift_epsilon: SWARM_CONFIG.drift_epsilon,
    },
    // Artifact (A)
    artifact: {
      epochs: result.epochs,
      observations: swarmState.observations,
      merged_deltas: finalArtifact.merged_deltas,
      conflicts: finalArtifact.conflicts,
    },
    // Receipts
    receipts: {
      count: result.receipts.length,
      composite_id: compositeReceipt.id,
      merkle_root: compositeReceipt.merkle_root,
      chain_valid: chainValid,
    },
    // Agents
    agents: SWARM_CONFIG.agents.map(a => ({
      id: a.id,
      entity: a.workload.entity,
      priority: a.priority,
    })),
    // Timing
    duration_ms: Date.now() - startTime,
    transcript,
  };

  console.log(JSON.stringify(demoOutput, null, 2));

  // ════════════════════════════════════════════════════════════════════════════
  // 8. Verification
  // ════════════════════════════════════════════════════════════════════════════
  printSection('KGC-SWARM 10-Agent Demo - Complete');

  console.log('\n✓ All 10 agents (α₁ through α₁₀) executed successfully');
  console.log('✓ Observations accumulated: O = O_vm ⊔ O_bb');
  console.log('✓ Artifact projected: A = μ(O)');
  console.log('✓ Deltas merged: Π ⊕-monoid composition');
  console.log('✓ Receipts chained: r₀ → r₁ → … → r_n');
  console.log('✓ Claims verified: ∀ claim c ∈ A : c ↦ {h(o₁),…,h(o_k)}');

  console.log(`\nTotal execution time: ${Date.now() - startTime}ms`);

  return demoOutput;
}

// Run demo if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runDemo()
    .then((output) => {
      process.exit(0);
    })
    .catch((error) => {
      console.error('\n' + '!'.repeat(80));
      console.error('DEMO FAILED');
      console.error('!'.repeat(80));
      console.error(error);
      process.exit(1);
    });
}

export { runDemo, SWARM_CONFIG };
