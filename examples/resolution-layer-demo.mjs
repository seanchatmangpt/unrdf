#!/usr/bin/env node

/**
 * @file Resolution Layer Demo
 * @description
 * Demonstrates multi-agent coordination and Delta resolution for swarm behavior.
 */

import { createResolutionLayer } from '../src/knowledge-engine/resolution-layer.mjs';
import { TransactionManager } from '../src/knowledge-engine/transaction.mjs';
import { createStore } from '../packages/oxigraph/src/index.mjs';

console.log('🤝 Resolution Layer Demo\n');

async function demonstrateResolutionLayer() {
  try {
    // Create a resolution layer
    const resolution = createResolutionLayer({
      defaultStrategy: 'voting',
      maxProposals: 100,
      enableConflictDetection: true,
      enableConsensus: true,
    });

    console.log('🤝 Resolution Layer created');

    // Register different types of agents
    const agents = [
      { id: 'validator-agent', type: 'validator', role: 'data-validation' },
      { id: 'enforcer-agent', type: 'enforcer', role: 'policy-enforcement' },
      { id: 'monitor-agent', type: 'monitor', role: 'system-monitoring' },
      { id: 'auditor-agent', type: 'auditor', role: 'compliance-audit' },
    ];

    for (const agent of agents) {
      resolution.registerAgent(agent.id, agent);
      console.log(`  ✅ Registered agent: ${agent.id} (${agent.type})`);
    }

    // Simulate multiple agents proposing changes to the same data
    console.log('\n📝 Simulating multi-agent proposals...');

    // Agent 1: Validator proposes adding a validation rule
    const validatorDelta = {
      additions: [
        {
          subject: { value: 'ex:user-data', termType: 'NamedNode' },
          predicate: { value: 'ex:hasValidation', termType: 'NamedNode' },
          object: { value: 'ex:email-format', termType: 'NamedNode' },
        },
      ],
      removals: [],
    };

    const proposal1 = await resolution.submitProposal('validator-agent', validatorDelta, {
      confidence: 0.9,
      priority: 80,
      metadata: { rule: 'email-validation', severity: 'high' },
    });

    // Agent 2: Enforcer proposes adding an enforcement policy
    const enforcerDelta = {
      additions: [
        {
          subject: { value: 'ex:user-data', termType: 'NamedNode' },
          predicate: { value: 'ex:hasPolicy', termType: 'NamedNode' },
          object: { value: 'ex:data-retention', termType: 'NamedNode' },
        },
      ],
      removals: [],
    };

    const proposal2 = await resolution.submitProposal('enforcer-agent', enforcerDelta, {
      confidence: 0.85,
      priority: 75,
      metadata: { policy: 'retention-policy', scope: 'global' },
    });

    // Agent 3: Monitor proposes adding monitoring rules
    const monitorDelta = {
      additions: [
        {
          subject: { value: 'ex:system', termType: 'NamedNode' },
          predicate: { value: 'ex:hasMonitoring', termType: 'NamedNode' },
          object: { value: 'ex:performance-metrics', termType: 'NamedNode' },
        },
      ],
      removals: [],
    };

    const proposal3 = await resolution.submitProposal('monitor-agent', monitorDelta, {
      confidence: 0.7,
      priority: 60,
      metadata: { metric: 'performance', threshold: '95%' },
    });

    // Agent 4: Auditor proposes conflicting change (removing validation)
    const auditorDelta = {
      additions: [],
      removals: [
        {
          subject: { value: 'ex:user-data', termType: 'NamedNode' },
          predicate: { value: 'ex:hasValidation', termType: 'NamedNode' },
          object: { value: 'ex:email-format', termType: 'NamedNode' },
        },
      ],
    };

    const proposal4 = await resolution.submitProposal('auditor-agent', auditorDelta, {
      confidence: 0.6,
      priority: 50,
      metadata: {
        reason: 'overly-restrictive',
        alternative: 'flexible-validation',
      },
    });

    console.log(`  ✅ Proposals submitted: ${proposal1}, ${proposal2}, ${proposal3}, ${proposal4}`);

    // Test different resolution strategies
    console.log('\n🗳️  Testing resolution strategies...');

    // 1. Voting strategy
    console.log('\n1️⃣  Voting Strategy:');
    const votingResult = await resolution.resolveProposals(
      [proposal1, proposal2, proposal3, proposal4],
      {
        type: 'voting',
        quorum: 0.5,
      }
    );

    console.log(`  Strategy: ${votingResult.strategy}`);
    console.log(`  Consensus: ${votingResult.consensus ? '✅ achieved' : '❌ not achieved'}`);
    console.log(`  Confidence: ${votingResult.confidence.toFixed(2)}`);
    console.log(`  Conflicts: ${votingResult.conflicts?.length || 0}`);
    console.log(
      `  Resolved delta: ${votingResult.resolvedDelta.additions.length} additions, ${votingResult.resolvedDelta.removals.length} removals`
    );

    // 2. Priority strategy
    console.log('\n2️⃣  Priority Strategy:');
    const priorityResult = await resolution.resolveProposals(
      [proposal1, proposal2, proposal3, proposal4],
      {
        type: 'priority',
      }
    );

    console.log(`  Strategy: ${priorityResult.strategy}`);
    console.log(`  Consensus: ${priorityResult.consensus ? '✅ achieved' : '❌ not achieved'}`);
    console.log(`  Confidence: ${priorityResult.confidence.toFixed(2)}`);

    // 3. Merging strategy
    console.log('\n3️⃣  Merging Strategy:');
    const mergingResult = await resolution.resolveProposals([proposal1, proposal2, proposal3], {
      type: 'merging',
    });

    console.log(`  Strategy: ${mergingResult.strategy}`);
    console.log(`  Consensus: ${mergingResult.consensus ? '✅ achieved' : '❌ not achieved'}`);
    console.log(`  Confidence: ${mergingResult.confidence.toFixed(2)}`);
    console.log(
      `  Merged delta: ${mergingResult.resolvedDelta.additions.length} additions, ${mergingResult.resolvedDelta.removals.length} removals`
    );

    // 4. CRDT strategy
    console.log('\n4️⃣  CRDT Strategy:');
    const crdtResult = await resolution.resolveProposals(
      [proposal1, proposal2, proposal3, proposal4],
      {
        type: 'crdt',
      }
    );

    console.log(`  Strategy: ${crdtResult.strategy}`);
    console.log(`  Consensus: ${crdtResult.consensus ? '✅ achieved' : '❌ not achieved'}`);
    console.log(`  Confidence: ${crdtResult.confidence.toFixed(2)}`);
    console.log(
      `  CRDT delta: ${crdtResult.resolvedDelta.additions.length} additions, ${crdtResult.resolvedDelta.removals.length} removals`
    );

    // Get resolution history
    console.log('\n📚 Resolution History:');
    const history = resolution.getResolutionHistory(5);
    console.log(`  Recent resolutions: ${history.length}`);
    for (const entry of history) {
      console.log(
        `    ${entry.strategy}: ${entry.consensus ? '✅' : '❌'} consensus, confidence: ${entry.confidence.toFixed(2)}`
      );
    }

    // Get comprehensive statistics
    const stats = resolution.getStats();
    console.log('\n📊 Resolution Layer Statistics:');
    console.log(`  Total proposals: ${stats.proposals.total}`);
    console.log(`  Active agents: ${stats.agents.active}`);
    console.log(`  Total resolutions: ${stats.resolutions.total}`);
    console.log(`  Strategy usage:`);
    for (const [strategy, count] of Object.entries(stats.resolutions.strategies)) {
      console.log(`    ${strategy}: ${count} times`);
    }

    // Demonstrate integration with TransactionManager
    console.log('\n🔗 Testing integration with TransactionManager...');

    const txManager = new TransactionManager({
      enableResolution: true,
      resolutionConfig: {
        defaultStrategy: 'voting',
        maxProposals: 50,
        enableConflictDetection: true,
      },
    });

    // Submit proposals through transaction manager
    const txProposal1 = await txManager.submitProposal('validator-agent', validatorDelta, {
      confidence: 0.9,
      priority: 80,
    });

    const txProposal2 = await txManager.submitProposal('enforcer-agent', enforcerDelta, {
      confidence: 0.85,
      priority: 75,
    });

    console.log(`  ✅ Transaction manager proposals: ${txProposal1}, ${txProposal2}`);

    // Resolve through transaction manager
    const txResolution = await txManager.resolveProposals([txProposal1, txProposal2]);
    console.log(
      `  ✅ Transaction manager resolution: ${txResolution.strategy}, consensus: ${txResolution.consensus}`
    );

    // Get transaction manager resolution stats
    const txStats = txManager.getResolutionStats();
    console.log(
      `  📊 Transaction manager resolution stats: ${txStats.enabled ? 'enabled' : 'disabled'}`
    );

    console.log('\n🎉 Resolution Layer demo completed successfully!');
  } catch (error) {
    console.error('❌ Resolution Layer demo failed:', error.message);
    throw error;
  }
}

// Run the demo
demonstrateResolutionLayer().catch(error => {
  console.error('💥 Demo failed:', error);
  process.exit(1);
});
