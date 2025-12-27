/**
 * @file orchestrator-demo.mjs
 * @description Demonstration of KGCSwarmOrchestrator and TokenGenerator
 */

import { createOrchestrator, createTokenGenerator } from '../src/index.mjs';

async function main() {
  console.log('='.repeat(60));
  console.log('KGC-SWARM Orchestrator & Token Generator Demo');
  console.log('='.repeat(60));

  // 1. Create token generator
  console.log('\n1. Token Generator G(σ, κ)');
  console.log('-'.repeat(60));

  const generator = createTokenGenerator({ deterministic: true });

  const seed = { seed: 42, context: 'Initialize swarm', priming: ['start'] };
  const control = { temperature: 0.7, maxTokens: 5 };

  const tokens = generator.emit(seed, control);

  console.log(`Seed parameter σ: seed=${seed.seed}, context="${seed.context}"`);
  console.log(`Control parameter κ: temperature=${control.temperature}, maxTokens=${control.maxTokens}`);
  console.log(`\nGenerated ${tokens.length} tokens:`);

  tokens.forEach((token, i) => {
    console.log(`  [${token.position}] "${token.value}" (logProb: ${token.logProb.toFixed(4)})`);
  });

  // 2. Create orchestrator with budget
  console.log('\n2. Orchestrator Execution Loop');
  console.log('-'.repeat(60));

  const orchestrator = createOrchestrator({
    budget: {
      maxTime: 10000,    // 10 seconds
      maxSteps: 20,      // 20 steps max
      maxBytes: 10 * 1024 * 1024  // 10MB
    },
    storeObservations: true
  });

  console.log('Budget constraints:');
  console.log(`  - maxTime: ${orchestrator.budget.maxTime}ms`);
  console.log(`  - maxSteps: ${orchestrator.budget.maxSteps}`);
  console.log(`  - maxBytes: ${orchestrator.budget.maxBytes} bytes`);

  // 3. Run orchestration
  console.log('\n3. Execution Loop: τ := 0; while ¬stop { ... }');
  console.log('-'.repeat(60));

  const result = await orchestrator.run(
    { seed: 123, context: 'Swarm coordination' },
    { temperature: 0.8, maxTokens: 3 },
    {
      onEpoch: (τ, observations) => {
        console.log(`  Epoch τ=${τ}: ${observations.length} observations`);
      },
      shouldStop: (state) => state.currentEpoch >= 3  // Stop after 3 epochs
    }
  );

  console.log('\nExecution complete:');
  console.log(`  - Epochs completed: ${result.epochs}`);
  console.log(`  - Total steps: ${result.totalSteps}`);
  console.log(`  - Total observations: ${result.observations}`);
  console.log(`  - Duration: ${result.duration}ms`);
  console.log(`  - Stop reason: ${result.stopReason}`);

  // 4. Query observations
  console.log('\n4. Observable Space O_τ');
  console.log('-'.repeat(60));

  const allObs = orchestrator.getObservations();
  console.log(`Total observations in O_τ: ${allObs.length}`);

  for (let τ = 0; τ < result.epochs; τ++) {
    const epochObs = orchestrator.getObservations(τ);
    console.log(`  Epoch ${τ}: ${epochObs.length} observations`);
  }

  // 5. RDF Store
  console.log('\n5. RDF Store Integration');
  console.log('-'.repeat(60));

  const store = orchestrator.getStore();
  if (store) {
    console.log(`RDF triples stored: ${store.size}`);
    console.log('Observations are stored as RDF quads with full provenance');
  } else {
    console.log('RDF store disabled');
  }

  // 6. State
  console.log('\n6. Orchestrator State');
  console.log('-'.repeat(60));

  const state = orchestrator.getState();
  console.log(`Current epoch τ: ${state.currentEpoch}`);
  console.log(`Total steps: ${state.totalSteps}`);
  console.log(`Stopped: ${state.stopped}`);
  console.log(`Stop reason: ${state.stopReason || 'N/A'}`);

  console.log('\n' + '='.repeat(60));
  console.log('Demo Complete ✅');
  console.log('='.repeat(60));
}

main().catch(console.error);
