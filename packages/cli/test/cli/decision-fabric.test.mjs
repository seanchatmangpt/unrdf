/**
 * Scenario-based CLI Tests for Hyperdimensional Decision Fabric Commands
 *
 * Uses citty-test-utils scenario DSL for testing CLI workflows.
 *
 * Tests cover:
 * - decision command (strategic decision processing)
 * - pareto command (feature analysis)
 * - socratic command (assumption extraction)
 *
 * @module cli/test/decision-fabric
 */

import { describe, it } from '@jest/globals';
import { scenario } from 'citty-test-utils';
import { writeFileSync, mkdirSync, rmSync } from 'node:fs';
import { join } from 'node:path';

const FIXTURES_DIR = join(process.cwd(), 'test', 'fixtures', 'decision-fabric');

// Setup fixtures directory
function setupFixtures() {
  try {
    mkdirSync(FIXTURES_DIR, { recursive: true });

    // KGC 4D example features (from thesis)
    const kgc4dFeatures = {
      features: [
        { id: 1, name: 'BigInt Time', value: 95, cost: 20, description: 'Nanosecond precision timestamps' },
        { id: 2, name: 'Event Log', value: 85, cost: 50, description: 'Immutable event sourcing' },
        { id: 3, name: 'Named Graphs', value: 80, cost: 30, description: 'Multi-graph support' },
        { id: 4, name: 'Freeze', value: 75, cost: 150, description: 'Snapshot creation' },
        { id: 5, name: 'Time-Travel', value: 70, cost: 200, description: '4D reconstruction' },
        { id: 6, name: 'Receipt', value: 60, cost: 80, description: 'Cryptographic receipts' },
        { id: 7, name: 'React UI', value: 40, cost: 300, description: 'Web interface' },
        { id: 8, name: 'Advanced Hooks', value: 30, cost: 500, description: 'Governance framework' }
      ]
    };

    writeFileSync(
      join(FIXTURES_DIR, 'kgc4d-features.json'),
      JSON.stringify(kgc4dFeatures, null, 2)
    );

    // Simple feature set (meets 80/20)
    const simpleFeatures = {
      features: [
        { id: 1, name: 'Core API', value: 95, cost: 50 },
        { id: 2, name: 'Analytics', value: 80, cost: 30 },
        { id: 3, name: 'Dashboard', value: 40, cost: 200 }
      ]
    };

    writeFileSync(
      join(FIXTURES_DIR, 'simple-features.json'),
      JSON.stringify(simpleFeatures, null, 2)
    );

    // High entropy feature set (iterative recommended)
    const complexFeatures = {
      features: Array.from({ length: 20 }, (_, i) => ({
        id: i + 1,
        name: `Feature ${i + 1}`,
        value: Math.floor(Math.random() * 100),
        cost: Math.floor(Math.random() * 200)
      }))
    };

    writeFileSync(
      join(FIXTURES_DIR, 'complex-features.json'),
      JSON.stringify(complexFeatures, null, 2)
    );

  } catch (error) {
    console.error('Failed to setup fixtures:', error);
  }
}

// Cleanup fixtures
function cleanupFixtures() {
  try {
    rmSync(FIXTURES_DIR, { recursive: true, force: true });
  } catch (error) {
    // Ignore cleanup errors
  }
}

describe('Decision Command - Scenario Tests', () => {
  beforeAll(() => {
    setupFixtures();
  });

  afterAll(() => {
    cleanupFixtures();
  });

  it('should show help when --help flag is provided', async () => {
    await scenario('decision --help')
      .step('Show help', ['decision', '--help'])
      .expectSuccess()
      .expectOutput(/Process strategic decision/)
      .expectOutput(/--features/)
      .execute();
  });

  it('should process simple decision without features', async () => {
    await scenario('decision simple statement')
      .step('Process decision', ['decision', 'Implement fraud detection system'])
      .expectSuccess()
      .expectOutput(/HYPERDIMENSIONAL DECISION FABRIC/)
      .expectOutput(/Status:/)
      .execute();
  });

  it('should process decision with features (KGC 4D example)', async () => {
    await scenario('decision with KGC 4D features')
      .step('Process decision', [
        'decision',
        'Implement KGC 4D event logging',
        '--features', join(FIXTURES_DIR, 'kgc4d-features.json')
      ])
      .expectSuccess()
      .expectOutput(/PARETO FRONTIER/)
      .expectOutput(/Big Bang 80\/20/)
      .expectOutput(/2-3 hours/)
      .expectOutput(/99\.99%/)
      .execute();
  });

  it('should reject vague statements', async () => {
    await scenario('decision with vague statement')
      .step('Process vague decision', ['decision', 'We need to optimize the system'])
      .expectExit(1) // Should block
      .expectOutput(/BLOCKED/)
      .expectOutput(/CHALLENGES/)
      .execute();
  });

  it('should output JSON format', async () => {
    await scenario('decision JSON output')
      .step('Process with JSON output', [
        'decision',
        'Implement feature X',
        '--output', 'json'
      ])
      .expectSuccess()
      .expectOutput(/"status"/)
      .expectOutput(/"confidence"/)
      .expectOutput(/"socratic_analysis"/)
      .execute();
  });
});

describe('Pareto Command - Scenario Tests', () => {
  beforeAll(() => {
    setupFixtures();
  });

  afterAll(() => {
    cleanupFixtures();
  });

  it('should show help when --help flag is provided', async () => {
    await scenario('pareto --help')
      .step('Show help', ['pareto', '--help'])
      .expectSuccess()
      .expectOutput(/Analyze feature set/)
      .expectOutput(/Big Bang 80\/20/)
      .execute();
  });

  it('should analyze KGC 4D features (BB80/20 applicable)', async () => {
    await scenario('pareto KGC 4D features')
      .step('Analyze features', ['pareto', join(FIXTURES_DIR, 'kgc4d-features.json')])
      .expectSuccess()
      .expectOutput(/BIG BANG 80\/20/)
      .expectOutput(/PARETO FRONTIER/)
      .expectOutput(/Methodology: Big Bang 80\/20/)
      .expectOutput(/H_spec:/)
      .expectOutput(/BOUNDED/)
      .execute();
  });

  it('should validate 80/20 rule for simple features', async () => {
    await scenario('pareto simple features')
      .step('Analyze simple features', ['pareto', join(FIXTURES_DIR, 'simple-features.json')])
      .expectSuccess()
      .expectOutput(/80\/20 RULE/)
      .expectOutput(/MEETS 80\/20/)
      .execute();
  });

  it('should recommend iterative for high entropy', async () => {
    await scenario('pareto complex features')
      .step('Analyze complex features', ['pareto', join(FIXTURES_DIR, 'complex-features.json')])
      .expectExit(1) // Warns about high entropy
      .expectOutput(/Specification entropy too high/)
      .execute();
  });

  it('should output JSON format', async () => {
    await scenario('pareto JSON output')
      .step('Output JSON', [
        'pareto',
        join(FIXTURES_DIR, 'simple-features.json'),
        '--output', 'json'
      ])
      .expectSuccess()
      .expectOutput(/"methodology"/)
      .expectOutput(/"pareto_frontier"/)
      .expectOutput(/"specification_entropy"/)
      .execute();
  });

  it('should output chart data', async () => {
    await scenario('pareto chart output')
      .step('Output chart', [
        'pareto',
        join(FIXTURES_DIR, 'simple-features.json'),
        '--output', 'chart'
      ])
      .expectSuccess()
      .expectOutput(/"feature"/)
      .expectOutput(/"value"/)
      .expectOutput(/"cost"/)
      .expectOutput(/"efficiency"/)
      .execute();
  });
});

describe('Socratic Command - Scenario Tests', () => {
  it('should show help when --help flag is provided', async () => {
    await scenario('socratic --help')
      .step('Show help', ['socratic', '--help'])
      .expectSuccess()
      .expectOutput(/Challenge assumptions/)
      .expectOutput(/Socratic AI/)
      .execute();
  });

  it('should detect causal assumptions', async () => {
    await scenario('socratic causal assumption')
      .step('Analyze causal', ['socratic', 'Adding feature X will solve problem Y'])
      .expectExit(1) // Should block due to unvalidated causal claim
      .expectOutput(/ASSUMPTIONS EXTRACTED/)
      .expectOutput(/causes/)
      .expectOutput(/DO NOT PROCEED/)
      .execute();
  });

  it('should detect vague optimization', async () => {
    await scenario('socratic vague optimization')
      .step('Analyze vague', ['socratic', 'We need to optimize the conversion rate'])
      .expectExit(1) // Should block
      .expectOutput(/VAGUE/)
      .expectOutput(/CLARIFICATION/)
      .expectOutput(/HIGH SEVERITY/)
      .execute();
  });

  it('should detect absolute claims', async () => {
    await scenario('socratic absolute claim')
      .step('Analyze absolute', ['socratic', 'All users always abandon at the payment step'])
      .expectExit(1) // Should challenge
      .expectOutput(/Absolute claim/)
      .expectOutput(/CHALLENGES/)
      .execute();
  });

  it('should accept well-formed statements', async () => {
    await scenario('socratic well-formed')
      .step('Analyze well-formed', ['socratic', 'Implement authentication using OAuth2 with JWT tokens'])
      .expectSuccess()
      .expectOutput(/PROCEED/)
      .execute();
  });

  it('should output JSON format', async () => {
    await scenario('socratic JSON output')
      .step('Output JSON', [
        'socratic',
        'Implement feature X',
        '--output', 'json'
      ])
      .expectOutput(/"original_statement"/)
      .expectOutput(/"assumptions"/)
      .expectOutput(/"challenges"/)
      .expectOutput(/"recommendation"/)
      .execute();
  });
});

describe('Integrated Workflow - Scenario Tests', () => {
  beforeAll(() => {
    setupFixtures();
  });

  afterAll(() => {
    cleanupFixtures();
  });

  it('should complete full BB80/20 workflow', async () => {
    await scenario('full BB80/20 workflow')
      // Step 1: Socratic analysis
      .step('Challenge assumptions', ['socratic', 'Implement KGC 4D-style event logging'])
      .expectSuccess()
      .expectOutput(/PROCEED/)

      // Step 2: Pareto analysis
      .step('Analyze features', ['pareto', join(FIXTURES_DIR, 'kgc4d-features.json')])
      .expectSuccess()
      .expectOutput(/Big Bang 80\/20/)

      // Step 3: Decision processing
      .step('Process decision', [
        'decision',
        'Implement KGC 4D-style event logging',
        '--features', join(FIXTURES_DIR, 'kgc4d-features.json')
      ])
      .expectSuccess()
      .expectOutput(/ACCEPTED/)
      .expectOutput(/IMPLEMENT_BB8020/)
      .expectOutput(/2-3 hours/)

      .execute();
  });

  it('should block invalid workflow early', async () => {
    await scenario('blocked workflow')
      // Step 1: Socratic blocks due to vague statement
      .step('Challenge vague statement', ['socratic', 'We need to optimize the system'])
      .expectExit(1)
      .expectOutput(/DO NOT PROCEED/)

      .execute();
  });
});

describe('Error Handling - Scenario Tests', () => {
  it('should error on missing features file', async () => {
    await scenario('missing features file')
      .step('Process with missing file', [
        'decision',
        'Test decision',
        '--features', 'nonexistent.json'
      ])
      .expectExit(1)
      .expectOutput(/Error/)
      .execute();
  });

  it('should error on invalid JSON', async () => {
    const invalidFile = join(FIXTURES_DIR, 'invalid.json');
    mkdirSync(FIXTURES_DIR, { recursive: true });
    writeFileSync(invalidFile, 'not valid json');

    await scenario('invalid JSON file')
      .step('Process with invalid JSON', ['pareto', invalidFile])
      .expectExit(1)
      .expectOutput(/Error/)
      .execute();

    rmSync(invalidFile, { force: true });
  });
});
