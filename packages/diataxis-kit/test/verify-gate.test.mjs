/**
 * @file Tests for Diátaxis verification gate
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { mkdir, writeFile, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { main, verifyPackage, isReferencPopulated, isExplanationPopulated } from '../bin/verify.mjs';
import { stableStringify } from '../src/stable-json.mjs';

const execFileAsync = promisify(execFile);

/**
 * Create a test workspace with inventory and package files
 * @param {string} testDir - Test directory path
 * @param {Object} config - Configuration
 * @param {Array} config.packages - Array of package configurations
 * @returns {Promise<void>}
 */
async function createTestWorkspace(testDir, config) {
  const artifactsDir = join(testDir, 'ARTIFACTS', 'diataxis');
  await mkdir(artifactsDir, { recursive: true });

  // Create inventory.json
  const inventory = {
    packages: config.packages.map(pkg => ({
      name: pkg.name,
      version: pkg.version || '1.0.0'
    }))
  };

  await writeFile(
    join(artifactsDir, 'inventory.json'),
    stableStringify(inventory)
  );

  // Create per-package diataxis.json files
  for (const pkg of config.packages) {
    const packageDir = join(artifactsDir, pkg.name);
    await mkdir(packageDir, { recursive: true });

    if (pkg.diataxis) {
      await writeFile(
        join(packageDir, 'diataxis.json'),
        stableStringify(pkg.diataxis)
      );
    }
  }
}

/**
 * Create a valid diataxis entry
 * @param {string} packageName - Package name
 * @param {Object} overrides - Override default values
 * @returns {Object} Diataxis entry
 */
function createDiataxisEntry(packageName, overrides = {}) {
  return {
    packageName,
    version: '1.0.0',
    generatedAt: '2000-01-01T00:00:00.000Z',
    confidence: {
      tutorials: 0.8,
      howtos: 0.9,
      reference: 0.95,
      explanation: 0.85
    },
    tutorials: [
      {
        id: 'getting-started',
        title: 'Getting Started',
        goal: 'Learn the basics',
        prerequisites: [],
        stepsOutline: ['Step 1', 'Step 2'],
        confidenceScore: 0.8,
        source: ['README.md']
      }
    ],
    howtos: [
      {
        id: 'install',
        title: 'How to Install',
        task: 'Install the package',
        context: 'Setup',
        steps: ['npm install'],
        confidenceScore: 0.9,
        source: ['README.md']
      },
      {
        id: 'configure',
        title: 'How to Configure',
        task: 'Configure the package',
        context: 'Setup',
        steps: ['Edit config'],
        confidenceScore: 0.85,
        source: ['README.md']
      }
    ],
    reference: {
      id: 'reference',
      title: `${packageName} Reference`,
      items: [
        {
          name: 'someFunction',
          type: 'export',
          description: 'A function',
          example: 'someFunction()'
        }
      ],
      confidenceScore: 0.95,
      source: ['src/index.js']
    },
    explanation: {
      id: 'explanation',
      title: `${packageName} Explanation`,
      concepts: ['Core concept'],
      architecture: 'Simple architecture',
      tradeoffs: ['Tradeoff 1'],
      confidenceScore: 0.85,
      source: ['README.md']
    },
    evidence: {
      readmeHeadings: ['Installation', 'Usage'],
      docsFiles: ['README.md'],
      examplesFiles: [],
      fingerprint: 'abc123'
    },
    ...overrides
  };
}

describe('Diátaxis Verification Gate', () => {
  it('Test 1: Pass when all packages have required stubs', async () => {
    const testDir = join(tmpdir(), `verify-test-${Date.now()}-1`);

    try {
      await createTestWorkspace(testDir, {
        packages: [
          {
            name: '@unrdf/package-a',
            diataxis: createDiataxisEntry('@unrdf/package-a')
          },
          {
            name: '@unrdf/package-b',
            diataxis: createDiataxisEntry('@unrdf/package-b')
          }
        ]
      });

      // Run verification from test directory
      const originalCwd = process.cwd();
      try {
        process.chdir(testDir);
        const exitCode = await main({ json: false, failFast: false, threshold: 0 });
        assert.equal(exitCode, 0, 'Should exit with 0 when all packages pass');
      } finally {
        process.chdir(originalCwd);
      }
    } finally {
      await rm(testDir, { recursive: true, force: true });
    }
  });

  it('Test 2: Fail when a package missing tutorials', async () => {
    const testDir = join(tmpdir(), `verify-test-${Date.now()}-2`);

    try {
      await createTestWorkspace(testDir, {
        packages: [
          {
            name: '@unrdf/package-a',
            diataxis: createDiataxisEntry('@unrdf/package-a', {
              tutorials: [] // Missing tutorials
            })
          }
        ]
      });

      const originalCwd = process.cwd();
      try {
        process.chdir(testDir);
        const exitCode = await main({ json: false, failFast: false, threshold: 0 });
        assert.equal(exitCode, 1, 'Should exit with 1 when tutorials missing');
      } finally {
        process.chdir(originalCwd);
      }
    } finally {
      await rm(testDir, { recursive: true, force: true });
    }
  });

  it('Test 3: Fail when a package missing 2+ how-tos', async () => {
    const testDir = join(tmpdir(), `verify-test-${Date.now()}-3`);

    try {
      await createTestWorkspace(testDir, {
        packages: [
          {
            name: '@unrdf/package-a',
            diataxis: createDiataxisEntry('@unrdf/package-a', {
              howtos: [
                {
                  id: 'install',
                  title: 'How to Install',
                  task: 'Install',
                  context: 'Setup',
                  steps: [],
                  confidenceScore: 0.9,
                  source: []
                }
              ] // Only 1 how-to, need 2
            })
          }
        ]
      });

      const originalCwd = process.cwd();
      try {
        process.chdir(testDir);
        const exitCode = await main({ json: false, failFast: false, threshold: 0 });
        assert.equal(exitCode, 1, 'Should exit with 1 when how-tos < 2');
      } finally {
        process.chdir(originalCwd);
      }
    } finally {
      await rm(testDir, { recursive: true, force: true });
    }
  });

  it('Test 4: Fail when missing reference', async () => {
    const testDir = join(tmpdir(), `verify-test-${Date.now()}-4`);

    try {
      await createTestWorkspace(testDir, {
        packages: [
          {
            name: '@unrdf/package-a',
            diataxis: createDiataxisEntry('@unrdf/package-a', {
              reference: {
                id: 'reference',
                title: 'Reference',
                items: [], // Empty items
                confidenceScore: 0,
                source: []
              }
            })
          }
        ]
      });

      const originalCwd = process.cwd();
      try {
        process.chdir(testDir);
        const exitCode = await main({ json: false, failFast: false, threshold: 0 });
        assert.equal(exitCode, 1, 'Should exit with 1 when reference empty');
      } finally {
        process.chdir(originalCwd);
      }
    } finally {
      await rm(testDir, { recursive: true, force: true });
    }
  });

  it('Test 5: Fail when missing explanation', async () => {
    const testDir = join(tmpdir(), `verify-test-${Date.now()}-5`);

    try {
      await createTestWorkspace(testDir, {
        packages: [
          {
            name: '@unrdf/package-a',
            diataxis: createDiataxisEntry('@unrdf/package-a', {
              explanation: {
                id: 'explanation',
                title: 'Explanation',
                concepts: [],
                architecture: '',
                tradeoffs: [],
                confidenceScore: 0,
                source: []
              }
            })
          }
        ]
      });

      const originalCwd = process.cwd();
      try {
        process.chdir(testDir);
        const exitCode = await main({ json: false, failFast: false, threshold: 0 });
        assert.equal(exitCode, 1, 'Should exit with 1 when explanation empty');
      } finally {
        process.chdir(originalCwd);
      }
    } finally {
      await rm(testDir, { recursive: true, force: true });
    }
  });

  it('Test 6: JSON output mode', async () => {
    const testDir = join(tmpdir(), `verify-test-${Date.now()}-6`);

    try {
      await createTestWorkspace(testDir, {
        packages: [
          {
            name: '@unrdf/package-a',
            diataxis: createDiataxisEntry('@unrdf/package-a')
          },
          {
            name: '@unrdf/package-b',
            diataxis: createDiataxisEntry('@unrdf/package-b', {
              tutorials: []
            })
          }
        ]
      });

      // Capture stdout
      const originalCwd = process.cwd();
      const originalLog = console.log;
      let output = '';
      console.log = (msg) => { output += msg; };

      try {
        process.chdir(testDir);
        const exitCode = await main({ json: true, failFast: false, threshold: 0 });

        assert.equal(exitCode, 1, 'Should exit with 1');

        // Parse JSON output
        const result = JSON.parse(output);
        assert.equal(result.total, 2, 'Should report 2 total packages');
        assert.equal(result.passing, 1, 'Should report 1 passing');
        assert.equal(result.failing, 1, 'Should report 1 failing');
        assert.equal(result.exitCode, 1, 'Should include exit code');
        assert.ok(Array.isArray(result.failures), 'Should include failures array');
        assert.equal(result.failures.length, 1, 'Should have 1 failure');
        assert.equal(result.failures[0].package, '@unrdf/package-b', 'Should identify failing package');
      } finally {
        console.log = originalLog;
        process.chdir(originalCwd);
      }
    } finally {
      await rm(testDir, { recursive: true, force: true });
    }
  });

  it('Test 7: --threshold option', async () => {
    const testDir = join(tmpdir(), `verify-test-${Date.now()}-7`);

    try {
      await createTestWorkspace(testDir, {
        packages: [
          {
            name: '@unrdf/package-a',
            diataxis: createDiataxisEntry('@unrdf/package-a', {
              tutorials: []
            })
          },
          {
            name: '@unrdf/package-b',
            diataxis: createDiataxisEntry('@unrdf/package-b')
          }
        ]
      });

      const originalCwd = process.cwd();
      try {
        process.chdir(testDir);

        // With threshold = 0, should fail (1 failure > 0)
        let exitCode = await main({ json: false, failFast: false, threshold: 0 });
        assert.equal(exitCode, 1, 'Should fail with threshold 0');

        // With threshold = 1, should pass (1 failure <= 1)
        exitCode = await main({ json: false, failFast: false, threshold: 1 });
        assert.equal(exitCode, 0, 'Should pass with threshold 1');

        // With threshold = 2, should pass (1 failure <= 2)
        exitCode = await main({ json: false, failFast: false, threshold: 2 });
        assert.equal(exitCode, 0, 'Should pass with threshold 2');
      } finally {
        process.chdir(originalCwd);
      }
    } finally {
      await rm(testDir, { recursive: true, force: true });
    }
  });

  it('Unit test: verifyPackage function', () => {
    // Valid package
    const validEntry = createDiataxisEntry('test-pkg');
    const result1 = verifyPackage('test-pkg', validEntry);
    assert.equal(result1.passing, true, 'Valid entry should pass');
    assert.equal(result1.failures.length, 0, 'Valid entry should have no failures');

    // Missing tutorials
    const noTutorials = createDiataxisEntry('test-pkg', { tutorials: [] });
    const result2 = verifyPackage('test-pkg', noTutorials);
    assert.equal(result2.passing, false, 'Should fail without tutorials');
    assert.ok(result2.failures.some(f => f.includes('tutorials')), 'Should mention tutorials');

    // Not enough how-tos
    const oneHowto = createDiataxisEntry('test-pkg', {
      howtos: [
        {
          id: 'install',
          title: 'Install',
          task: 'Install',
          context: 'Setup',
          steps: [],
          confidenceScore: 0.9,
          source: []
        }
      ]
    });
    const result3 = verifyPackage('test-pkg', oneHowto);
    assert.equal(result3.passing, false, 'Should fail with only 1 how-to');
    assert.ok(result3.failures.some(f => f.includes('how-tos')), 'Should mention how-tos');
  });

  it('Unit test: isReferencePopulated function', () => {
    // Reference with items
    assert.equal(
      isReferencPopulated({
        items: [{ name: 'test', type: 'export', description: 'test' }],
        confidenceScore: 0.9
      }),
      true,
      'Reference with items should be populated'
    );

    // Reference with confidence but no items
    assert.equal(
      isReferencPopulated({
        items: [],
        confidenceScore: 0.5
      }),
      true,
      'Reference with confidence > 0 should be populated'
    );

    // Empty reference
    assert.equal(
      isReferencPopulated({
        items: [],
        confidenceScore: 0
      }),
      false,
      'Reference with no items and 0 confidence should not be populated'
    );

    // Null/undefined
    assert.equal(isReferencPopulated(null), false, 'Null should not be populated');
    assert.equal(isReferencPopulated(undefined), false, 'Undefined should not be populated');
  });

  it('Unit test: isExplanationPopulated function', () => {
    // Explanation with concepts
    assert.equal(
      isExplanationPopulated({
        concepts: ['Concept 1'],
        architecture: '',
        tradeoffs: [],
        confidenceScore: 0
      }),
      true,
      'Explanation with concepts should be populated'
    );

    // Explanation with architecture
    assert.equal(
      isExplanationPopulated({
        concepts: [],
        architecture: 'Some architecture',
        tradeoffs: [],
        confidenceScore: 0
      }),
      true,
      'Explanation with architecture should be populated'
    );

    // Explanation with tradeoffs
    assert.equal(
      isExplanationPopulated({
        concepts: [],
        architecture: '',
        tradeoffs: ['Tradeoff 1'],
        confidenceScore: 0
      }),
      true,
      'Explanation with tradeoffs should be populated'
    );

    // Explanation with confidence but no content
    assert.equal(
      isExplanationPopulated({
        concepts: [],
        architecture: '',
        tradeoffs: [],
        confidenceScore: 0.5
      }),
      true,
      'Explanation with confidence > 0 should be populated'
    );

    // Empty explanation
    assert.equal(
      isExplanationPopulated({
        concepts: [],
        architecture: '',
        tradeoffs: [],
        confidenceScore: 0
      }),
      false,
      'Explanation with no content should not be populated'
    );

    // Null/undefined
    assert.equal(isExplanationPopulated(null), false, 'Null should not be populated');
    assert.equal(isExplanationPopulated(undefined), false, 'Undefined should not be populated');
  });

  it('Edge case: --fail-fast option', async () => {
    const testDir = join(tmpdir(), `verify-test-${Date.now()}-failfast`);

    try {
      await createTestWorkspace(testDir, {
        packages: [
          {
            name: '@unrdf/package-a',
            diataxis: createDiataxisEntry('@unrdf/package-a', { tutorials: [] })
          },
          {
            name: '@unrdf/package-b',
            diataxis: createDiataxisEntry('@unrdf/package-b', { tutorials: [] })
          },
          {
            name: '@unrdf/package-c',
            diataxis: createDiataxisEntry('@unrdf/package-c')
          }
        ]
      });

      // Capture stdout to check behavior
      const originalCwd = process.cwd();
      const originalLog = console.log;
      let output = '';
      console.log = (msg) => { output += msg; };

      try {
        process.chdir(testDir);
        const exitCode = await main({ json: true, failFast: true, threshold: 0 });

        assert.equal(exitCode, 1, 'Should exit with 1');

        const result = JSON.parse(output);
        // With fail-fast, should stop after first failure
        // Total should be 1 (only checked package-a)
        assert.equal(result.total, 1, 'Should only check until first failure');
        assert.equal(result.failing, 1, 'Should have 1 failing');
      } finally {
        console.log = originalLog;
        process.chdir(originalCwd);
      }
    } finally {
      await rm(testDir, { recursive: true, force: true });
    }
  });

  it('Edge case: Missing diataxis.json file', async () => {
    const testDir = join(tmpdir(), `verify-test-${Date.now()}-missing`);

    try {
      // Create inventory but no diataxis.json files
      const artifactsDir = join(testDir, 'ARTIFACTS', 'diataxis');
      await mkdir(artifactsDir, { recursive: true });

      await writeFile(
        join(artifactsDir, 'inventory.json'),
        stableStringify({
          packages: [{ name: '@unrdf/package-a', version: '1.0.0' }]
        })
      );

      const originalCwd = process.cwd();
      try {
        process.chdir(testDir);
        const exitCode = await main({ json: false, failFast: false, threshold: 0 });
        assert.equal(exitCode, 1, 'Should fail when diataxis.json missing');
      } finally {
        process.chdir(originalCwd);
      }
    } finally {
      await rm(testDir, { recursive: true, force: true });
    }
  });

  it('Edge case: Invalid JSON in diataxis.json', async () => {
    const testDir = join(tmpdir(), `verify-test-${Date.now()}-invalid`);

    try {
      const artifactsDir = join(testDir, 'ARTIFACTS', 'diataxis');
      await mkdir(artifactsDir, { recursive: true });

      await writeFile(
        join(artifactsDir, 'inventory.json'),
        stableStringify({
          packages: [{ name: '@unrdf/package-a', version: '1.0.0' }]
        })
      );

      const packageDir = join(artifactsDir, '@unrdf/package-a');
      await mkdir(packageDir, { recursive: true });
      await writeFile(join(packageDir, 'diataxis.json'), 'invalid json{');

      const originalCwd = process.cwd();
      try {
        process.chdir(testDir);
        const exitCode = await main({ json: false, failFast: false, threshold: 0 });
        assert.equal(exitCode, 1, 'Should fail when JSON invalid');
      } finally {
        process.chdir(originalCwd);
      }
    } finally {
      await rm(testDir, { recursive: true, force: true });
    }
  });
});
