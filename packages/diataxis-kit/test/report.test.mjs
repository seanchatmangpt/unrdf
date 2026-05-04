/**
 * @file report.test.mjs
 * @description Tests for report CLI tool
 */

import { describe, it, beforeEach } from 'node:test';
import assert from 'node:assert/strict';
import { exec } from 'node:child_process';
import { promisify } from 'node:util';
import { mkdir, writeFile, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

const execAsync = promisify(exec);
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const binPath = join(__dirname, '..', 'bin', 'report.mjs');
const testArtifactsDir = join(__dirname, 'fixtures', 'test-artifacts');

/**
 * Create test inventory files
 */
async function setupTestInventories() {
  const diataxisDir = join(testArtifactsDir, 'ARTIFACTS', 'diataxis');
  await mkdir(diataxisDir, { recursive: true });

  // Inventory 1: High confidence package
  const inventory1 = {
    packageName: '@unrdf/test-pkg-1',
    version: '1.0.0',
    generatedAt: '2025-01-01T00:00:00.000Z',
    confidence: {
      tutorials: 0.9,
      howtos: 0.85,
      reference: 1.0,
      explanation: 0.8
    },
    tutorials: [
      {
        id: 'tutorial-getting-started',
        title: 'Getting Started',
        goal: 'Learn basics',
        prerequisites: [],
        stepsOutline: ['Install', 'Import', 'Use'],
        confidenceScore: 0.9,
        source: ['readme', 'examples']
      }
    ],
    howtos: [
      {
        id: 'howto-configure',
        title: 'Configure Package',
        task: 'Set up configuration',
        context: 'When customizing',
        steps: ['Create config', 'Set options'],
        confidenceScore: 0.85,
        source: ['readme']
      },
      {
        id: 'howto-use-cli',
        title: 'Use CLI',
        task: 'Use command line',
        context: 'When running CLI',
        steps: ['Install', 'Run'],
        confidenceScore: 1.0,
        source: ['bin']
      }
    ],
    reference: {
      id: 'reference-api',
      title: 'API Reference',
      items: [
        { name: 'export1', type: 'export', description: 'Export 1', example: 'import x' },
        { name: 'cli-tool', type: 'bin', description: 'CLI tool', example: 'cli-tool' }
      ],
      confidenceScore: 1.0,
      source: ['exports', 'bin']
    },
    explanation: {
      id: 'explanation-overview',
      title: 'Understanding Package',
      concepts: ['concept1', 'concept2'],
      architecture: 'This is the architecture',
      tradeoffs: ['Tradeoff 1', 'Tradeoff 2'],
      confidenceScore: 0.8,
      source: ['readme', 'keywords']
    },
    evidence: {
      readmeHeadings: ['Usage', 'API', 'Examples'],
      docsFiles: ['guide.md'],
      examplesFiles: ['example1.mjs', 'example2.mjs'],
      fingerprint: 'abc123'
    }
  };

  // Inventory 2: Low confidence package
  const inventory2 = {
    packageName: '@unrdf/test-pkg-2',
    version: '0.5.0',
    generatedAt: '2025-01-01T00:00:00.000Z',
    confidence: {
      tutorials: 0.3,
      howtos: 0.4,
      reference: 0.5,
      explanation: 0.4
    },
    tutorials: [
      {
        id: 'tutorial-basic',
        title: 'Basic Tutorial',
        goal: 'Learn',
        prerequisites: [],
        stepsOutline: ['Step 1'],
        confidenceScore: 0.3,
        source: ['inferred']
      }
    ],
    howtos: [
      {
        id: 'howto-use',
        title: 'Use Package',
        task: 'Use it',
        context: 'When needed',
        steps: ['Import', 'Use'],
        confidenceScore: 0.4,
        source: ['inferred']
      },
      {
        id: 'howto-troubleshoot',
        title: 'Troubleshoot',
        task: 'Fix issues',
        context: 'When broken',
        steps: ['Check', 'Fix'],
        confidenceScore: 0.4,
        source: ['inferred']
      }
    ],
    reference: {
      id: 'reference-api',
      title: 'API Reference',
      items: [
        { name: 'unknown', type: 'unknown', description: 'To be added', example: null }
      ],
      confidenceScore: 0.5,
      source: ['inferred']
    },
    explanation: {
      id: 'explanation-overview',
      title: 'Understanding Package',
      concepts: ['core'],
      architecture: 'Basic package',
      tradeoffs: ['Generic tradeoff'],
      confidenceScore: 0.4,
      source: ['inferred']
    },
    evidence: {
      readmeHeadings: [],
      docsFiles: [],
      examplesFiles: [],
      fingerprint: 'def456'
    }
  };

  // Inventory 3: Medium confidence package
  const inventory3 = {
    packageName: '@unrdf/test-pkg-3',
    version: '2.1.0',
    generatedAt: '2025-01-01T00:00:00.000Z',
    confidence: {
      tutorials: 0.6,
      howtos: 0.7,
      reference: 0.8,
      explanation: 0.65
    },
    tutorials: [
      {
        id: 'tutorial-start',
        title: 'Start Here',
        goal: 'Get started',
        prerequisites: [],
        stepsOutline: ['Install', 'Use'],
        confidenceScore: 0.6,
        source: ['readme']
      }
    ],
    howtos: [
      {
        id: 'howto-config',
        title: 'Configure',
        task: 'Configure',
        context: 'Setup',
        steps: ['Edit config'],
        confidenceScore: 0.7,
        source: ['readme']
      },
      {
        id: 'howto-integrate',
        title: 'Integrate',
        task: 'Integrate',
        context: 'Integration',
        steps: ['Setup'],
        confidenceScore: 0.7,
        source: ['keywords']
      }
    ],
    reference: {
      id: 'reference-api',
      title: 'API Reference',
      items: [
        { name: 'export1', type: 'export', description: 'Export', example: 'import' }
      ],
      confidenceScore: 0.8,
      source: ['exports']
    },
    explanation: {
      id: 'explanation-overview',
      title: 'Understanding Package',
      concepts: ['concept'],
      architecture: 'Architecture description',
      tradeoffs: ['Tradeoff'],
      confidenceScore: 0.65,
      source: ['readme']
    },
    evidence: {
      readmeHeadings: ['Getting Started', 'Usage'],
      docsFiles: [],
      examplesFiles: ['example.mjs'],
      fingerprint: 'ghi789'
    }
  };

  // Write inventory files
  await writeFile(
    join(diataxisDir, 'test-pkg-1.inventory.json'),
    JSON.stringify(inventory1, null, 2)
  );
  await writeFile(
    join(diataxisDir, 'test-pkg-2.inventory.json'),
    JSON.stringify(inventory2, null, 2)
  );
  await writeFile(
    join(diataxisDir, 'test-pkg-3.inventory.json'),
    JSON.stringify(inventory3, null, 2)
  );
}

/**
 * Clean up test artifacts
 */
async function cleanupTestInventories() {
  try {
    await rm(testArtifactsDir, { recursive: true, force: true });
  } catch (error) {
    // Ignore cleanup errors
  }
}

describe('report.mjs', () => {
  beforeEach(async () => {
    await cleanupTestInventories();
    await setupTestInventories();
  });

  describe('Test 1: Coverage summary calculation', () => {
    it('should calculate correct coverage percentages', async () => {
      const { stdout } = await execAsync(
        `cd "${testArtifactsDir}" && node "${binPath}"`
      );

      // Check summary section
      assert.ok(stdout.includes('Total packages:     3'));
      assert.ok(stdout.includes('With tutorials:     3 (100%)'));
      assert.ok(stdout.includes('With 2+ how-tos:    3 (100%)'));
      assert.ok(stdout.includes('With reference:     3 (100%)'));
      assert.ok(stdout.includes('With explanation:   3 (100%)'));
    });
  });

  describe('Test 2: Confidence stats (avg, min, max)', () => {
    it('should calculate correct confidence statistics', async () => {
      const { stdout } = await execAsync(
        `cd "${testArtifactsDir}" && node "${binPath}"`
      );

      // Check confidence section exists
      assert.ok(stdout.includes('CONFIDENCE'));

      // Check format (avg=X.XX, min=X.XX, max=X.XX)
      assert.match(stdout, /Tutorials\s+: avg=\d+\.\d{2}, min=\d+\.\d{2}, max=\d+\.\d{2}/);
      assert.match(stdout, /Howtos\s+: avg=\d+\.\d{2}, min=\d+\.\d{2}, max=\d+\.\d{2}/);
      assert.match(stdout, /Reference\s+: avg=\d+\.\d{2}, min=\d+\.\d{2}, max=\d+\.\d{2}/);
      assert.match(stdout, /Explanation\s+: avg=\d+\.\d{2}, min=\d+\.\d{2}, max=\d+\.\d{2}/);

      // Tutorials: avg=(0.9+0.3+0.6)/3=0.60, min=0.30, max=0.90
      assert.ok(stdout.includes('Tutorials   : avg=0.60, min=0.30, max=0.90'));
    });
  });

  describe('Test 3: Lowest confidence packages ranking', () => {
    it('should rank packages by lowest confidence', async () => {
      const { stdout } = await execAsync(
        `cd "${testArtifactsDir}" && node "${binPath}" --top 3`
      );

      // Check lowest confidence section
      assert.ok(stdout.includes('LOWEST CONFIDENCE (3 packages)'));

      // Package 2 should be first (avg=0.40)
      assert.ok(stdout.includes('@unrdf/test-pkg-2'));
      assert.ok(stdout.includes('(0.40)'));

      // Package 3 should be second (avg=0.69)
      assert.ok(stdout.includes('@unrdf/test-pkg-3'));
      assert.ok(stdout.includes('(0.69)'));

      // Package 1 should be last (avg=0.89)
      assert.ok(stdout.includes('@unrdf/test-pkg-1'));
      assert.ok(stdout.includes('(0.89)'));
    });

    it('should respect --top N option', async () => {
      const { stdout } = await execAsync(
        `cd "${testArtifactsDir}" && node "${binPath}" --top 1`
      );

      assert.ok(stdout.includes('LOWEST CONFIDENCE (1 packages)'));

      // Should only show one package
      const lines = stdout.split('\n').filter(line => line.match(/^\s*\d+\./));
      assert.equal(lines.length, 1);
    });
  });

  describe('Test 4: Missing evidence detection', () => {
    it('should detect and count missing evidence', async () => {
      const { stdout } = await execAsync(
        `cd "${testArtifactsDir}" && node "${binPath}"`
      );

      // Check missing evidence section
      assert.ok(stdout.includes('MISSING EVIDENCE'));

      // Package 2 has all evidence missing
      // Package 3 has no docs/
      // Package 1 has all evidence present

      // no examples/: 1 package (pkg-2)
      assert.match(stdout, /no examples\/\s*:\s*1 packages?/);

      // no docs/: 2 packages (pkg-2, pkg-3)
      assert.match(stdout, /no docs\/\s*:\s*2 packages?/);

      // empty README: 1 package (pkg-2)
      assert.match(stdout, /empty README\s*:\s*1 packages?/);

      // no bin entries: 2 packages (pkg-2, pkg-3)
      assert.match(stdout, /no bin entries\s*:\s*2 packages?/);
    });
  });

  describe('Test 5: JSON output format', () => {
    it('should generate valid JSON output', async () => {
      const { stdout } = await execAsync(
        `cd "${testArtifactsDir}" && node "${binPath}" --json`
      );

      // Parse JSON
      const report = JSON.parse(stdout);

      // Check structure
      assert.ok(report.summary);
      assert.equal(report.summary.total, 3);
      assert.equal(report.summary.withTutorials, 3);
      assert.equal(report.summary.with2PlusHowtos, 3);

      // Check confidence
      assert.ok(report.confidence);
      assert.ok(report.confidence.tutorials);
      assert.equal(report.confidence.tutorials.avg.toFixed(2), '0.60');
      assert.equal(report.confidence.tutorials.min, 0.3);
      assert.equal(report.confidence.tutorials.max, 0.9);

      // Check lowestConfidence
      assert.ok(Array.isArray(report.lowestConfidence));
      assert.equal(report.lowestConfidence.length, 3); // Only 3 packages total

      // First should be lowest (pkg-2)
      assert.equal(report.lowestConfidence[0].packageName, '@unrdf/test-pkg-2');
      assert.equal(report.lowestConfidence[0].avgConfidence, 0.40);

      // Check missingEvidence
      assert.ok(report.missingEvidence);
      assert.equal(report.missingEvidence['no docs/'], 2);
      assert.equal(report.missingEvidence['no examples/'], 1);

      // Check generatedAt
      assert.ok(report.generatedAt);
      assert.match(report.generatedAt, /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/);
    });
  });

  describe('Test 6: CSV output format', () => {
    it('should generate valid CSV output', async () => {
      const { stdout } = await execAsync(
        `cd "${testArtifactsDir}" && node "${binPath}" --csv`
      );

      const lines = stdout.trim().split('\n');

      // Check header
      assert.equal(
        lines[0],
        'Package,Tutorials,HowTos,HasReference,HasExplanation,AvgConfidence,TutorialsConf,HowtosConf,ReferenceConf,ExplanationConf,MissingEvidence'
      );

      // Check data rows (3 packages)
      assert.equal(lines.length, 4); // Header + 3 rows

      // Check pkg-1 row
      const pkg1Row = lines.find(line => line.startsWith('@unrdf/test-pkg-1'));
      assert.ok(pkg1Row);
      assert.ok(pkg1Row.includes(',1,2,yes,yes,0.89,0.90,0.85,1.00,0.80,'));

      // Check pkg-2 row (low confidence)
      const pkg2Row = lines.find(line => line.startsWith('@unrdf/test-pkg-2'));
      assert.ok(pkg2Row);
      assert.ok(pkg2Row.includes(',1,2,yes,yes,0.40,0.30,0.40,0.50,0.40,'));
      assert.ok(pkg2Row.includes('"no examples/; no docs/; empty README; no bin entries"'));
    });
  });

  describe('Test 7: Filter option', () => {
    it('should filter packages by keyword', async () => {
      const { stdout } = await execAsync(
        `cd "${testArtifactsDir}" && node "${binPath}" --filter "pkg-1"`
      );

      // Should only show pkg-1
      assert.ok(stdout.includes('Total packages:     1'));
      assert.ok(stdout.includes('@unrdf/test-pkg-1'));
      assert.ok(!stdout.includes('@unrdf/test-pkg-2'));
      assert.ok(!stdout.includes('@unrdf/test-pkg-3'));
    });
  });

  describe('Test 8: Sort option', () => {
    it('should sort by tutorials count', async () => {
      const { stdout } = await execAsync(
        `cd "${testArtifactsDir}" && node "${binPath}" --sort tutorials --top 3 --csv`
      );

      const lines = stdout.trim().split('\n').slice(1); // Skip header

      // All have 1 tutorial, so order should be stable (alphabetical)
      assert.ok(lines[0].startsWith('@unrdf/test-pkg-1'));
      assert.ok(lines[1].startsWith('@unrdf/test-pkg-2'));
      assert.ok(lines[2].startsWith('@unrdf/test-pkg-3'));
    });

    it('should sort by confidence (default)', async () => {
      const { stdout } = await execAsync(
        `cd "${testArtifactsDir}" && node "${binPath}" --sort confidence --csv`
      );

      const lines = stdout.trim().split('\n').slice(1); // Skip header

      // Should be sorted by avgConfidence descending
      // pkg-1: 0.89, pkg-3: 0.69, pkg-2: 0.40
      assert.ok(lines[0].startsWith('@unrdf/test-pkg-1'));
      assert.ok(lines[1].startsWith('@unrdf/test-pkg-3'));
      assert.ok(lines[2].startsWith('@unrdf/test-pkg-2'));
    });
  });

  describe('Test 9: Empty inventory handling', () => {
    it('should handle missing ARTIFACTS directory gracefully', async () => {
      await cleanupTestInventories();
      // Create test directory again so cd works, but without ARTIFACTS
      await mkdir(testArtifactsDir, { recursive: true });

      const { stdout } = await execAsync(
        `cd "${testArtifactsDir}" && node "${binPath}"`
      );

      assert.ok(stdout.includes('No inventory generated'));
    });
  });

  describe('Test 10: Help option', () => {
    it('should display help message', async () => {
      const { stdout } = await execAsync(`node "${binPath}" --help`);

      assert.ok(stdout.includes('Diátaxis Coverage Report Generator'));
      assert.ok(stdout.includes('Usage:'));
      assert.ok(stdout.includes('--json'));
      assert.ok(stdout.includes('--csv'));
      assert.ok(stdout.includes('--top'));
      assert.ok(stdout.includes('--filter'));
      assert.ok(stdout.includes('--sort'));
    });
  });
});
