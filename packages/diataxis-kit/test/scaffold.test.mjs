/**
 * @file scaffold.test.mjs
 * @description Tests for scaffold generator module
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { generateScaffold } from '../src/scaffold.mjs';
import { createDiataxisEntry } from '../src/diataxis-schema.mjs';
import { readFile, rm } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const testOutputDir = join(__dirname, 'fixtures', 'scaffold-output');

describe('scaffold.mjs', () => {
  describe('generateScaffold', () => {
    it('should generate complete scaffold structure', async () => {
      // Clean up test output directory
      if (existsSync(testOutputDir)) {
        await rm(testOutputDir, { recursive: true, force: true });
      }

      // Create a minimal DiataxisEntry
      const entry = createDiataxisEntry('test-package', '1.0.0', {
        readmeHeadings: ['Installation', 'Usage'],
        docsFiles: ['api.md'],
        examplesFiles: ['example.js'],
        tutorials: [
          {
            id: 'getting-started',
            title: 'Getting Started',
            goal: 'Learn the basics',
            prerequisites: ['Node.js 18+'],
            stepsOutline: ['Install package', 'Create config', 'Run first command'],
            confidenceScore: 0.9,
            source: ['README.md', 'docs/tutorial.md']
          }
        ],
        howtos: [
          {
            id: 'configure-options',
            title: 'Configure Options',
            task: 'Configure advanced options',
            context: 'When you need custom configuration',
            steps: ['Open config file', 'Set options', 'Validate config'],
            confidenceScore: 0.85,
            source: ['docs/config.md']
          }
        ],
        reference: {
          id: 'api-reference',
          title: 'API Reference',
          items: [
            {
              name: 'createClient',
              type: 'export',
              description: 'Creates a new client instance',
              example: 'const client = createClient();'
            }
          ],
          confidenceScore: 0.95,
          source: ['src/index.mjs']
        },
        explanation: {
          id: 'architecture',
          title: 'Architecture',
          concepts: ['Event-driven design', 'Plugin system'],
          architecture: 'The system uses a modular architecture with plugins.',
          tradeoffs: ['Performance vs flexibility', 'Memory vs speed'],
          confidenceScore: 0.8,
          source: ['docs/architecture.md']
        },
        confidence: {
          tutorials: 0.9,
          howtos: 0.85,
          reference: 0.95,
          explanation: 0.8
        }
      });

      // Generate scaffold
      const result = await generateScaffold(entry, testOutputDir);

      // Verify result structure
      assert.ok(result);
      assert.equal(result.packageName, 'test-package');
      assert.ok(Array.isArray(result.filesGenerated));
      assert.ok(result.filesGenerated.length > 0);
      assert.ok(typeof result.indexPath === 'string');
      assert.ok(typeof result.filesHash === 'string');
      assert.equal(result.filesHash.length, 64); // SHA256 hex

      // Verify index.md exists
      assert.ok(existsSync(result.indexPath));
      const indexContent = await readFile(result.indexPath, 'utf8');
      assert.ok(indexContent.includes('test-package'));
      assert.ok(indexContent.includes('**Tutorials**: 1'));
      assert.ok(indexContent.includes('**How-To Guides**: 1'));

      // Verify tutorial file
      const tutorialPath = join(testOutputDir, 'tutorials', 'tutorial-getting-started.md');
      assert.ok(existsSync(tutorialPath));
      const tutorialContent = await readFile(tutorialPath, 'utf8');
      assert.ok(tutorialContent.includes('---'));
      assert.ok(tutorialContent.includes('title: "Getting Started"'));
      assert.ok(tutorialContent.includes('type: "tutorial"'));
      assert.ok(tutorialContent.includes('confidenceScore: 0.9'));
      assert.ok(tutorialContent.includes('proof:'));
      assert.ok(tutorialContent.includes('## Prerequisites'));
      assert.ok(tutorialContent.includes('Node.js 18+'));

      // Verify how-to file
      const howtoPath = join(testOutputDir, 'how-to', 'howto-configure-options.md');
      assert.ok(existsSync(howtoPath));
      const howtoContent = await readFile(howtoPath, 'utf8');
      assert.ok(howtoContent.includes('title: "Configure Options"'));
      assert.ok(howtoContent.includes('type: "how-to"'));
      assert.ok(howtoContent.includes('## Task'));
      assert.ok(howtoContent.includes('## Context'));

      // Verify reference file
      const referencePath = join(testOutputDir, 'reference', 'reference.md');
      assert.ok(existsSync(referencePath));
      const referenceContent = await readFile(referencePath, 'utf8');
      assert.ok(referenceContent.includes('API Reference'));
      assert.ok(referenceContent.includes('| Name | Type | Description |'));
      assert.ok(referenceContent.includes('createClient'));

      // Verify explanation file
      const explanationPath = join(testOutputDir, 'explanation', 'explanation.md');
      assert.ok(existsSync(explanationPath));
      const explanationContent = await readFile(explanationPath, 'utf8');
      assert.ok(explanationContent.includes('Architecture'));
      assert.ok(explanationContent.includes('Event-driven design'));
      assert.ok(explanationContent.includes('## Tradeoffs'));

      // Verify proof blocks exist
      assert.ok(tutorialContent.includes('## Proof'));
      assert.ok(tutorialContent.includes('```json'));
      assert.ok(howtoContent.includes('## Proof'));
      assert.ok(referenceContent.includes('## Proof'));
      assert.ok(explanationContent.includes('## Proof'));
    });

    it('should handle empty entry gracefully', async () => {
      const emptyOutputDir = join(__dirname, 'fixtures', 'scaffold-empty');
      if (existsSync(emptyOutputDir)) {
        await rm(emptyOutputDir, { recursive: true, force: true });
      }

      const entry = createDiataxisEntry('empty-package', '0.0.1', {});

      const result = await generateScaffold(entry, emptyOutputDir);

      assert.ok(result);
      assert.equal(result.packageName, 'empty-package');
      assert.ok(existsSync(result.indexPath));

      // Should still create reference and explanation files
      assert.ok(existsSync(join(emptyOutputDir, 'reference', 'reference.md')));
      assert.ok(existsSync(join(emptyOutputDir, 'explanation', 'explanation.md')));
    });

    it('should produce deterministic output', async () => {
      const deterministicDir1 = join(__dirname, 'fixtures', 'scaffold-det1');
      const deterministicDir2 = join(__dirname, 'fixtures', 'scaffold-det2');

      // Clean up
      for (const dir of [deterministicDir1, deterministicDir2]) {
        if (existsSync(dir)) {
          await rm(dir, { recursive: true, force: true });
        }
      }

      // Set deterministic mode
      process.env.DETERMINISTIC = '1';

      const entry = createDiataxisEntry('test-deterministic', '1.0.0', {
        tutorials: [
          {
            title: 'Test Tutorial',
            goal: 'Test',
            prerequisites: [],
            stepsOutline: ['Step 1'],
            confidenceScore: 0.9,
            source: ['test.md']
          }
        ]
      });

      const result1 = await generateScaffold(entry, deterministicDir1);
      const result2 = await generateScaffold(entry, deterministicDir2);

      // Hashes should be identical
      assert.equal(result1.filesHash, result2.filesHash);

      // Content should be identical
      const index1 = await readFile(result1.indexPath, 'utf8');
      const index2 = await readFile(result2.indexPath, 'utf8');
      assert.equal(index1, index2);

      // Clean up env
      delete process.env.DETERMINISTIC;
    });

    it('should escape markdown in table cells', async () => {
      const escapeTestDir = join(__dirname, 'fixtures', 'scaffold-escape');
      if (existsSync(escapeTestDir)) {
        await rm(escapeTestDir, { recursive: true, force: true });
      }

      const entry = createDiataxisEntry('escape-test', '1.0.0', {
        reference: {
          title: 'Reference',
          items: [
            {
              name: 'test|pipe',
              type: 'export',
              description: 'Description with | pipe',
              example: null
            }
          ],
          confidenceScore: 0.9,
          source: ['test.md']
        }
      });

      const result = await generateScaffold(entry, escapeTestDir);
      const referencePath = join(escapeTestDir, 'reference', 'reference.md');
      const content = await readFile(referencePath, 'utf8');

      // Pipes should be escaped
      assert.ok(content.includes('test\\|pipe'));
      assert.ok(content.includes('Description with \\| pipe'));
    });
  });
});
