/**
 * @vitest-environment node
 * @file Project Initialization Pipeline with Code Complexity (Chicago School TDD)
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { mkdtemp, writeFile, rm, mkdir } from 'fs/promises';
import { join } from 'path';
import { tmpdir } from 'os';
import { createProjectInitializationPipeline } from '../../src/project-engine/initialize.mjs';

describe('createProjectInitializationPipeline with code complexity', () => {
  let tempDir;

  beforeAll(async () => {
    tempDir = await mkdtemp(join(tmpdir(), 'init-complexity-'));
  });

  afterAll(async () => {
    if (tempDir) {
      await rm(tempDir, { recursive: true, force: true });
    }
  });

  describe('Phase 6.5: Code Complexity Analysis', () => {
    it('should include code complexity phase in receipt', async () => {
      // Create minimal project structure
      const srcDir = join(tempDir, 'src');
      await mkdir(srcDir, { recursive: true });
      await writeFile(join(srcDir, 'index.js'), 'function hello() { return "world"; }');

      const result = await createProjectInitializationPipeline(tempDir, {
        skipPhases: ['templateInference', 'hooks', 'snapshot'],
      });

      expect(result.success).toBe(true);
      expect(result.receipt.phases.codeComplexity).toBeDefined();
      expect(result.receipt.phases.codeComplexity.success).toBe(true);
    });

    it('should capture code complexity summary in receipt metrics', async () => {
      // Create minimal project structure
      const srcDir = join(tempDir, 'src');
      await mkdir(srcDir, { recursive: true });
      await writeFile(join(srcDir, 'index.js'), 'function hello() { return "world"; }');

      const result = await createProjectInitializationPipeline(tempDir, {
        skipPhases: ['templateInference', 'hooks', 'snapshot'],
      });

      expect(result.receipt.metrics).toBeDefined();
      expect(result.receipt.metrics.filesAnalyzed).toBeDefined();
      expect(result.receipt.metrics.mode).toBe('observe');
    });

    it('should skip code complexity phase when requested', async () => {
      const result = await createProjectInitializationPipeline(tempDir, {
        skipPhases: ['codeComplexity', 'templateInference', 'hooks', 'snapshot'],
      });

      expect(result.success).toBe(true);
      expect(result.receipt.phases.codeComplexity).toBeUndefined();
      expect(result.receipt.metrics).toBeUndefined();
    });

    it('should include complexity store in initialization state', async () => {
      // Create minimal project structure
      const srcDir = join(tempDir, 'src');
      await mkdir(srcDir, { recursive: true });
      await writeFile(join(srcDir, 'index.js'), 'function test() { if (true) return 1; }');

      const result = await createProjectInitializationPipeline(tempDir, {
        skipPhases: ['templateInference', 'hooks', 'snapshot'],
      });

      expect(result.state.complexityStore).toBeDefined();
    });

    it('should handle code complexity phase errors gracefully', async () => {
      const result = await createProjectInitializationPipeline('/nonexistent/path', {
        skipPhases: ['templateInference', 'hooks', 'snapshot'],
      });

      expect(result.success).toBe(false);
      expect(result.receipt.failedPhase).toBe('codeComplexity');
      expect(result.receipt.error).toBeDefined();
    });

    it('should execute all phases including complexity when not skipped', async () => {
      // Create minimal project structure
      const srcDir = join(tempDir, 'src');
      await mkdir(srcDir, { recursive: true });
      await writeFile(join(srcDir, 'index.js'), 'export default function() { return 42; }');
      await writeFile(join(srcDir, 'helper.js'), 'export const add = (a, b) => a + b;');

      const result = await createProjectInitializationPipeline(tempDir, {
        skipPhases: ['templateInference', 'hooks', 'snapshot'],
      });

      // Verify all expected phases completed
      expect(result.receipt.phases.scan).toBeDefined();
      expect(result.receipt.phases.stackDetection).toBeDefined();
      expect(result.receipt.phases.projectModel).toBeDefined();
      expect(result.receipt.phases.fileRoles).toBeDefined();
      expect(result.receipt.phases.domainInference).toBeDefined();
      expect(result.receipt.phases.codeComplexity).toBeDefined();

      // Verify complexity metrics captured
      expect(result.receipt.metrics).toBeDefined();
      expect(result.receipt.metrics.filesAnalyzed).toBeGreaterThan(0);
    });
  });
});
