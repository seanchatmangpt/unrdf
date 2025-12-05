/**
 * @file Project initialization pipeline tests
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import path from 'path';
import { createProjectInitializationPipeline } from '../../packages/project-engine/initialize.mjs';

describe('initialize', () => {
  describe('createProjectInitializationPipeline', () => {
    it('should initialize with valid project root', async () => {
      const projectRoot = path.resolve(process.cwd());

      const result = await createProjectInitializationPipeline(projectRoot, {
        ignorePatterns: ['node_modules', '.git', 'dist', 'coverage'],
      });

      expect(result.success).toBe(true);
      expect(result.receipt).toBeDefined();
      expect(result.receipt.phases).toBeDefined();
      expect(result.receipt.phases.scan).toBeDefined();
      expect(result.receipt.phases.scan.success).toBe(true);
      expect(result.receipt.totalDuration).toBeGreaterThan(0);
    });

    it('should return receipt with all phase data', async () => {
      const projectRoot = path.resolve(process.cwd());

      const result = await createProjectInitializationPipeline(projectRoot, {
        ignorePatterns: ['node_modules', '.git', 'dist', 'coverage', '.next', '.turbo'],
      });

      expect(result.success).toBe(true);

      // Check all phases
      const { phases } = result.receipt;

      expect(phases.scan.data.files).toBeGreaterThan(0);
      expect(phases.scan.data.folders).toBeGreaterThan(0);

      expect(phases.stackDetection).toBeDefined();
      expect(phases.stackDetection.data.profile).toBeDefined();

      expect(phases.projectModel).toBeDefined();
      expect(phases.projectModel.data.store).toBeDefined();

      expect(phases.fileRoles).toBeDefined();
      expect(phases.fileRoles.data.classified).toBeGreaterThanOrEqual(0);

      expect(phases.domainInference).toBeDefined();
      expect(phases.domainInference.data.store).toBeDefined();

      expect(phases.templateInference).toBeDefined();
      expect(phases.templateInference.data.templateGraph).toBeDefined();

      expect(phases.snapshot).toBeDefined();
      expect(phases.snapshot.data.hash).toBeDefined();

      expect(phases.hooks).toBeDefined();
      expect(phases.hooks.data.hooks).toBeInstanceOf(Array);

      expect(phases.report).toBeDefined();
      expect(phases.report.data.report).toBeDefined();
    });

    it('should return state with all stores', async () => {
      const projectRoot = path.resolve(process.cwd());

      const result = await createProjectInitializationPipeline(projectRoot, {
        ignorePatterns: ['node_modules', '.git', 'dist', 'coverage'],
      });

      expect(result.success).toBe(true);
      expect(result.state).toBeDefined();
      expect(result.state.fsStore).toBeDefined();
      expect(result.state.projectStore).toBeDefined();
      expect(result.state.domainStore).toBeDefined();
      expect(result.state.templateGraph).toBeDefined();
      expect(result.state.snapshot).toBeDefined();
    });

    it('should return human-readable report', async () => {
      const projectRoot = path.resolve(process.cwd());

      const result = await createProjectInitializationPipeline(projectRoot, {
        ignorePatterns: ['node_modules', '.git', 'dist', 'coverage'],
      });

      expect(result.success).toBe(true);
      expect(result.report).toBeDefined();
      expect(result.report.summary).toBeDefined();
      expect(typeof result.report.summary).toBe('string');
      expect(result.report.features).toBeInstanceOf(Array);
      expect(result.report.entities).toBeInstanceOf(Array);
    });

    it('should skip phases when specified', async () => {
      const projectRoot = path.resolve(process.cwd());

      const result = await createProjectInitializationPipeline(projectRoot, {
        ignorePatterns: ['node_modules', '.git', 'dist', 'coverage'],
        skipPhases: ['domainInference', 'templateInference', 'hooks'],
      });

      expect(result.success).toBe(true);
      expect(result.receipt.phases.scan).toBeDefined();
      expect(result.receipt.phases.domainInference).toBeUndefined();
      expect(result.receipt.phases.templateInference).toBeUndefined();
      expect(result.receipt.phases.hooks).toBeUndefined();
    });

    it('should be idempotent', async () => {
      const projectRoot = path.resolve(process.cwd());
      const options = {
        ignorePatterns: ['node_modules', '.git', 'dist', 'coverage'],
      };

      const result1 = await createProjectInitializationPipeline(projectRoot, options);
      const result2 = await createProjectInitializationPipeline(projectRoot, options);

      expect(result1.success).toBe(true);
      expect(result2.success).toBe(true);
      expect(result1.receipt.phases.scan.data.files).toBe(result2.receipt.phases.scan.data.files);
    });

    it('should handle nonexistent path gracefully', async () => {
      const projectRoot = '/nonexistent/path/to/project';

      const result = await createProjectInitializationPipeline(projectRoot, {});

      // The fs-scan handles missing directories gracefully by logging a warning
      // and returning an empty store. This is by design for robustness.
      expect(result.success).toBe(true);
      expect(result.receipt.phases.scan.data.files).toBe(0);
      expect(result.receipt.phases.scan.data.folders).toBe(0);
    });
  });
});
