/**
 * @file Persistence Probe Tests
 * @module @unrdf/kgc-probe/test/persistence
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { probePersistence } from '../src/probes/persistence.mjs';
import { promises as fs } from 'node:fs';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

describe('Persistence Probe', () => {
  let testDir;

  beforeAll(async () => {
    // Create a temporary test directory
    testDir = join(tmpdir(), `kgc-probe-test-${Date.now()}`);
    await fs.mkdir(testDir, { recursive: true });
  });

  afterAll(async () => {
    // Clean up test directory
    try {
      await fs.rm(testDir, { recursive: true, force: true });
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  it('should probe persistence and return observations', async () => {
    const config = {
      out: testDir,
      timeout: 5000,
      maxQuota: 10 * 1024 * 1024, // 10 MB for testing
      chunkSize: 1024 * 1024, // 1 MB
    };

    const observations = await probePersistence(config);

    expect(observations).toBeDefined();
    expect(Array.isArray(observations)).toBe(true);
    expect(observations.length).toBeGreaterThan(0);
  });

  it('should include guard decisions in observations', async () => {
    const config = {
      out: testDir,
      timeout: 5000,
    };

    const observations = await probePersistence(config);

    // At least some observations should have guard decisions
    const withGuardDecisions = observations.filter(obs => obs.guardDecision);
    expect(withGuardDecisions.length).toBeGreaterThan(0);

    // Check guard decision structure
    const guardDecision = withGuardDecisions[0].guardDecision;
    expect(guardDecision).toHaveProperty('path');
    expect(guardDecision).toHaveProperty('allowed');
    expect(guardDecision).toHaveProperty('reason');
    expect(guardDecision).toHaveProperty('policy');
  });

  it('should test basic write/read persistence', async () => {
    const config = {
      out: testDir,
      timeout: 5000,
    };

    const observations = await probePersistence(config);

    // Should have write and read observations
    const writeObs = observations.find(obs => obs.observation.includes('Write operation'));
    const readObs = observations.find(obs => obs.observation.includes('Read operation'));

    expect(writeObs).toBeDefined();
    expect(readObs).toBeDefined();
    expect(writeObs.value).toBe(true);
    expect(readObs.value).toBe(true);
  });

  it('should test directory permissions', async () => {
    const config = {
      out: testDir,
      timeout: 5000,
    };

    const observations = await probePersistence(config);

    // Should have directory creation observation
    const dirCreateObs = observations.find(obs => obs.observation.includes('Directory creation'));

    expect(dirCreateObs).toBeDefined();
    expect(dirCreateObs.value).toBe(true);
  });

  it('should detect storage type', async () => {
    const config = {
      out: testDir,
      timeout: 5000,
    };

    const observations = await probePersistence(config);

    // Should have storage type observation
    const storageTypeObs = observations.find(obs => obs.observation.includes('Storage type detection'));

    expect(storageTypeObs).toBeDefined();
    expect(storageTypeObs.value).toBeDefined();
    expect(typeof storageTypeObs.value).toBe('string');
  });

  it('should validate observations schema', async () => {
    const config = {
      out: testDir,
      timeout: 5000,
    };

    const observations = await probePersistence(config);

    // All observations should have required fields
    observations.forEach(obs => {
      expect(obs).toHaveProperty('probeName');
      expect(obs).toHaveProperty('timestamp');
      expect(obs).toHaveProperty('category');
      expect(obs).toHaveProperty('observation');
      expect(obs.probeName).toBe('persistence');
      expect(typeof obs.timestamp).toBe('number');
    });
  });

  it('should respect guard constraints', async () => {
    // Try to use a path outside the allowed directory
    const outsideDir = '/tmp/outside';
    const config = {
      out: testDir,
      timeout: 5000,
    };

    const observations = await probePersistence(config);

    // All guard decisions should be for paths within testDir
    const guardDecisions = observations
      .filter(obs => obs.guardDecision)
      .map(obs => obs.guardDecision);

    guardDecisions.forEach(decision => {
      if (decision.allowed) {
        expect(decision.path).toContain(testDir);
      }
    });
  });

  it('should complete quota test within timeout', async () => {
    const config = {
      out: testDir,
      timeout: 5000,
      maxQuota: 5 * 1024 * 1024, // 5 MB
    };

    const startTime = Date.now();
    const observations = await probePersistence(config);
    const duration = Date.now() - startTime;

    // Should complete within reasonable time (timeout * 2 for buffer)
    expect(duration).toBeLessThan(config.timeout * 2);

    // Should have quota observation
    const quotaObs = observations.find(obs => obs.category === 'quota');
    expect(quotaObs).toBeDefined();
  });
});
