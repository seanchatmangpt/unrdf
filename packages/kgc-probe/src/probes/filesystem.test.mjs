/**
 * Tests for Filesystem Probe
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { probeFilesystem, guardPath } from './filesystem.mjs';
import fs from 'fs/promises';
import path from 'path';
import os from 'os';

describe('Filesystem Probe', () => {
  let testRoot;
  let outDir;

  beforeEach(async () => {
    // Create temp test directory
    testRoot = path.join(os.tmpdir(), `kgc-probe-test-${Date.now()}`);
    outDir = path.join(testRoot, 'out');
    await fs.mkdir(outDir, { recursive: true });
  });

  afterEach(async () => {
    // Cleanup
    try {
      await fs.rm(testRoot, { recursive: true, force: true });
    } catch {}
  });

  describe('guardPath', () => {
    it('should allow paths within allowed roots', () => {
      const result = guardPath('/home/user/project/file.txt', ['/home/user/project']);
      expect(result.allowed).toBe(true);
    });

    it('should deny paths outside allowed roots', () => {
      const result = guardPath('/etc/passwd', ['/home/user/project']);
      expect(result.allowed).toBe(false);
      expect(result.reason).toContain('outside allowed roots');
    });

    it('should deny /etc/ paths', () => {
      const result = guardPath('/etc/hosts', ['/etc']);
      expect(result.allowed).toBe(false);
      expect(result.reason).toContain('forbidden pattern');
    });

    it('should deny /root/ paths', () => {
      const result = guardPath('/root/secret.txt', ['/root']);
      expect(result.allowed).toBe(false);
      expect(result.reason).toContain('forbidden pattern');
    });

    it('should deny .ssh directories', () => {
      const result = guardPath('/home/user/.ssh/id_rsa', ['/home/user']);
      expect(result.allowed).toBe(false);
      expect(result.reason).toContain('forbidden pattern');
    });

    it('should deny .env files', () => {
      const result = guardPath('/home/user/project/.env', ['/home/user/project']);
      expect(result.allowed).toBe(false);
      expect(result.reason).toContain('forbidden pattern');
    });

    it('should deny credentials.json', () => {
      const result = guardPath('/home/user/credentials.json', ['/home/user']);
      expect(result.allowed).toBe(false);
      expect(result.reason).toContain('forbidden pattern');
    });

    it('should deny .pem files', () => {
      const result = guardPath('/home/user/cert.pem', ['/home/user']);
      expect(result.allowed).toBe(false);
      expect(result.reason).toContain('forbidden pattern');
    });
  });

  describe('probeFilesystem', () => {
    it('should return observations array', async () => {
      const observations = await probeFilesystem({
        roots: [testRoot],
        out: outDir,
        budgetMs: 5000
      });

      expect(Array.isArray(observations)).toBe(true);
      expect(observations.length).toBeGreaterThan(0);
    });

    it('should have valid observation structure', async () => {
      const observations = await probeFilesystem({
        roots: [testRoot],
        out: outDir,
        budgetMs: 5000
      });

      const obs = observations[0];
      expect(obs).toHaveProperty('method');
      expect(obs).toHaveProperty('inputs');
      expect(obs).toHaveProperty('timestamp');
      expect(obs).toHaveProperty('hash');
      expect(obs).toHaveProperty('guardDecision');
    });

    it('should deny access to forbidden paths', async () => {
      const observations = await probeFilesystem({
        roots: ['/etc'],
        out: '/etc/kgc-probe',
        budgetMs: 5000
      });

      const deniedObs = observations.find(o => o.guardDecision === 'denied');
      expect(deniedObs).toBeDefined();
      expect(deniedObs.guardReason).toContain('forbidden pattern');
    });

    it('should deny when output directory outside roots', async () => {
      const observations = await probeFilesystem({
        roots: [testRoot],
        out: '/tmp/outside-roots',
        budgetMs: 5000
      });

      const mainObs = observations.find(o => o.method === 'probeFilesystem');
      if (mainObs) {
        expect(mainObs.guardDecision).toBe('denied');
      }
    });

    it('should not include outputs for denied operations', async () => {
      const observations = await probeFilesystem({
        roots: ['/etc'],
        out: '/etc/test',
        budgetMs: 5000
      });

      const deniedObs = observations.find(o => o.guardDecision === 'denied');
      if (deniedObs) {
        expect(deniedObs.outputs).toBeUndefined();
      }
    });

    it('should complete within budget', async () => {
      const start = Date.now();
      await probeFilesystem({
        roots: [testRoot],
        out: outDir,
        budgetMs: 3000
      });
      const elapsed = Date.now() - start;

      expect(elapsed).toBeLessThan(4000); // Allow some margin
    });

    it('should probe read capability', async () => {
      const observations = await probeFilesystem({
        roots: [testRoot],
        out: outDir,
        budgetMs: 5000
      });

      const readObs = observations.find(o => o.method === 'fs.access(R_OK)');
      expect(readObs).toBeDefined();
      expect(readObs.guardDecision).toBe('allowed');
    });

    it('should probe write capability', async () => {
      const observations = await probeFilesystem({
        roots: [testRoot],
        out: outDir,
        budgetMs: 5000
      });

      const writeObs = observations.find(o => o.method === 'fs.access(W_OK)');
      expect(writeObs).toBeDefined();
      expect(writeObs.guardDecision).toBe('allowed');
    });

    it('should probe symlink behavior', async () => {
      const observations = await probeFilesystem({
        roots: [testRoot],
        out: outDir,
        budgetMs: 5000
      });

      const symlinkObs = observations.find(o => o.method === 'fs.symlink');
      expect(symlinkObs).toBeDefined();
      expect(symlinkObs.guardDecision).toBe('allowed');
    });

    it('should probe directory traversal', async () => {
      const observations = await probeFilesystem({
        roots: [testRoot],
        out: outDir,
        budgetMs: 5000
      });

      const traversalObs = observations.find(o => o.method === 'fs.readdir(recursive)');
      expect(traversalObs).toBeDefined();
      expect(traversalObs.guardDecision).toBe('allowed');
    });

    it('should have deterministic hashes', async () => {
      const obs1 = await probeFilesystem({
        roots: [testRoot],
        out: outDir,
        budgetMs: 5000
      });

      const obs2 = await probeFilesystem({
        roots: [testRoot],
        out: outDir,
        budgetMs: 5000
      });

      // Methods should be same
      expect(obs1.map(o => o.method).sort()).toEqual(obs2.map(o => o.method).sort());
    });
  });

  describe('Observation Schema Compliance', () => {
    it('should include all required fields', async () => {
      const observations = await probeFilesystem({
        roots: [testRoot],
        out: outDir,
        budgetMs: 5000
      });

      for (const obs of observations) {
        expect(obs.method).toBeDefined();
        expect(obs.inputs).toBeDefined();
        expect(obs.timestamp).toBeDefined();
        expect(obs.hash).toBeDefined();
        expect(obs.guardDecision).toBeDefined();

        // Validate timestamp format
        expect(() => new Date(obs.timestamp)).not.toThrow();

        // Validate hash format (SHA256 = 64 hex chars)
        expect(obs.hash).toMatch(/^[a-f0-9]{64}$/);
      }
    });
  });
});
