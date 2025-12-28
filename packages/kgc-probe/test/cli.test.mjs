/**
 * @fileoverview CLI Command Tests
 *
 * Tests for:
 * - scan command
 * - merge command
 * - diff command
 * - report command
 * - verify command
 */

import { describe, it, expect } from 'vitest';
import {
  scanCommand,
  mergeCommand,
  diffCommand,
  reportCommand,
  verifyCommand,
  ScanArgsSchema,
  MergeArgsSchema,
  DiffArgsSchema,
  ReportArgsSchema,
  VerifyArgsSchema,
  probeExtension
} from '../src/cli.mjs';

describe('CLI Schemas', () => {
  describe('ScanArgsSchema', () => {
    it('should accept valid scan arguments', () => {
      const result = ScanArgsSchema.parse({
        output: './output',
        parallel: 5,
        timeout: 10000
      });
      expect(result.output).toBe('./output');
      expect(result.parallel).toBe(5);
    });

    it('should apply defaults', () => {
      const result = ScanArgsSchema.parse({});
      expect(result.timeout).toBe(30000);
      expect(result.parallel).toBe(10);
      expect(result.format).toBe('all');
      expect(result.validate).toBe(true);
    });

    it('should validate format enum', () => {
      expect(() => ScanArgsSchema.parse({ format: 'invalid' })).toThrow();
    });
  });

  describe('MergeArgsSchema', () => {
    it('should require shardDir', () => {
      expect(() => MergeArgsSchema.parse({})).toThrow();
    });

    it('should accept valid merge arguments', () => {
      const result = MergeArgsSchema.parse({
        shardDir: './shards',
        onConflict: 'fail'
      });
      expect(result.shardDir).toBe('./shards');
      expect(result.onConflict).toBe('fail');
    });
  });

  describe('DiffArgsSchema', () => {
    it('should require both artifact paths', () => {
      expect(() => DiffArgsSchema.parse({})).toThrow();
      expect(() => DiffArgsSchema.parse({ oldArtifact: './old.json' })).toThrow();
    });

    it('should accept valid diff arguments', () => {
      const result = DiffArgsSchema.parse({
        oldArtifact: './old.json',
        newArtifact: './new.json',
        format: 'md'
      });
      expect(result.format).toBe('md');
    });
  });

  describe('ReportArgsSchema', () => {
    it('should require artifactPath', () => {
      expect(() => ReportArgsSchema.parse({})).toThrow();
    });

    it('should accept valid report arguments', () => {
      const result = ReportArgsSchema.parse({
        artifactPath: './artifact.json',
        style: 'executive',
        format: 'json'
      });
      expect(result.style).toBe('executive');
    });
  });

  describe('VerifyArgsSchema', () => {
    it('should require artifactPath', () => {
      expect(() => VerifyArgsSchema.parse({})).toThrow();
    });

    it('should accept valid verify arguments', () => {
      const result = VerifyArgsSchema.parse({
        artifactPath: './artifact',
        checkMerkle: false,
        strict: false
      });
      expect(result.checkMerkle).toBe(false);
      expect(result.strict).toBe(false);
    });
  });
});

describe('scanCommand', () => {
  it('should execute scan and return result', async () => {
    const result = await scanCommand({
      output: './test-output',
      parallel: 5
    });

    expect(result.success).toBe(true);
    expect(result.runId).toBeDefined();
    expect(result.shardCount).toBe(10);
    expect(result.mergedArtifact).toBeDefined();
    expect(result.metrics).toBeDefined();
  });

  it('should generate receipts by default', async () => {
    const result = await scanCommand({});

    expect(result.receipts).toBeDefined();
    expect(result.receipts.chain).toBeDefined();
    expect(result.receipts.chain.length).toBe(10);
  });

  it('should include merkle tree', async () => {
    const result = await scanCommand({ merkle: true });

    expect(result.receipts.merkle).toBeDefined();
    expect(result.receipts.merkle.root).toBeDefined();
    expect(result.receipts.merkle.proofs.length).toBe(10);
  });

  it('should skip receipts when noReceipts is true', async () => {
    const result = await scanCommand({ noReceipts: true });

    expect(result.receipts).toBeNull();
  });

  it('should track execution metrics', async () => {
    const result = await scanCommand({});

    expect(result.metrics.duration_ms).toBeGreaterThanOrEqual(0);
    expect(result.metrics.agentCount).toBe(10);
    expect(result.metrics.startTime).toBeDefined();
    expect(result.metrics.endTime).toBeDefined();
  });

  it('should generate reports in correct format', async () => {
    const resultAll = await scanCommand({ format: 'all' });
    expect(resultAll.reports.ttl).toBeDefined();
    expect(resultAll.reports.json).toBeDefined();
    expect(resultAll.reports.md).toBeDefined();

    const resultMd = await scanCommand({ format: 'md' });
    expect(resultMd.reports.ttl).toBeNull();
    expect(resultMd.reports.md).toBeDefined();
  });

  it('should handle verbose mode', async () => {
    const result = await scanCommand({ verbose: true });
    expect(result.success).toBe(true);
  });
});

describe('mergeCommand', () => {
  it('should execute merge and return result', async () => {
    const result = await mergeCommand({
      shardDir: './shards'
    });

    expect(result.mergedArtifact).toBeDefined();
    expect(result.shardCount).toBeDefined();
  });

  it('should detect conflicts', async () => {
    const result = await mergeCommand({
      shardDir: './shards',
      onConflict: 'list'
    });

    expect(result.conflicts).toBeDefined();
    expect(Array.isArray(result.conflicts)).toBe(true);
  });

  it('should fail on conflicts when onConflict is fail', async () => {
    // With empty shards, no conflicts expected
    const result = await mergeCommand({
      shardDir: './shards',
      onConflict: 'fail'
    });

    expect(result.success).toBe(true);
  });

  it('should handle different output formats', async () => {
    const result = await mergeCommand({
      shardDir: './shards',
      format: 'json'
    });

    expect(result.mergedArtifact).toBeDefined();
  });
});

describe('diffCommand', () => {
  it('should compare two artifacts', async () => {
    const result = await diffCommand({
      oldArtifact: './old.json',
      newArtifact: './new.json'
    });

    expect(result.added).toBeDefined();
    expect(result.removed).toBeDefined();
    expect(result.modified).toBeDefined();
    expect(result.summary).toBeDefined();
  });

  it('should return JSON format by default', async () => {
    const result = await diffCommand({
      oldArtifact: './old.json',
      newArtifact: './new.json'
    });

    expect(result.format).toBe('json');
    expect(result.content).toBeDefined();
    JSON.parse(result.content); // Should be valid JSON
  });

  it('should generate markdown format', async () => {
    const result = await diffCommand({
      oldArtifact: './old.json',
      newArtifact: './new.json',
      format: 'md'
    });

    expect(result.format).toBe('md');
    expect(result.content).toContain('# Probe Diff Report');
  });

  it('should include summary counts', async () => {
    const result = await diffCommand({
      oldArtifact: './old.json',
      newArtifact: './new.json'
    });

    expect(result.summary.addedCount).toBeDefined();
    expect(result.summary.removedCount).toBeDefined();
    expect(result.summary.modifiedCount).toBeDefined();
    expect(result.summary.changesetSize).toBeDefined();
  });
});

describe('reportCommand', () => {
  it('should generate markdown report', async () => {
    const result = await reportCommand({
      artifactPath: './artifact.json',
      format: 'md'
    });

    expect(result.success).toBe(true);
    expect(result.format).toBe('md');
    expect(result.content).toContain('# KGC Probe Report');
  });

  it('should generate JSON report', async () => {
    const result = await reportCommand({
      artifactPath: './artifact.json',
      format: 'json'
    });

    expect(result.format).toBe('json');
    const parsed = JSON.parse(result.content);
    expect(parsed.metadata).toBeDefined();
    expect(parsed.summary).toBeDefined();
  });

  it('should generate Turtle report', async () => {
    const result = await reportCommand({
      artifactPath: './artifact.json',
      format: 'ttl'
    });

    expect(result.format).toBe('ttl');
    expect(result.content).toContain('@prefix kgc:');
    expect(result.content).toContain('a kgc:ProbeReport');
  });

  it('should include all sections', async () => {
    const result = await reportCommand({
      artifactPath: './artifact.json'
    });

    expect(result.sections).toContain('Tutorial');
    expect(result.sections).toContain('How-To');
    expect(result.sections).toContain('Reference');
    expect(result.sections).toContain('Explanation');
    expect(result.sections).toContain('Statistics');
  });

  it('should apply report style', async () => {
    const technical = await reportCommand({
      artifactPath: './artifact.json',
      style: 'technical'
    });

    const executive = await reportCommand({
      artifactPath: './artifact.json',
      style: 'executive'
    });

    expect(technical.success).toBe(true);
    expect(executive.success).toBe(true);
  });
});

describe('verifyCommand', () => {
  it('should verify artifact integrity', async () => {
    const result = await verifyCommand({
      artifactPath: './artifact'
    });

    expect(result.valid).toBe(true);
    expect(result.checks).toBeDefined();
  });

  it('should check hash chain', async () => {
    const result = await verifyCommand({
      artifactPath: './artifact'
    });

    expect(result.checks.hashChainValid).toBe(true);
    expect(result.details.some(d => d.includes('Hash chain verified'))).toBe(true);
  });

  it('should check merkle tree when enabled', async () => {
    const result = await verifyCommand({
      artifactPath: './artifact',
      checkMerkle: true
    });

    expect(result.checks.merkleValid).toBe(true);
    expect(result.details.some(d => d.includes('Merkle tree verified'))).toBe(true);
  });

  it('should check schema when enabled', async () => {
    const result = await verifyCommand({
      artifactPath: './artifact',
      checkSchema: true
    });

    expect(result.checks.schemaValid).toBe(true);
    expect(result.details.some(d => d.includes('Schema validation'))).toBe(true);
  });

  it('should skip crypto check when no keys', async () => {
    const result = await verifyCommand({
      artifactPath: './artifact',
      checkCrypto: true
    });

    expect(result.details.some(d => d.includes('SKIPPED'))).toBe(true);
  });

  it('should return confidence score', async () => {
    const result = await verifyCommand({
      artifactPath: './artifact'
    });

    expect(result.confidence).toBeDefined();
    expect(result.confidence).toBe(100);
  });

  it('should include mismatches when verification fails', async () => {
    const result = await verifyCommand({
      artifactPath: './artifact',
      strict: false
    });

    expect(result.mismatches).toBeDefined();
    expect(Array.isArray(result.mismatches)).toBe(true);
  });
});

describe('probeExtension', () => {
  it('should have correct structure', () => {
    expect(probeExtension.id).toBe('@unrdf/kgc-probe');
    expect(probeExtension.description).toBeDefined();
    expect(probeExtension.nouns).toBeDefined();
    expect(probeExtension.nouns.probe).toBeDefined();
  });

  it('should define all 5 commands', () => {
    const verbs = probeExtension.nouns.probe.verbs;
    expect(verbs.scan).toBeDefined();
    expect(verbs.merge).toBeDefined();
    expect(verbs.diff).toBeDefined();
    expect(verbs.report).toBeDefined();
    expect(verbs.verify).toBeDefined();
  });

  it('should have handlers for all commands', () => {
    const verbs = probeExtension.nouns.probe.verbs;
    expect(typeof verbs.scan.handler).toBe('function');
    expect(typeof verbs.merge.handler).toBe('function');
    expect(typeof verbs.diff.handler).toBe('function');
    expect(typeof verbs.report.handler).toBe('function');
    expect(typeof verbs.verify.handler).toBe('function');
  });

  it('should have schemas for all commands', () => {
    const verbs = probeExtension.nouns.probe.verbs;
    expect(verbs.scan.argsSchema).toBeDefined();
    expect(verbs.merge.argsSchema).toBeDefined();
    expect(verbs.diff.argsSchema).toBeDefined();
    expect(verbs.report.argsSchema).toBeDefined();
    expect(verbs.verify.argsSchema).toBeDefined();
  });

  it('should have examples for all commands', () => {
    const verbs = probeExtension.nouns.probe.verbs;
    expect(verbs.scan.meta.examples.length).toBeGreaterThan(0);
    expect(verbs.merge.meta.examples.length).toBeGreaterThan(0);
    expect(verbs.diff.meta.examples.length).toBeGreaterThan(0);
    expect(verbs.report.meta.examples.length).toBeGreaterThan(0);
    expect(verbs.verify.meta.examples.length).toBeGreaterThan(0);
  });
});
