/**
 * KGC Probe Reporter Tests
 * Validates RDF generation, claim derivation, and report formatting
 */

import { describe, it, expect } from 'vitest';
import {
  observationsToRdf,
  generateReport,
  deriveClaims,
  ObservationSchema,
} from '../src/reporter.mjs';

describe('KGC Probe Reporter', () => {
  // Sample observations for testing
  const sampleObservations = [
    {
      method: 'probeRuntime',
      domain: 'runtime',
      timestamp: 1703721600000, // 2023-12-28T00:00:00.000Z
      outputs: {
        node: 'v18.19.0',
        v8: '11.3.244',
        worker_threads: true,
      },
    },
    {
      method: 'probeWasm',
      domain: 'wasm',
      timestamp: 1703721601000,
      outputs: {
        available: true,
        maxMemory: '2GB',
        compileTime: 2.5,
      },
    },
    {
      method: 'probeFilesystem',
      domain: 'filesystem',
      timestamp: 1703721602000,
      outputs: {
        allowedPaths: ['/tmp', '/workspace'],
      },
    },
    {
      method: 'probeFilesystemWrite',
      domain: 'filesystem',
      timestamp: 1703721603000,
      guardDecision: 'denied',
      error: 'Write permission denied',
    },
    {
      method: 'probeNetwork',
      domain: 'network',
      timestamp: 1703721604000,
      outputs: {
        allowedUrls: ['https://api.example.com', 'https://data.example.org'],
      },
    },
    {
      method: 'probeBenchmark',
      domain: 'performance',
      timestamp: 1703721605000,
      outputs: {
        executionTime: 15.3,
        iterations: 1000,
      },
    },
  ];

  describe('ObservationSchema', () => {
    it('should validate correct observations', () => {
      const result = ObservationSchema.parse(sampleObservations[0]);
      expect(result.method).toBe('probeRuntime');
      expect(result.domain).toBe('runtime');
      expect(result.outputs).toEqual({
        node: 'v18.19.0',
        v8: '11.3.244',
        worker_threads: true,
      });
    });

    it('should accept minimal observation', () => {
      const minimal = { method: 'test', outputs: {} };
      const result = ObservationSchema.parse(minimal);
      expect(result.method).toBe('test');
    });

    it('should reject invalid observation', () => {
      expect(() => ObservationSchema.parse({ outputs: {} })).toThrow();
      expect(() => ObservationSchema.parse({ method: 123 })).toThrow();
    });
  });

  describe('observationsToRdf', () => {
    it('should convert observations to valid Turtle RDF', () => {
      const turtle = observationsToRdf(sampleObservations);

      // Check that output is a string
      expect(typeof turtle).toBe('string');

      // Check for RDF prefix declarations
      expect(turtle).toContain('@prefix');

      // Check for kgc:Observation type
      expect(turtle).toContain('kgc:Observation');

      // Check for method predicates
      expect(turtle).toContain('kgc:method');

      // Check for timestamp predicates
      expect(turtle).toContain('kgc:timestamp');
    });

    it('should include all observation methods', () => {
      const turtle = observationsToRdf(sampleObservations);

      expect(turtle).toContain('probeRuntime');
      expect(turtle).toContain('probeWasm');
      expect(turtle).toContain('probeFilesystem');
      expect(turtle).toContain('probeNetwork');
    });

    it('should include guard decisions', () => {
      const turtle = observationsToRdf(sampleObservations);
      expect(turtle).toContain('denied');
    });

    it('should handle empty observations array', () => {
      const turtle = observationsToRdf([]);
      expect(typeof turtle).toBe('string');
      expect(turtle).toContain('@prefix'); // Should still have prefixes
    });

    it('should sort observations deterministically', () => {
      const shuffled = [...sampleObservations].reverse();
      const turtle1 = observationsToRdf(sampleObservations);
      const turtle2 = observationsToRdf(shuffled);

      // Should produce same output regardless of input order
      expect(turtle1).toBe(turtle2);
    });
  });

  describe('deriveClaims', () => {
    it('should derive capabilities from observations', () => {
      const { capabilities } = deriveClaims(sampleObservations);

      expect(capabilities.length).toBeGreaterThan(0);

      // Should find Node.js runtime capability
      const runtimeCap = capabilities.find(c =>
        c.title.includes('Node.js Runtime')
      );
      expect(runtimeCap).toBeDefined();
      expect(runtimeCap.type).toBe('capability');
      expect(runtimeCap.description).toContain('v18.19.0');
      expect(runtimeCap.evidence).toContain('probeRuntime');
    });

    it('should derive constraints from observations', () => {
      const { constraints } = deriveClaims(sampleObservations);

      expect(constraints.length).toBeGreaterThan(0);

      // Should find filesystem constraint
      const fsCons = constraints.find(c =>
        c.title.includes('Filesystem')
      );
      expect(fsCons).toBeDefined();
      expect(fsCons.type).toBe('constraint');
      expect(fsCons.evidence.length).toBeGreaterThan(0);
    });

    it('should detect worker_threads capability', () => {
      const { capabilities } = deriveClaims(sampleObservations);

      const workerCap = capabilities.find(c =>
        c.title.includes('Worker Threads')
      );
      expect(workerCap).toBeDefined();
      expect(workerCap.description).toContain('worker_threads');
    });

    it('should detect WASM capability', () => {
      const { capabilities } = deriveClaims(sampleObservations);

      const wasmCap = capabilities.find(c =>
        c.title.includes('WebAssembly')
      );
      expect(wasmCap).toBeDefined();
      expect(wasmCap.description).toContain('WASM');
    });

    it('should detect network constraints', () => {
      const { constraints } = deriveClaims(sampleObservations);

      const netCons = constraints.find(c =>
        c.title.includes('Network')
      );
      expect(netCons).toBeDefined();
      expect(netCons.description).toContain('allowlist');
    });

    it('should detect denied operations as constraints', () => {
      const { constraints } = deriveClaims(sampleObservations);

      const deniedCons = constraints.find(c =>
        c.title.includes('Denied')
      );
      expect(deniedCons).toBeDefined();
    });

    it('should handle empty observations', () => {
      const { capabilities, constraints } = deriveClaims([]);
      expect(capabilities).toEqual([]);
      expect(constraints).toEqual([]);
    });

    it('should validate capability schema', () => {
      const { capabilities } = deriveClaims(sampleObservations);

      for (const cap of capabilities) {
        expect(cap.type).toBe('capability');
        expect(typeof cap.title).toBe('string');
        expect(typeof cap.description).toBe('string');
        expect(Array.isArray(cap.evidence)).toBe(true);
      }
    });

    it('should validate constraint schema', () => {
      const { constraints } = deriveClaims(sampleObservations);

      for (const cons of constraints) {
        expect(cons.type).toBe('constraint');
        expect(typeof cons.title).toBe('string');
        expect(typeof cons.description).toBe('string');
        expect(Array.isArray(cons.evidence)).toBe(true);
      }
    });
  });

  describe('generateReport', () => {
    it('should generate valid Markdown report', () => {
      const report = generateReport(sampleObservations);

      // Check report structure
      expect(report).toContain('# KGC Probe Report');
      expect(report).toContain('## Summary');
      expect(report).toContain('## Capabilities');
      expect(report).toContain('## Constraints');
      expect(report).toContain('## Observations by Domain');
    });

    it('should include summary statistics', () => {
      const report = generateReport(sampleObservations);

      expect(report).toContain('Total observations');
      expect(report).toContain('Capabilities discovered');
      expect(report).toContain('Constraints detected');
      expect(report).toContain('Execution time');
    });

    it('should list all capabilities', () => {
      const report = generateReport(sampleObservations);
      const { capabilities } = deriveClaims(sampleObservations);

      for (const cap of capabilities) {
        expect(report).toContain(cap.title);
        expect(report).toContain(cap.description);
      }
    });

    it('should list all constraints', () => {
      const report = generateReport(sampleObservations);
      const { constraints } = deriveClaims(sampleObservations);

      for (const cons of constraints) {
        expect(report).toContain(cons.title);
        expect(report).toContain(cons.description);
      }
    });

    it('should group observations by domain', () => {
      const report = generateReport(sampleObservations);

      expect(report).toContain('### Runtime');
      expect(report).toContain('### Wasm');
      expect(report).toContain('### Filesystem');
      expect(report).toContain('### Network');
      expect(report).toContain('### Performance');
    });

    it('should include observation details', () => {
      const report = generateReport(sampleObservations);

      // Check method names
      expect(report).toContain('probeRuntime');
      expect(report).toContain('probeWasm');

      // Check outputs
      expect(report).toContain('v18.19.0');
      expect(report).toContain('worker_threads');

      // Check guard decisions
      expect(report).toContain('denied');

      // Check errors
      expect(report).toContain('Write permission denied');
    });

    it('should include timestamps', () => {
      const report = generateReport(sampleObservations);
      expect(report).toContain('2023-12-28');
    });

    it('should include observation hashes', () => {
      const report = generateReport(sampleObservations);
      expect(report).toContain('Hash:');
    });

    it('should handle empty observations gracefully', () => {
      const report = generateReport([]);

      expect(report).toContain('# KGC Probe Report');
      expect(report).toContain('Total observations**: 0');
      expect(report).toContain('No capabilities detected');
      expect(report).toContain('No constraints detected');
    });

    it('should include report generation timestamp', () => {
      const report = generateReport(sampleObservations);
      expect(report).toMatch(/Report generated at \d{4}-\d{2}-\d{2}/);
    });
  });

  describe('Integration tests', () => {
    it('should handle full workflow: observations -> RDF + report', () => {
      // Generate RDF
      const turtle = observationsToRdf(sampleObservations);
      expect(turtle.length).toBeGreaterThan(100);

      // Derive claims
      const { capabilities, constraints } = deriveClaims(sampleObservations);
      expect(capabilities.length).toBeGreaterThan(0);
      expect(constraints.length).toBeGreaterThan(0);

      // Generate report
      const report = generateReport(sampleObservations);
      expect(report.length).toBeGreaterThan(500);
    });

    it('should maintain consistent hashing', () => {
      const obs1 = {
        method: 'testMethod',
        outputs: { value: 42 },
        timestamp: 1000,
      };

      const obs2 = {
        method: 'testMethod',
        outputs: { value: 42 },
        timestamp: 1000,
      };

      const turtle1 = observationsToRdf([obs1]);
      const turtle2 = observationsToRdf([obs2]);

      expect(turtle1).toBe(turtle2);
    });

    it('should handle mixed domains', () => {
      const mixedObs = [
        {
          method: 'test1',
          domain: 'runtime',
          outputs: { a: 1 },
        },
        {
          method: 'test2',
          domain: 'network',
          outputs: { b: 2 },
        },
        {
          method: 'test3',
          outputs: { c: 3 }, // No domain
        },
      ];

      const report = generateReport(mixedObs);
      expect(report).toContain('Runtime');
      expect(report).toContain('Network');
      expect(report).toContain('General');
    });
  });
});
