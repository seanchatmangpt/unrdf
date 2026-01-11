/**
 * @file Circuit Compiler Tests
 * @module @unrdf/zkp/test/circuit-compiler
 * @description Tests for SPARQL to R1CS circuit compilation
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  CircuitCompiler,
  createCircuitCompiler,
  compileSPARQL,
} from '../src/circuit-compiler.mjs';

describe('CircuitCompiler', () => {
  let compiler;

  beforeEach(() => {
    compiler = new CircuitCompiler({ optimize: true });
  });

  describe('Basic Compilation', () => {
    it('should compile simple SELECT query', async () => {
      const query = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s rdf:type :Person }
      `;

      const circuit = await compiler.compile(query);

      expect(circuit).toBeDefined();
      expect(circuit.constraints).toBeDefined();
      expect(circuit.publicInputs).toBeDefined();
      expect(circuit.privateInputs).toBeDefined();
      expect(circuit.nConstraints).toBeGreaterThan(0);
      expect(circuit.nVars).toBeGreaterThan(0);
    });

    it('should compile query with multiple patterns', async () => {
      const query = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX : <http://example.org/>
        SELECT ?s ?age
        WHERE {
          ?s rdf:type :Person .
          ?s :age ?age
        }
      `;

      const circuit = await compiler.compile(query);

      expect(circuit.constraints.length).toBeGreaterThan(1);
    });

    it('should compile query with FILTER', async () => {
      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s ?age
        WHERE {
          ?s :age ?age
          FILTER(?age > 18)
        }
      `;

      const circuit = await compiler.compile(query);

      expect(circuit.constraints.length).toBeGreaterThan(0);
      expect(circuit.publicInputs.length).toBeGreaterThan(0);
    });

    it('should compile query with multiple FILTERs', async () => {
      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s ?age
        WHERE {
          ?s :age ?age
          FILTER(?age > 18)
          FILTER(?age < 65)
        }
      `;

      const circuit = await compiler.compile(query);

      expect(circuit.constraints.length).toBeGreaterThan(1);
    });
  });

  describe('Constraint Generation', () => {
    it('should generate equality constraints', async () => {
      const query = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { :Alice rdf:type :Person }
      `;

      const circuit = await compiler.compile(query);

      expect(circuit.constraints.length).toBeGreaterThan(0);
      expect(circuit.constraints[0]).toHaveProperty('a');
      expect(circuit.constraints[0]).toHaveProperty('b');
      expect(circuit.constraints[0]).toHaveProperty('c');
    });

    it('should generate comparison constraints for FILTER', async () => {
      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s :age ?age FILTER(?age > 18) }
      `;

      const circuit = await compiler.compile(query);

      const hasComparisonConstraint = circuit.constraints.some(
        (c) => Object.keys(c.a).length > 1
      );

      expect(hasComparisonConstraint).toBe(true);
    });
  });

  describe('Optimization', () => {
    it('should remove duplicate constraints when optimizing', async () => {
      const compiler = new CircuitCompiler({ optimize: true });

      const query = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX : <http://example.org/>
        SELECT ?s
        WHERE {
          ?s rdf:type :Person .
          ?s rdf:type :Person
        }
      `;

      const circuit = await compiler.compile(query);

      const compilerNoOpt = new CircuitCompiler({ optimize: false });
      const circuitNoOpt = await compilerNoOpt.compile(query);

      expect(circuit.constraints.length).toBeLessThanOrEqual(
        circuitNoOpt.constraints.length
      );
    });

    it('should maintain correctness after optimization', async () => {
      const compilerOpt = new CircuitCompiler({ optimize: true });
      const compilerNoOpt = new CircuitCompiler({ optimize: false });

      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s :age ?age FILTER(?age > 18) }
      `;

      const circuitOpt = await compilerOpt.compile(query);
      const circuitNoOpt = await compilerNoOpt.compile(query);

      expect(circuitOpt.publicInputs).toEqual(circuitNoOpt.publicInputs);
    });
  });

  describe('Variable Allocation', () => {
    it('should allocate unique variables', async () => {
      const query = `
        SELECT ?s ?p ?o
        WHERE {
          ?s ?p ?o
        }
      `;

      const circuit = await compiler.compile(query);

      expect(circuit.nVars).toBeGreaterThan(0);
    });

    it('should track variable count correctly', async () => {
      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s :age ?age }
      `;

      const circuit = await compiler.compile(query);

      expect(circuit.nVars).toBeGreaterThanOrEqual(circuit.constraints.length);
    });
  });

  describe('SPARQL Feature Support', () => {
    it('should support greater-than FILTER', async () => {
      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s :value ?v FILTER(?v > 100) }
      `;

      const circuit = await compiler.compile(query);

      expect(circuit.constraints.length).toBeGreaterThan(0);
    });

    it('should support less-than FILTER', async () => {
      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s :value ?v FILTER(?v < 100) }
      `;

      const circuit = await compiler.compile(query);

      expect(circuit.constraints.length).toBeGreaterThan(0);
    });

    it('should support equality FILTER', async () => {
      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s :value ?v FILTER(?v = 100) }
      `;

      const circuit = await compiler.compile(query);

      expect(circuit.constraints.length).toBeGreaterThan(0);
    });

    it('should support inequality FILTER', async () => {
      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s :value ?v FILTER(?v != 100) }
      `;

      const circuit = await compiler.compile(query);

      expect(circuit.constraints.length).toBeGreaterThan(0);
    });
  });

  describe('Error Handling', () => {
    it('should reject non-SELECT queries', async () => {
      const query = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX : <http://example.org/>
        ASK { ?s rdf:type :Person }
      `;

      await expect(compiler.compile(query)).rejects.toThrow(/Unsupported query type/);
    });

    it('should reject queries exceeding constraint limit', async () => {
      const compiler = new CircuitCompiler({ maxConstraints: 5 });

      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s
        WHERE {
          ?s :p1 ?o1 .
          ?s :p2 ?o2 .
          ?s :p3 ?o3 .
          ?s :p4 ?o4 .
          ?s :p5 ?o5 .
          ?s :p6 ?o6 .
          ?s :p7 ?o7 .
          ?s :p8 ?o8 .
          ?s :p9 ?o9 .
          ?s :p10 ?o10
        }
      `;

      await expect(compiler.compile(query)).rejects.toThrow(/exceeds maximum/);
    });
  });

  describe('Compilation Performance', () => {
    it('should compile quickly', async () => {
      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s :age ?age FILTER(?age > 18) }
      `;

      const startTime = performance.now();

      await compiler.compile(query);

      const duration = performance.now() - startTime;

      expect(duration).toBeLessThan(100);
    });

    it('should scale reasonably with query complexity', async () => {
      const simpleQuery = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s rdf:type :Person }
      `;

      const complexQuery = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX : <http://example.org/>
        SELECT ?s
        WHERE {
          ?s rdf:type :Person .
          ?s :age ?age .
          ?s :name ?name
          FILTER(?age > 18)
        }
      `;

      const start1 = performance.now();
      await compiler.compile(simpleQuery);
      const time1 = performance.now() - start1;

      const start2 = performance.now();
      await compiler.compile(complexQuery);
      const time2 = performance.now() - start2;

      expect(time2).toBeLessThan(time1 * 10);
    });
  });

  describe('Factory Functions', () => {
    it('should create compiler with createCircuitCompiler', () => {
      const compiler = createCircuitCompiler({ maxConstraints: 5000 });

      expect(compiler).toBeInstanceOf(CircuitCompiler);
      expect(compiler.options.maxConstraints).toBe(5000);
    });

    it('should compile with compileSPARQL', async () => {
      const query = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s rdf:type :Person }
      `;

      const circuit = await compileSPARQL(query);

      expect(circuit).toBeDefined();
      expect(circuit.constraints).toBeDefined();
    });
  });

  describe('Public and Private Inputs', () => {
    it('should separate public and private inputs', async () => {
      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s :age ?age FILTER(?age > 18) }
      `;

      const circuit = await compiler.compile(query);

      expect(circuit.publicInputs.length).toBeGreaterThan(0);
      expect(circuit.privateInputs.length).toBeGreaterThan(0);
    });

    it('should include filter constants in public inputs', async () => {
      const query = `
        PREFIX : <http://example.org/>
        SELECT ?s WHERE { ?s :value ?v FILTER(?v > 100) }
      `;

      const circuit = await compiler.compile(query);

      const hasFilterConstant = circuit.publicInputs.some((input) =>
        input.includes('filter_const')
      );

      expect(hasFilterConstant).toBe(true);
    });
  });
});
