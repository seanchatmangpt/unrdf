/**
 * Comprehensive SHACL Validation Test Suite
 *
 * Tests the full rdf-validate-shacl integration:
 * - Cardinality constraints (minCount, maxCount)
 * - Datatype validation (xsd:string, xsd:integer, xsd:dateTime)
 * - Node shape targeting (sh:targetClass)
 * - Violation reporting (focusNode, resultPath, resultMessage, severity)
 * - Integration with repair/annotate enforcement modes
 * - Shape caching performance
 * - Edge cases and error handling
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  validateShacl,
  validateNode,
  isConforming,
  getViolations,
  getWarnings,
  clearValidatorCache,
} from '../src/hooks/validate.mjs';

const { namedNode, literal, quad } = dataFactory;

const SH = 'http://www.w3.org/ns/shacl#';
const XSD = 'http://www.w3.org/2001/XMLSchema#';
const RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
const EX = 'http://example.org/';

// Reusable shapes Turtle strings
const personShapes = `
@prefix sh: <${SH}> .
@prefix ex: <${EX}> .
@prefix xsd: <${XSD}> .

ex:PersonShape
  a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [
    sh:path ex:name ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:string ;
  ] ;
  sh:property [
    sh:path ex:age ;
    sh:maxCount 1 ;
    sh:datatype xsd:integer ;
  ] .
`;

function loadTurtle(store, turtle) {
  store.load(turtle, { format: 'text/turtle' });
}

describe('SHACL Validation - Complete Suite', () => {
  beforeEach(() => {
    clearValidatorCache();
  });

  // ── Cardinality constraints ────────────────────────────────────────

  describe('Cardinality Constraints', () => {
    it('should detect minCount violation when required property is missing', async () => {
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> .`);

      const report = await validateShacl(store, personShapes);

      expect(report.conforms).toBe(false);
      expect(report.results.length).toBeGreaterThanOrEqual(1);
      const nameViolation = report.results.find(r => r.resultPath === `${EX}name`);
      expect(nameViolation).toBeDefined();
      expect(nameViolation.severity).toBe('violation');
      expect(nameViolation.focusNode).toBe(`${EX}p1`);
    });

    it('should detect maxCount violation when too many values exist', async () => {
      const store = createStore();
      loadTurtle(
        store,
        `
        <${EX}p1> a <${EX}Person> ;
          <${EX}name> "Alice" ;
          <${EX}name> "Bob" .
      `
      );

      const report = await validateShacl(store, personShapes);

      expect(report.conforms).toBe(false);
      const nameViolation = report.results.find(r => r.resultPath === `${EX}name`);
      expect(nameViolation).toBeDefined();
      expect(nameViolation.message).toMatch(/more than 1/i);
    });

    it('should pass when cardinality constraints are satisfied', async () => {
      const store = createStore();
      loadTurtle(
        store,
        `<${EX}p1> a <${EX}Person> ; <${EX}name> "Alice" .`
      );

      const report = await validateShacl(store, personShapes);

      expect(report.conforms).toBe(true);
      expect(report.results).toHaveLength(0);
    });

    it('should allow optional properties (no minCount) to be absent', async () => {
      const store = createStore();
      // age has no minCount, so it's optional
      loadTurtle(
        store,
        `<${EX}p1> a <${EX}Person> ; <${EX}name> "Alice" .`
      );

      const report = await validateShacl(store, personShapes);

      expect(report.conforms).toBe(true);
    });

    it('should validate minCount of 0 (always valid)', async () => {
      const shapes = `
        @prefix sh: <${SH}> .
        @prefix ex: <${EX}> .
        @prefix xsd: <${XSD}> .
        ex:ItemShape a sh:NodeShape ;
          sh:targetClass ex:Item ;
          sh:property [ sh:path ex:tag ; sh:minCount 0 ] .
      `;
      const store = createStore();
      loadTurtle(store, `<${EX}i1> a <${EX}Item> .`);

      const report = await validateShacl(store, shapes);

      expect(report.conforms).toBe(true);
    });

    it('should detect violation when minCount > 1 and not enough values', async () => {
      const shapes = `
        @prefix sh: <${SH}> .
        @prefix ex: <${EX}> .
        ex:TeamShape a sh:NodeShape ;
          sh:targetClass ex:Team ;
          sh:property [ sh:path ex:member ; sh:minCount 2 ] .
      `;
      const store = createStore();
      loadTurtle(
        store,
        `<${EX}t1> a <${EX}Team> ; <${EX}member> <${EX}alice> .`
      );

      const report = await validateShacl(store, shapes);

      expect(report.conforms).toBe(false);
      expect(report.results[0].resultPath).toBe(`${EX}member`);
    });
  });

  // ── Datatype validation ────────────────────────────────────────────

  describe('Datatype Validation', () => {
    it('should detect datatype violation for wrong type', async () => {
      const store = createStore();
      // name should be xsd:string but we give it an integer
      loadTurtle(
        store,
        `<${EX}p1> a <${EX}Person> ; <${EX}name> 42 .`
      );

      const report = await validateShacl(store, personShapes);

      expect(report.conforms).toBe(false);
      const dtViolation = report.results.find(
        r => r.resultPath === `${EX}name` && r.message?.includes('datatype')
      );
      expect(dtViolation).toBeDefined();
    });

    it('should accept correct xsd:integer values', async () => {
      const store = createStore();
      loadTurtle(
        store,
        `
        <${EX}p1> a <${EX}Person> ;
          <${EX}name> "Alice" ;
          <${EX}age> 30 .
      `
      );

      const report = await validateShacl(store, personShapes);

      expect(report.conforms).toBe(true);
    });

    it('should detect xsd:integer datatype violation on string value', async () => {
      const store = createStore();
      loadTurtle(
        store,
        `
        <${EX}p1> a <${EX}Person> ;
          <${EX}name> "Alice" ;
          <${EX}age> "not-a-number" .
      `
      );

      const report = await validateShacl(store, personShapes);

      expect(report.conforms).toBe(false);
      const ageViolation = report.results.find(r => r.resultPath === `${EX}age`);
      expect(ageViolation).toBeDefined();
      expect(ageViolation.message).toMatch(/datatype/i);
    });

    it('should validate xsd:dateTime datatype', async () => {
      const shapes = `
        @prefix sh: <${SH}> .
        @prefix ex: <${EX}> .
        @prefix xsd: <${XSD}> .
        ex:EventShape a sh:NodeShape ;
          sh:targetClass ex:Event ;
          sh:property [ sh:path ex:startDate ; sh:datatype xsd:dateTime ; sh:minCount 1 ] .
      `;
      const store = createStore();
      loadTurtle(
        store,
        `<${EX}e1> a <${EX}Event> ; <${EX}startDate> "2026-01-01T00:00:00"^^<${XSD}dateTime> .`
      );

      const report = await validateShacl(store, shapes);

      expect(report.conforms).toBe(true);
    });

    it('should detect invalid dateTime value', async () => {
      const shapes = `
        @prefix sh: <${SH}> .
        @prefix ex: <${EX}> .
        @prefix xsd: <${XSD}> .
        ex:EventShape a sh:NodeShape ;
          sh:targetClass ex:Event ;
          sh:property [ sh:path ex:startDate ; sh:datatype xsd:dateTime ; sh:minCount 1 ] .
      `;
      const store = createStore();
      loadTurtle(
        store,
        `<${EX}e1> a <${EX}Event> ; <${EX}startDate> "not-a-date" .`
      );

      const report = await validateShacl(store, shapes);

      expect(report.conforms).toBe(false);
    });
  });

  // ── Node shape targeting ───────────────────────────────────────────

  describe('Node Shape Targeting', () => {
    it('should only validate instances of the target class', async () => {
      const store = createStore();
      // p1 is a Person (requires name), p2 is an Animal (no shape constraints)
      loadTurtle(
        store,
        `
        <${EX}p1> a <${EX}Person> ; <${EX}name> "Alice" .
        <${EX}a1> a <${EX}Animal> .
      `
      );

      const report = await validateShacl(store, personShapes);

      expect(report.conforms).toBe(true);
    });

    it('should validate all instances of target class', async () => {
      const store = createStore();
      loadTurtle(
        store,
        `
        <${EX}p1> a <${EX}Person> ; <${EX}name> "Alice" .
        <${EX}p2> a <${EX}Person> .
      `
      );

      const report = await validateShacl(store, personShapes);

      expect(report.conforms).toBe(false);
      const p2Violation = report.results.find(r => r.focusNode === `${EX}p2`);
      expect(p2Violation).toBeDefined();
    });

    it('should handle empty store (no target instances)', async () => {
      const store = createStore();

      const report = await validateShacl(store, personShapes);

      expect(report.conforms).toBe(true);
      expect(report.results).toHaveLength(0);
    });

    it('should handle multiple shapes targeting different classes', async () => {
      const shapes = `
        @prefix sh: <${SH}> .
        @prefix ex: <${EX}> .
        @prefix xsd: <${XSD}> .
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person ;
          sh:property [ sh:path ex:name ; sh:minCount 1 ; sh:datatype xsd:string ] .
        ex:ProductShape a sh:NodeShape ;
          sh:targetClass ex:Product ;
          sh:property [ sh:path ex:price ; sh:minCount 1 ; sh:datatype xsd:decimal ] .
      `;
      const store = createStore();
      loadTurtle(
        store,
        `
        <${EX}p1> a <${EX}Person> ; <${EX}name> "Alice" .
        <${EX}prod1> a <${EX}Product> .
      `
      );

      const report = await validateShacl(store, shapes);

      expect(report.conforms).toBe(false);
      // Only Product should have a violation (missing price)
      expect(report.results.some(r => r.focusNode === `${EX}prod1`)).toBe(true);
      expect(report.results.some(r => r.focusNode === `${EX}p1`)).toBe(false);
    });
  });

  // ── Violation reporting ────────────────────────────────────────────

  describe('Violation Reporting', () => {
    it('should include focusNode in violation', async () => {
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> .`);

      const report = await validateShacl(store, personShapes);

      expect(report.results[0].focusNode).toBe(`${EX}p1`);
    });

    it('should include resultPath in violation', async () => {
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> .`);

      const report = await validateShacl(store, personShapes);

      expect(report.results[0].resultPath).toBe(`${EX}name`);
    });

    it('should include resultMessage in violation', async () => {
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> .`);

      const report = await validateShacl(store, personShapes);

      expect(report.results[0].resultMessage).toBeTruthy();
      expect(typeof report.results[0].resultMessage).toBe('string');
    });

    it('should map severity correctly', async () => {
      const shapes = `
        @prefix sh: <${SH}> .
        @prefix ex: <${EX}> .
        @prefix xsd: <${XSD}> .
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person ;
          sh:property [
            sh:path ex:nickname ;
            sh:minCount 1 ;
            sh:severity sh:Warning ;
          ] .
      `;
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> .`);

      const report = await validateShacl(store, shapes);

      expect(report.conforms).toBe(false);
      expect(report.results[0].severity).toBe('warning');
    });

    it('should include timestamp in report', async () => {
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> ; <${EX}name> "Alice" .`);

      const report = await validateShacl(store, personShapes);

      expect(report.timestamp).toBeDefined();
      expect(new Date(report.timestamp).getTime()).not.toBeNaN();
    });

    it('should include details when includeDetails is true', async () => {
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> ; <${EX}name> "Alice" .`);

      const report = await validateShacl(store, personShapes, { includeDetails: true });

      expect(report.details).toBeDefined();
      expect(report.details.shapesCount).toBeGreaterThanOrEqual(1);
    });

    it('should report multiple violations for the same focus node', async () => {
      const shapes = `
        @prefix sh: <${SH}> .
        @prefix ex: <${EX}> .
        @prefix xsd: <${XSD}> .
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person ;
          sh:property [ sh:path ex:name ; sh:minCount 1 ; sh:datatype xsd:string ] ;
          sh:property [ sh:path ex:email ; sh:minCount 1 ; sh:datatype xsd:string ] .
      `;
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> .`);

      const report = await validateShacl(store, shapes);

      expect(report.conforms).toBe(false);
      expect(report.results.length).toBeGreaterThanOrEqual(2);
    });
  });

  // ── Utility functions ──────────────────────────────────────────────

  describe('Utility Functions', () => {
    it('isConforming returns true for conforming report', () => {
      expect(isConforming({ conforms: true, results: [] })).toBe(true);
    });

    it('isConforming returns false for non-conforming report', () => {
      expect(isConforming({ conforms: false, results: [{ severity: 'violation' }] })).toBe(false);
    });

    it('getViolations filters only violations', () => {
      const report = {
        results: [
          { severity: 'violation', message: 'v1' },
          { severity: 'warning', message: 'w1' },
          { severity: 'violation', message: 'v2' },
        ],
      };
      const violations = getViolations(report);
      expect(violations).toHaveLength(2);
      expect(violations.every(v => v.severity === 'violation')).toBe(true);
    });

    it('getWarnings filters only warnings', () => {
      const report = {
        results: [
          { severity: 'violation', message: 'v1' },
          { severity: 'warning', message: 'w1' },
        ],
      };
      const warnings = getWarnings(report);
      expect(warnings).toHaveLength(1);
      expect(warnings[0].severity).toBe('warning');
    });

    it('validateNode includes node in report', async () => {
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> ; <${EX}name> "Alice" .`);

      const report = await validateNode(
        store,
        namedNode(`${EX}p1`),
        personShapes
      );

      expect(report.node).toBe(`${EX}p1`);
      expect(report.conforms).toBe(true);
    });
  });

  // ── Error handling ─────────────────────────────────────────────────

  describe('Error Handling', () => {
    it('should return error report for invalid data store', async () => {
      const report = await validateShacl(null, personShapes);

      expect(report.conforms).toBe(false);
      expect(report.error).toBeDefined();
      expect(report.results[0].message).toMatch(/invalid data store/i);
    });

    it('should return error report for invalid shapes string', async () => {
      const store = createStore();
      const report = await validateShacl(store, null);

      expect(report.conforms).toBe(false);
      expect(report.error).toBeDefined();
      expect(report.results[0].message).toMatch(/invalid shapes string/i);
    });

    it('should return error report for malformed Turtle', async () => {
      const store = createStore();
      const report = await validateShacl(store, 'this is not valid turtle @@@');

      expect(report.conforms).toBe(false);
      expect(report.error).toBeDefined();
    });
  });

  // ── Caching ────────────────────────────────────────────────────────

  describe('Shape Caching', () => {
    it('should produce same results on repeated calls with same shapes', async () => {
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> .`);

      const report1 = await validateShacl(store, personShapes);
      const report2 = await validateShacl(store, personShapes);

      expect(report1.conforms).toBe(report2.conforms);
      expect(report1.results.length).toBe(report2.results.length);
    });

    it('clearValidatorCache should clear without breaking subsequent calls', async () => {
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> ; <${EX}name> "Alice" .`);

      await validateShacl(store, personShapes);
      clearValidatorCache();
      const report = await validateShacl(store, personShapes);

      expect(report.conforms).toBe(true);
    });
  });

  // ── Integration with enforcement modes ─────────────────────────────

  describe('Integration with Enforcement Modes', () => {
    it('block mode: report contains enough info for enforcement', async () => {
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> .`);

      const report = await validateShacl(store, personShapes);

      // Block mode just checks report.conforms
      expect(report.conforms).toBe(false);
      expect(report.results.length).toBeGreaterThan(0);
      // Each result has all the fields needed for serialization
      for (const r of report.results) {
        expect(r).toHaveProperty('severity');
        expect(r).toHaveProperty('message');
        expect(r).toHaveProperty('focusNode');
        expect(r).toHaveProperty('resultPath');
      }
    });

    it('annotate mode: results can be serialized to RDF triples', async () => {
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> .`);

      const report = await validateShacl(store, personShapes);

      // Verify results have the format needed by serializeShaclReport
      for (const r of report.results) {
        expect(typeof r.severity).toBe('string');
        expect(['violation', 'warning', 'info']).toContain(r.severity);
        expect(typeof r.message === 'string' || r.message === null).toBe(true);
      }
    });

    it('repair mode: can re-validate after store modification', async () => {
      const store = createStore();
      loadTurtle(store, `<${EX}p1> a <${EX}Person> .`);

      // First validation: missing name
      const report1 = await validateShacl(store, personShapes);
      expect(report1.conforms).toBe(false);

      // Simulate repair: add the missing name
      store.add(
        quad(
          namedNode(`${EX}p1`),
          namedNode(`${EX}name`),
          literal('Alice')
        )
      );

      // Re-validate: should now conform
      const report2 = await validateShacl(store, personShapes);
      expect(report2.conforms).toBe(true);
    });
  });
});
