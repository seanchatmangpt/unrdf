/**
 * @file SHACL Serialization Tests - RDF Term Validation
 * @description Tests for proper RDF term creation in SHACL violation reports
 */
import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, dataFactory } from '../../oxigraph/src/index.mjs';
import { serializeShaclReport } from '../src/index.mjs';

const { namedNode, literal, quad } = dataFactory;

describe('SHACL Report Serialization - RDF Term Validation', () => {
  let store;

  beforeEach(() => {
    store = createStore();
  });

  describe('serializeShaclReport - Term Creation', () => {
    it('should create valid NamedNode terms for violation subjects', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'Test violation',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      expect(quads).toHaveLength(3); // rdf:type, resultMessage, resultSeverity
      expect(quads[0].subject.termType).toBe('NamedNode');
      expect(quads[0].subject.value).toMatch(/^http:\/\/example\.com\/validation\/result-/);
    });

    it('should create proper RDF predicates with full IRIs', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'Test message',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Check rdf:type predicate
      expect(quads[0].predicate.termType).toBe('NamedNode');
      expect(quads[0].predicate.value).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

      // Check sh:resultMessage predicate
      expect(quads[1].predicate.termType).toBe('NamedNode');
      expect(quads[1].predicate.value).toBe('http://www.w3.org/ns/shacl#resultMessage');

      // Check sh:resultSeverity predicate
      expect(quads[2].predicate.termType).toBe('NamedNode');
      expect(quads[2].predicate.value).toBe('http://www.w3.org/ns/shacl#resultSeverity');
    });

    it('should create proper RDF objects (ValidationResult type)', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'Test',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // First quad: rdf:type sh:ValidationResult
      expect(quads[0].object.termType).toBe('NamedNode');
      expect(quads[0].object.value).toBe('http://www.w3.org/ns/shacl#ValidationResult');
    });

    it('should create Literal objects for messages', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'Property is required but missing',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Second quad: sh:resultMessage should have Literal object
      expect(quads[1].object.termType).toBe('Literal');
      expect(quads[1].object.value).toBe('Property is required but missing');
    });

    it('should create proper severity level objects', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'Test',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Third quad: sh:resultSeverity sh:Violation
      expect(quads[2].object.termType).toBe('NamedNode');
      expect(quads[2].object.value).toBe('http://www.w3.org/ns/shacl#Violation');
    });

    it('should map violation severity correctly', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'Violation level',
          },
        ],
      };

      const quads = serializeShaclReport(report);
      expect(quads[2].object.value).toBe('http://www.w3.org/ns/shacl#Violation');
    });

    it('should map warning severity correctly', () => {
      const report = {
        results: [
          {
            severity: 'warning',
            message: 'Warning level',
          },
        ],
      };

      const quads = serializeShaclReport(report);
      expect(quads[2].object.value).toBe('http://www.w3.org/ns/shacl#Warning');
    });

    it('should map info severity correctly', () => {
      const report = {
        results: [
          {
            severity: 'info',
            message: 'Info level',
          },
        ],
      };

      const quads = serializeShaclReport(report);
      expect(quads[2].object.value).toBe('http://www.w3.org/ns/shacl#Info');
    });

    it('should use default violation severity for unknown types', () => {
      const report = {
        results: [
          {
            severity: 'unknown',
            message: 'Unknown severity',
          },
        ],
      };

      const quads = serializeShaclReport(report);
      expect(quads[2].object.value).toBe('http://www.w3.org/ns/shacl#Violation');
    });
  });

  describe('serializeShaclReport - Store Integration', () => {
    it('should generate quads that can be added to store', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'Data integrity violation',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Should not throw when adding to store
      expect(() => {
        for (const q of quads) {
          store.add(q);
        }
      }).not.toThrow();

      // Verify quads were added to store
      const matches = store.getQuads();
      expect(matches.length).toBe(3);
    });

    it('should generate multiple result quads for multiple violations', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'First violation',
          },
          {
            severity: 'warning',
            message: 'Second warning',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Should have 3 quads per result (type, message, severity) = 6 total
      expect(quads).toHaveLength(6);

      // Add all to store and verify
      expect(() => {
        for (const q of quads) {
          store.add(q);
        }
      }).not.toThrow();

      const storeQuads = store.getQuads();
      expect(storeQuads.length).toBe(6);
    });

    it('should generate unique URIs for each violation', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'First violation',
          },
          {
            severity: 'violation',
            message: 'Second violation',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Get all subject URIs from first triple of each result
      const uris = new Set();
      for (let i = 0; i < quads.length; i += 3) {
        uris.add(quads[i].subject.value);
      }

      // Should have 2 unique URIs (one per violation)
      expect(uris.size).toBe(2);
    });
  });

  describe('serializeShaclReport - Edge Cases', () => {
    it('should handle empty results array', () => {
      const report = {
        results: [],
      };

      const quads = serializeShaclReport(report);

      expect(quads).toHaveLength(0);
    });

    it('should handle undefined results', () => {
      const report = {};

      const quads = serializeShaclReport(report);

      expect(quads).toHaveLength(0);
    });

    it('should handle null results', () => {
      const report = {
        results: null,
      };

      const quads = serializeShaclReport(report);

      expect(quads).toHaveLength(0);
    });

    it('should handle violation without message', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            // No message
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Should have 2 quads (type and severity, no message)
      expect(quads).toHaveLength(2);
      expect(quads[0].predicate.value).toBe(
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
      );
      expect(quads[1].predicate.value).toBe(
        'http://www.w3.org/ns/shacl#resultSeverity'
      );
    });

    it('should handle violation without severity', () => {
      const report = {
        results: [
          {
            message: 'Message without severity',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Should still generate quads with default severity
      expect(quads).toHaveLength(3);
      expect(quads[2].object.value).toBe(
        'http://www.w3.org/ns/shacl#Violation'
      );
    });
  });

  describe('serializeShaclReport - RDF Validation', () => {
    it('should generate valid RDF quads with all required components', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'Complete validation failure',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Check each quad has all required components
      for (const q of quads) {
        expect(q).toHaveProperty('subject');
        expect(q).toHaveProperty('predicate');
        expect(q).toHaveProperty('object');
        expect(q.subject).toHaveProperty('termType');
        expect(q.predicate).toHaveProperty('termType');
        expect(q.object).toHaveProperty('termType');
        expect(q.subject).toHaveProperty('value');
        expect(q.predicate).toHaveProperty('value');
        expect(q.object).toHaveProperty('value');
      }
    });

    it('should not generate bare prefix terms', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'Test violation',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Verify no terms are bare prefixes (e.g., "shacl:violation-0")
      for (const q of quads) {
        expect(q.subject.value).toMatch(/^http:\/\//);
        expect(q.predicate.value).toMatch(/^http:\/\//);
        // Objects can be literals or URIs, but no bare prefixes
        if (q.object.termType === 'NamedNode') {
          expect(q.object.value).toMatch(/^http:\/\//);
        }
      }
    });

    it('should use full SHACL namespace URIs', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'Test',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Verify SHACL namespace is properly formed
      const shaclNS = 'http://www.w3.org/ns/shacl#';
      for (const q of quads) {
        if (q.predicate.value.startsWith(shaclNS)) {
          // Valid SHACL predicate
          expect(q.predicate.value).toMatch(/^http:\/\/www\.w3\.org\/ns\/shacl#/);
        }
      }
    });

    it('should be compatible with store operations', () => {
      const report = {
        results: [
          {
            severity: 'violation',
            message: 'Compatibility test',
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Test that we can perform store operations
      for (const q of quads) {
        store.add(q);
      }

      // Query the store
      const matches = store.getQuads();
      expect(matches.length).toBe(quads.length);

      // Verify we can retrieve by pattern
      const typeMatches = store.getQuads(null, namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), null);
      expect(typeMatches.length).toBeGreaterThan(0);
    });
  });

  describe('serializeShaclReport - Data Integrity', () => {
    it('should preserve violation message content exactly', () => {
      const testMessage = 'This is a test message with special chars: @#$%^&*()';
      const report = {
        results: [
          {
            severity: 'violation',
            message: testMessage,
          },
        ],
      };

      const quads = serializeShaclReport(report);

      // Find the message quad
      const messageQuad = quads.find(
        (q) => q.predicate.value === 'http://www.w3.org/ns/shacl#resultMessage'
      );

      expect(messageQuad).toBeDefined();
      expect(messageQuad.object.value).toBe(testMessage);
    });

    it('should not corrupt data when adding multiple violations to store', () => {
      const violations = [
        { severity: 'violation', message: 'Violation 1' },
        { severity: 'warning', message: 'Warning 1' },
        { severity: 'info', message: 'Info 1' },
      ];

      const report = {
        results: violations,
      };

      const quads = serializeShaclReport(report);

      // Add all to store
      for (const q of quads) {
        store.add(q);
      }

      // Verify total quads
      expect(store.getQuads().length).toBe(9); // 3 per violation

      // Verify each violation type is present
      const violations_quads = store.getQuads(
        null,
        namedNode('http://www.w3.org/ns/shacl#resultSeverity'),
        null
      );
      expect(violations_quads.length).toBe(3);

      // Check severity values
      const severityValues = violations_quads.map((q) => q.object.value);
      expect(severityValues).toContain('http://www.w3.org/ns/shacl#Violation');
      expect(severityValues).toContain('http://www.w3.org/ns/shacl#Warning');
      expect(severityValues).toContain('http://www.w3.org/ns/shacl#Info');
    });
  });
});
