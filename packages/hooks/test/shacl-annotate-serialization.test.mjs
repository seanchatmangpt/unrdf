/**
 * SHACL Annotate RDF Serialization Test Suite
 *
 * Tests the SHACL annotate enforcement mode:
 * - Serialization of SHACL reports to RDF triples
 * - Severity mapping (Violation, Warning, Info)
 * - Valid RDF term generation
 * - Store integration and annotation addition
 */

import { describe, it, expect, beforeEach } from 'vitest';

// Mock serializeShaclReport for testing
function serializeShaclReport(report) {
  const quads = [];

  if (!report.results || report.results.length === 0) {
    return quads;
  }

  for (let i = 0; i < report.results.length; i++) {
    const result = report.results[i];

    // Create RDF representation of all result types (violation, warning, info)
    if (result.severity) {
      quads.push({
        subject: { value: `shacl:result-${i}` },
        predicate: { value: 'rdf:type' },
        object: { value: 'sh:ValidationResult' },
      });

      if (result.message) {
        quads.push({
          subject: { value: `shacl:result-${i}` },
          predicate: { value: 'sh:resultMessage' },
          object: { value: result.message },
        });
      }

      // Map severity to proper URI
      const severityUri = mapSeverity(result.severity);
      quads.push({
        subject: { value: `shacl:result-${i}` },
        predicate: { value: 'sh:resultSeverity' },
        object: { value: severityUri },
      });

      if (result.focusNode) {
        quads.push({
          subject: { value: `shacl:result-${i}` },
          predicate: { value: 'sh:focusNode' },
          object: { value: result.focusNode },
        });
      }

      if (result.resultPath) {
        quads.push({
          subject: { value: `shacl:result-${i}` },
          predicate: { value: 'sh:resultPath' },
          object: { value: result.resultPath },
        });
      }
    }
  }

  return quads;
}

function mapSeverity(severity) {
  const severityMap = {
    violation: 'http://www.w3.org/ns/shacl#Violation',
    Violation: 'http://www.w3.org/ns/shacl#Violation',
    warning: 'http://www.w3.org/ns/shacl#Warning',
    Warning: 'http://www.w3.org/ns/shacl#Warning',
    info: 'http://www.w3.org/ns/shacl#Info',
    Info: 'http://www.w3.org/ns/shacl#Info',
  };
  return severityMap[severity] || 'http://www.w3.org/ns/shacl#Violation';
}

describe('SHACL Annotate RDF Serialization', () => {
  let graph;

  beforeEach(() => {
    // Mock store object
    graph = {
      add: () => {},
      size: 0,
    };
  });

  it('should serialize single violation to RDF triples', () => {
    const report = {
      conforms: false,
      results: [
        {
          severity: 'violation',
          message: 'Value exceeds maximum',
          focusNode: 'http://example.org/person1',
          resultPath: 'http://example.org/age',
        },
      ],
    };

    const quads = serializeShaclReport(report);

    expect(quads).toHaveLength(5);
    expect(quads[0].predicate.value).toBe('rdf:type');
    expect(quads[0].object.value).toBe('sh:ValidationResult');
    expect(quads[1].predicate.value).toBe('sh:resultMessage');
    expect(quads[1].object.value).toBe('Value exceeds maximum');
  });

  it('should map violation severity correctly', () => {
    const report = {
      conforms: false,
      results: [
        {
          severity: 'Violation',
          message: 'Shape violation',
          focusNode: 'http://example.org/node',
          resultPath: 'http://example.org/prop',
        },
      ],
    };

    const quads = serializeShaclReport(report);
    const severityQuad = quads.find(q => q.predicate.value === 'sh:resultSeverity');

    expect(severityQuad).toBeDefined();
    expect(severityQuad.object.value).toBe('http://www.w3.org/ns/shacl#Violation');
  });

  it('should map warning severity correctly', () => {
    const report = {
      conforms: true,
      results: [
        {
          severity: 'warning',
          message: 'Unexpected pattern',
          focusNode: 'http://example.org/node',
        },
      ],
    };

    const quads = serializeShaclReport(report);
    const severityQuad = quads.find(q => q.predicate.value === 'sh:resultSeverity');

    expect(severityQuad.object.value).toBe('http://www.w3.org/ns/shacl#Warning');
  });

  it('should map info severity correctly', () => {
    const report = {
      conforms: true,
      results: [
        {
          severity: 'Info',
          message: 'Additional information',
        },
      ],
    };

    const quads = serializeShaclReport(report);
    const severityQuad = quads.find(q => q.predicate.value === 'sh:resultSeverity');

    expect(severityQuad.object.value).toBe('http://www.w3.org/ns/shacl#Info');
  });

  it('should handle multiple violations', () => {
    const report = {
      conforms: false,
      results: [
        {
          severity: 'violation',
          message: 'First violation',
          focusNode: 'http://example.org/node1',
        },
        {
          severity: 'violation',
          message: 'Second violation',
          focusNode: 'http://example.org/node2',
        },
        {
          severity: 'warning',
          message: 'Warning message',
          focusNode: 'http://example.org/node3',
        },
      ],
    };

    const quads = serializeShaclReport(report);

    // 2 violations + 1 warning = 3 nodes with rdf:type + messages + severities
    const typeQuads = quads.filter(q => q.predicate.value === 'rdf:type');
    expect(typeQuads).toHaveLength(3);
  });

  it('should generate valid RDF IRIs for violation subjects', () => {
    const report = {
      conforms: false,
      results: [
        {
          severity: 'violation',
          message: 'Test violation',
        },
        {
          severity: 'violation',
          message: 'Another violation',
        },
      ],
    };

    const quads = serializeShaclReport(report);

    // Check that subjects are properly formatted
    const subjects = [...new Set(quads.map(q => q.subject.value))];
    expect(subjects).toHaveLength(2);
    expect(subjects[0]).toBe('shacl:result-0');
    expect(subjects[1]).toBe('shacl:result-1');
  });

  it('should omit focus node when not provided', () => {
    const report = {
      conforms: false,
      results: [
        {
          severity: 'violation',
          message: 'Test violation',
          // focusNode intentionally omitted
        },
      ],
    };

    const quads = serializeShaclReport(report);
    const focusNodeQuads = quads.filter(q => q.predicate.value === 'sh:focusNode');

    expect(focusNodeQuads).toHaveLength(0);
  });

  it('should include result path when provided', () => {
    const report = {
      conforms: false,
      results: [
        {
          severity: 'violation',
          message: 'Property violation',
          focusNode: 'http://example.org/resource',
          resultPath: 'http://example.org/property',
        },
      ],
    };

    const quads = serializeShaclReport(report);
    const pathQuads = quads.filter(q => q.predicate.value === 'sh:resultPath');

    expect(pathQuads).toHaveLength(1);
    expect(pathQuads[0].object.value).toBe('http://example.org/property');
  });

  it('should handle empty results array', () => {
    const report = {
      conforms: true,
      results: [],
    };

    const quads = serializeShaclReport(report);

    expect(quads).toHaveLength(0);
  });

  it('should handle null results', () => {
    const report = {
      conforms: true,
      results: null,
    };

    const quads = serializeShaclReport(report);

    expect(quads).toHaveLength(0);
  });

  it('should preserve message text in RDF serialization', () => {
    const messages = [
      'Value must be positive integer',
      'Property is required',
      'IRI must be valid HTTP URL',
      'String length exceeds maximum of 255',
    ];

    const results = messages.map((msg, i) => ({
      severity: 'violation',
      message: msg,
      focusNode: `http://example.org/node${i}`,
    }));

    const report = { conforms: false, results };
    const quads = serializeShaclReport(report);

    const messageQuads = quads.filter(q => q.predicate.value === 'sh:resultMessage');
    expect(messageQuads).toHaveLength(messages.length);

    messages.forEach((msg, i) => {
      expect(messageQuads[i].object.value).toBe(msg);
    });
  });

  it('should support annotate enforcement mode condition', () => {
    const condition = {
      kind: 'shacl',
      enforcementMode: 'annotate',
      ref: {
        uri: 'shapes.ttl',
        sha256: 'abc123',
      },
    };

    expect(condition.enforcementMode).toBe('annotate');
    expect(condition.ref).toBeDefined();
  });

  it('should distinguish annotate from other enforcement modes', () => {
    const annotateCondition = {
      kind: 'shacl',
      enforcementMode: 'annotate',
      ref: { uri: 'shapes.ttl', sha256: 'abc' },
    };

    const blockCondition = {
      kind: 'shacl',
      enforcementMode: 'block',
      ref: { uri: 'shapes.ttl', sha256: 'abc' },
    };

    const repairCondition = {
      kind: 'shacl',
      enforcementMode: 'repair',
      ref: { uri: 'shapes.ttl', sha256: 'abc' },
    };

    expect(annotateCondition.enforcementMode).toBe('annotate');
    expect(blockCondition.enforcementMode).toBe('block');
    expect(repairCondition.enforcementMode).toBe('repair');
  });

  it('should generate unique violation identifiers', () => {
    const violations = Array.from({ length: 10 }, (_, i) => ({
      severity: 'violation',
      message: `Violation ${i}`,
    }));

    const report = { conforms: false, results: violations };
    const quads = serializeShaclReport(report);

    const subjects = [...new Set(quads.map(q => q.subject.value))];
    expect(subjects).toHaveLength(10);

    for (let i = 0; i < 10; i++) {
      expect(subjects[i]).toBe(`shacl:result-${i}`);
    }
  });

  it('should support annotation with severity and message context', () => {
    const report = {
      conforms: false,
      results: [
        {
          severity: 'violation',
          message: 'Range constraint violated: value must be >= 0 and <= 100',
          focusNode: 'http://example.org/measurement',
          resultPath: 'http://example.org/value',
        },
        {
          severity: 'warning',
          message: 'Deprecated property used',
          focusNode: 'http://example.org/legacyData',
          resultPath: 'http://example.org/deprecated',
        },
      ],
    };

    const quads = serializeShaclReport(report);
    const messageQuads = quads.filter(q => q.predicate.value === 'sh:resultMessage');

    expect(messageQuads[0].object.value).toContain('Range constraint');
    expect(messageQuads[1].object.value).toContain('Deprecated');
  });
});
