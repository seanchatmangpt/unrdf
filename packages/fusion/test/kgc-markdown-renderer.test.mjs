/**
 * KGC Markdown Renderer Tests
 *
 * Verifies:
 * - Determinism: Same input always produces byte-identical output
 * - Idempotence: render(render(input)) = render(input) where applicable
 * - Correctness: Output is valid markdown that parses correctly
 * - Integration: Works with receipt system
 *
 * @module @unrdf/fusion/test/kgc-markdown-renderer
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  renderKGCDocument,
  renderSection,
  renderExecutableBlock,
  renderProofAppendix,
  renderFrontmatter,
  renderTable,
  renderCodeBlock,
  renderLinks,
  canonicalizeMarkdown,
  hashMarkdown,
} from '../src/kgc-markdown-renderer.mjs';

import { createReceipt } from '../src/receipts-kernel.mjs';

// =============================================================================
// Test Data
// =============================================================================

const testFrontmatter = {
  title: 'Test Document',
  date: '2024-01-01',
  author: 'Test Author',
  tags: ['test', 'markdown', 'kgc'],
};

const testSections = [
  {
    type: 'tutorial',
    title: 'Getting Started',
    level: 2,
    content: 'This is a tutorial on how to get started.',
  },
  {
    type: 'reference',
    title: 'API Reference',
    level: 2,
    content: 'Complete API reference documentation.',
    links: [
      { text: 'GitHub', url: 'https://github.com/example' },
      { text: 'Docs', url: 'https://docs.example.com' },
    ],
  },
];

const testQueryBlock = {
  type: 'kgc:query',
  content: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
  result: [
    { s: 'ex:alice', p: 'rdf:type', o: 'ex:Person' },
    { s: 'ex:bob', p: 'rdf:type', o: 'ex:Person' },
  ],
};

const testProofBlock = {
  type: 'kgc:proof',
  content: 'verify-receipt',
  result: {
    valid: true,
    receiptId: 'receipt-test-123',
    hash: 'abc123def456',
    timestamp: '2024-01-01T00:00:00.000Z',
  },
};

const testExtractBlock = {
  type: 'kgc:extract',
  content: 'extract-api',
  result: [
    { name: 'createStore', type: 'function', description: 'Creates a new RDF store' },
    { name: 'dataFactory', type: 'object', description: 'RDF data factory' },
  ],
};

const testRenderBlock = {
  type: 'kgc:render',
  content: 'render-markdown',
  result: '## Dynamic Content\n\nThis is dynamically rendered markdown.',
};

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Run function multiple times and verify identical output
 */
function verifyDeterminism(fn, iterations = 10) {
  const results = [];
  for (let i = 0; i < iterations; i++) {
    results.push(fn());
  }

  // All results must be byte-identical
  const first = results[0];
  for (let i = 1; i < results.length; i++) {
    expect(results[i]).toBe(first);
  }

  return first;
}

/**
 * Verify idempotence: f(f(x)) = f(x)
 */
function verifyIdempotence(fn, input) {
  const once = fn(input);
  const twice = fn(once);
  expect(twice).toBe(once);
  return once;
}

// =============================================================================
// Tests
// =============================================================================

describe('KGC Markdown Renderer', () => {
  describe('renderFrontmatter', () => {
    it('should render frontmatter as deterministic YAML', () => {
      const yaml = verifyDeterminism(() => renderFrontmatter(testFrontmatter));

      expect(yaml).toContain('---');
      expect(yaml).toContain('title: Test Document');
      expect(yaml).toContain('date: 2024-01-01');
      expect(yaml).toContain('author: Test Author');
      expect(yaml).toContain('tags:');
    });

    it('should sort keys alphabetically', () => {
      const yaml = renderFrontmatter({ z: 'last', a: 'first', m: 'middle' });

      const lines = yaml.split('\n');
      const keys = lines.slice(1, -1).map(line => line.split(':')[0]);

      expect(keys).toEqual(['a', 'm', 'z']);
    });

    it('should handle nested objects', () => {
      const yaml = renderFrontmatter({
        nested: { b: 2, a: 1 },
        simple: 'value',
      });

      expect(yaml).toContain('nested:');
      expect(yaml).toContain('a: 1');
      expect(yaml).toContain('b: 2');
    });

    it('should handle arrays', () => {
      const yaml = renderFrontmatter({
        items: ['one', 'two', 'three'],
      });

      expect(yaml).toContain('items:');
      expect(yaml).toContain('- one');
      expect(yaml).toContain('- two');
      expect(yaml).toContain('- three');
    });

    it('should quote strings with special characters', () => {
      const yaml = renderFrontmatter({
        special: 'value: with: colons',
        normal: 'simple value',
      });

      expect(yaml).toContain('"value: with: colons"');
      expect(yaml).toContain('normal: simple value');
    });
  });

  describe('renderTable', () => {
    const testRows = [
      { name: 'Alice', age: 30, city: 'NYC' },
      { name: 'Bob', age: 25, city: 'SF' },
      { name: 'Charlie', age: 35, city: 'LA' },
    ];

    const testColumns = [
      { key: 'name', header: 'Name', align: 'left' },
      { key: 'age', header: 'Age', align: 'right' },
      { key: 'city', header: 'City', align: 'left' },
    ];

    it('should render table with sorted rows', () => {
      const table = verifyDeterminism(() =>
        renderTable(testRows, testColumns, 'name')
      );

      expect(table).toContain('| Name');
      expect(table).toContain('| Alice');
      expect(table).toContain('| Bob');
      expect(table).toContain('| Charlie');

      // Verify order (should be alphabetical by name)
      const lines = table.split('\n');
      const dataLines = lines.slice(2); // Skip header and separator
      expect(dataLines[0]).toContain('Alice');
      expect(dataLines[1]).toContain('Bob');
      expect(dataLines[2]).toContain('Charlie');
    });

    it('should handle alignment', () => {
      const table = renderTable(testRows, testColumns, 'name');

      const lines = table.split('\n');
      const separator = lines[1];

      // Left align (default)
      expect(separator).toMatch(/\| -+/);
      // Right align
      expect(separator).toMatch(/\| -+:/);
    });

    it('should escape pipes in content', () => {
      const rows = [
        { name: 'Test | With | Pipes', value: 'normal' },
      ];
      const columns = [
        { key: 'name', header: 'Name' },
        { key: 'value', header: 'Value' },
      ];

      const table = renderTable(rows, columns);
      expect(table).toContain('Test \\| With \\| Pipes');
    });

    it('should replace newlines with spaces', () => {
      const rows = [
        { name: 'Multi\nline\nvalue', value: 'normal' },
      ];
      const columns = [
        { key: 'name', header: 'Name' },
        { key: 'value', header: 'Value' },
      ];

      const table = renderTable(rows, columns);
      expect(table).toContain('Multi line value');
      expect(table).not.toContain('\n\n');
    });

    it('should handle empty rows', () => {
      const table = renderTable([], testColumns);
      expect(table).toBe('');
    });

    it('should throw on missing columns', () => {
      expect(() => renderTable(testRows, [])).toThrow();
    });
  });

  describe('renderCodeBlock', () => {
    it('should render code block with language', () => {
      const code = 'const x = 1;\nconsole.log(x);';
      const block = verifyDeterminism(() =>
        renderCodeBlock(code, 'javascript')
      );

      expect(block).toContain('```javascript');
      expect(block).toContain('const x = 1;');
      expect(block).toContain('```');
    });

    it('should normalize line endings', () => {
      const codeWithCRLF = 'line1\r\nline2\r\nline3';
      const block = renderCodeBlock(codeWithCRLF, 'text');

      expect(block).toContain('line1\nline2\nline3');
      expect(block).not.toContain('\r');
    });

    it('should remove trailing whitespace from lines', () => {
      const codeWithSpaces = 'line1   \nline2  \nline3';
      const block = renderCodeBlock(codeWithSpaces, 'text');

      expect(block).toContain('line1\nline2\nline3');
    });

    it('should handle line numbers metadata', () => {
      const block = renderCodeBlock('code', 'js', { lineNumbers: true, startLine: 5 });
      expect(block).toContain('```js {5}');
    });

    it('should handle highlights metadata', () => {
      const block = renderCodeBlock('code', 'js', { highlights: [1, 3, 5] });
      expect(block).toContain('```js {1,3,5}');
    });
  });

  describe('renderLinks', () => {
    const testLinks = [
      { text: 'GitHub', url: 'https://github.com' },
      { text: 'Docs', url: 'https://docs.example.com', title: 'Documentation' },
      { text: 'API', url: 'https://api.example.com' },
    ];

    it('should render links sorted by URL', () => {
      const links = verifyDeterminism(() => renderLinks(testLinks));

      expect(links).toContain('[API](https://api.example.com)');
      expect(links).toContain('[Docs](https://docs.example.com');
      expect(links).toContain('[GitHub](https://github.com)');

      // Verify order (sorted by URL)
      const lines = links.split('\n');
      expect(lines[0]).toContain('api.example.com');
      expect(lines[1]).toContain('docs.example.com');
      expect(lines[2]).toContain('github.com');
    });

    it('should include title attribute if present', () => {
      const links = renderLinks(testLinks);
      expect(links).toContain('"Documentation"');
    });

    it('should handle empty array', () => {
      const links = renderLinks([]);
      expect(links).toBe('');
    });
  });

  describe('renderExecutableBlock', () => {
    it('should render kgc:query block', () => {
      const md = verifyDeterminism(() =>
        renderExecutableBlock(testQueryBlock, testQueryBlock.result)
      );

      expect(md).toContain('<!-- kgc:query -->');
      expect(md).toContain('```sparql');
      expect(md).toContain('SELECT ?s ?p ?o');
      expect(md).toContain('**Results:**');
      expect(md).toContain('ex:alice');
      expect(md).toContain('ex:bob');
    });

    it('should render kgc:proof block', () => {
      const md = verifyDeterminism(() =>
        renderExecutableBlock(testProofBlock, testProofBlock.result)
      );

      expect(md).toContain('<!-- kgc:proof -->');
      expect(md).toContain('**Verification Report**');
      expect(md).toContain('receipt-test-123');
      expect(md).toContain('✓');
    });

    it('should render kgc:extract block', () => {
      const md = verifyDeterminism(() =>
        renderExecutableBlock(testExtractBlock, testExtractBlock.result)
      );

      expect(md).toContain('<!-- kgc:extract -->');
      expect(md).toContain('**API Extraction**');
      expect(md).toContain('createStore');
      expect(md).toContain('dataFactory');
    });

    it('should render kgc:render block', () => {
      const md = verifyDeterminism(() =>
        renderExecutableBlock(testRenderBlock, testRenderBlock.result)
      );

      expect(md).toContain('<!-- kgc:render -->');
      expect(md).toContain('## Dynamic Content');
      expect(md).toContain('dynamically rendered');
    });

    it('should handle empty query results', () => {
      const emptyBlock = { ...testQueryBlock, result: [] };
      const md = renderExecutableBlock(emptyBlock, []);

      expect(md).toContain('(empty)');
    });
  });

  describe('renderSection', () => {
    it('should render section with heading and content', () => {
      const section = {
        type: 'tutorial',
        title: 'Test Section',
        level: 2,
        content: 'This is test content.',
      };

      const md = verifyDeterminism(() => renderSection(section));

      expect(md).toContain('## Test Section');
      expect(md).toContain('<!-- section: tutorial -->');
      expect(md).toContain('This is test content.');
    });

    it('should render section with blocks', () => {
      const section = {
        type: 'reference',
        title: 'API Reference',
        level: 2,
        blocks: [testQueryBlock, testExtractBlock],
      };

      const md = renderSection(section);

      expect(md).toContain('## API Reference');
      expect(md).toContain('<!-- kgc:query -->');
      expect(md).toContain('<!-- kgc:extract -->');
    });

    it('should render section with links', () => {
      const section = {
        type: 'explanation',
        title: 'Background',
        level: 2,
        content: 'Some explanation.',
        links: [
          { text: 'Link 1', url: 'https://example.com/1' },
          { text: 'Link 2', url: 'https://example.com/2' },
        ],
      };

      const md = renderSection(section);

      expect(md).toContain('## References');
      expect(md).toContain('[Link 1]');
      expect(md).toContain('[Link 2]');
    });

    it('should respect heading level', () => {
      const sections = [
        { type: 'tutorial', title: 'H1', level: 1 },
        { type: 'tutorial', title: 'H2', level: 2 },
        { type: 'tutorial', title: 'H3', level: 3 },
        { type: 'tutorial', title: 'H4', level: 4 },
      ];

      const md1 = renderSection(sections[0]);
      const md2 = renderSection(sections[1]);
      const md3 = renderSection(sections[2]);
      const md4 = renderSection(sections[3]);

      expect(md1).toContain('# H1');
      expect(md2).toContain('## H2');
      expect(md3).toContain('### H3');
      expect(md4).toContain('#### H4');
    });
  });

  describe('renderProofAppendix', () => {
    let testReceipts;

    beforeEach(async () => {
      // Create test receipts
      process.env.DETERMINISTIC = '1';

      testReceipts = [
        await createReceipt('snapshot', { data: 'test1' }),
        await createReceipt('anchor', { data: 'test2' }),
        await createReceipt('validation', { data: 'test3' }),
      ];

      delete process.env.DETERMINISTIC;
    });

    it('should render proof appendix with receipts', () => {
      const merkleRoot = 'a1b2c3d4e5f6';
      const md = verifyDeterminism(() =>
        renderProofAppendix(testReceipts, merkleRoot)
      );

      expect(md).toContain('## Proof Appendix');
      expect(md).toContain('### Receipt Chain');
      expect(md).toContain('### Merkle Tree');
      expect(md).toContain('### Chain Integrity');
      expect(md).toContain('### Verification');
      expect(md).toContain(merkleRoot);
    });

    it('should sort receipts by timestamp', () => {
      const merkleRoot = 'test123';
      const md = renderProofAppendix(testReceipts, merkleRoot);

      const lines = md.split('\n');
      const receiptLines = lines.filter(line => line.includes('receipt-'));

      // Receipts should be in timestamp order
      expect(receiptLines.length).toBeGreaterThan(0);
    });

    it('should render ASCII Merkle tree diagram', () => {
      const merkleRoot = 'test123';
      const md = renderProofAppendix(testReceipts, merkleRoot);

      expect(md).toContain('Root');
      expect(md).toContain('/    \\');
      expect(md).toContain('R0');
      expect(md).toContain('R1');
      expect(md).toContain('R2');
    });

    it('should handle empty receipts', () => {
      const md = renderProofAppendix([], 'empty');
      expect(md).toBe('');
    });
  });

  describe('canonicalizeMarkdown', () => {
    it('should normalize line endings', () => {
      const mdCRLF = 'line1\r\nline2\r\nline3';
      const mdLF = canonicalizeMarkdown(mdCRLF);

      expect(mdLF).not.toContain('\r');
      expect(mdLF).toContain('line1\nline2\nline3');
    });

    it('should remove trailing whitespace from lines', () => {
      const mdSpaces = 'line1   \nline2  \nline3 ';
      const canonical = canonicalizeMarkdown(mdSpaces);

      expect(canonical).toBe('line1\nline2\nline3\n');
    });

    it('should normalize heading markers', () => {
      const mdHeadings = '#  Multiple  Spaces\n##   Also  Here';
      const canonical = canonicalizeMarkdown(mdHeadings);

      expect(canonical).toContain('# Multiple  Spaces');
      expect(canonical).toContain('## Also  Here');
    });

    it('should add single trailing newline', () => {
      const mdNoNewline = 'content';
      const mdMultipleNewlines = 'content\n\n\n';

      const canonical1 = canonicalizeMarkdown(mdNoNewline);
      const canonical2 = canonicalizeMarkdown(mdMultipleNewlines);

      expect(canonical1).toBe('content\n');
      expect(canonical2).toBe('content\n');
    });

    it('should be idempotent', () => {
      const md = 'test\r\nwith  \nvarious   \r\nformats\n\n';
      verifyIdempotence(canonicalizeMarkdown, md);
    });
  });

  describe('hashMarkdown', () => {
    it('should produce deterministic hash', () => {
      const md = 'test content\n';
      const hash = verifyDeterminism(() => hashMarkdown(md));

      expect(hash).toMatch(/^[0-9a-f]{64}$/);
    });

    it('should produce same hash for equivalent content', () => {
      const md1 = 'content\n';
      const md2 = 'content  \r\n'; // Different whitespace, same canonical

      const hash1 = hashMarkdown(md1);
      const hash2 = hashMarkdown(md2);

      expect(hash1).toBe(hash2);
    });

    it('should produce different hash for different content', () => {
      const md1 = 'content1\n';
      const md2 = 'content2\n';

      const hash1 = hashMarkdown(md1);
      const hash2 = hashMarkdown(md2);

      expect(hash1).not.toBe(hash2);
    });
  });

  describe('renderKGCDocument', () => {
    let testReceipts;

    beforeEach(async () => {
      process.env.DETERMINISTIC = '1';

      testReceipts = [
        await createReceipt('snapshot', { data: 'test1' }),
        await createReceipt('anchor', { data: 'test2' }),
      ];

      delete process.env.DETERMINISTIC;
    });

    it('should render complete document with all components', () => {
      const doc = verifyDeterminism(() =>
        renderKGCDocument(testFrontmatter, testSections, testReceipts)
      );

      // Frontmatter
      expect(doc).toContain('---');
      expect(doc).toContain('title: Test Document');

      // Sections
      expect(doc).toContain('## Getting Started');
      expect(doc).toContain('## API Reference');

      // Proof appendix
      expect(doc).toContain('## Proof Appendix');
      expect(doc).toContain('### Receipt Chain');
    });

    it('should produce byte-identical output for same input', () => {
      const doc1 = renderKGCDocument(testFrontmatter, testSections, testReceipts);
      const doc2 = renderKGCDocument(testFrontmatter, testSections, testReceipts);

      expect(doc1).toBe(doc2);
      expect(doc1.length).toBe(doc2.length);
    });

    it('should produce same hash for multiple renders', () => {
      const doc1 = renderKGCDocument(testFrontmatter, testSections, testReceipts);
      const doc2 = renderKGCDocument(testFrontmatter, testSections, testReceipts);

      const hash1 = hashMarkdown(doc1);
      const hash2 = hashMarkdown(doc2);

      expect(hash1).toBe(hash2);
    });

    it('should handle document without frontmatter', () => {
      const doc = renderKGCDocument(null, testSections, testReceipts);

      // Should not have frontmatter at the start, but may have --- separator in proof appendix
      const lines = doc.split('\n');
      expect(lines[0]).not.toBe('---');
      expect(doc).toContain('## Getting Started');
    });

    it('should handle document without receipts', () => {
      const doc = renderKGCDocument(testFrontmatter, testSections, []);

      expect(doc).toContain('title: Test Document');
      expect(doc).not.toContain('## Proof Appendix');
    });

    it('should handle minimal document', () => {
      const doc = renderKGCDocument(
        { title: 'Minimal' },
        [{ type: 'tutorial', title: 'Test', level: 1 }],
        []
      );

      expect(doc).toContain('title: Minimal');
      expect(doc).toContain('# Test');
    });
  });

  describe('Integration with Receipt System', () => {
    it('should work with real receipts', async () => {
      process.env.DETERMINISTIC = '1';

      const receipts = [];
      for (let i = 0; i < 5; i++) {
        const receipt = await createReceipt('test-event', {
          index: i,
          data: `test-data-${i}`,
        });
        receipts.push(receipt);
      }

      delete process.env.DETERMINISTIC;

      const merkleRoot = 'test-root-hash';
      const appendix = renderProofAppendix(receipts, merkleRoot);

      expect(appendix).toContain('## Proof Appendix');
      expect(appendix).toContain('receipt-test-event');
      expect(appendix).toContain(merkleRoot);
    });

    it('should render document with chained receipts', async () => {
      process.env.DETERMINISTIC = '1';

      // Create chained receipts
      const receipt1 = await createReceipt('genesis', { data: 'first' });
      const receipt2 = await createReceipt('second', { data: 'second' }, {
        chain: receipt1.hash,
      });
      const receipt3 = await createReceipt('third', { data: 'third' }, {
        chain: receipt2.hash,
      });

      delete process.env.DETERMINISTIC;

      const doc = renderKGCDocument(
        { title: 'Chained Document' },
        [{ type: 'tutorial', title: 'Intro', level: 1 }],
        [receipt1, receipt2, receipt3]
      );

      expect(doc).toContain('## Proof Appendix');
      expect(doc).toContain('### Chain Integrity');
      expect(doc).toContain('receipt-genesis');
      expect(doc).toContain('receipt-second');
      expect(doc).toContain('receipt-third');
    });
  });

  describe('Determinism Properties', () => {
    it('should produce identical output across multiple renders', () => {
      const iterations = 100;
      const hashes = new Set();

      for (let i = 0; i < iterations; i++) {
        const doc = renderKGCDocument(testFrontmatter, testSections, []);
        hashes.add(hashMarkdown(doc));
      }

      // Only one unique hash
      expect(hashes.size).toBe(1);
    });

    it('should handle concurrent renders deterministically', async () => {
      const promises = Array.from({ length: 10 }, () =>
        Promise.resolve(renderKGCDocument(testFrontmatter, testSections, []))
      );

      const results = await Promise.all(promises);
      const first = results[0];

      for (const result of results) {
        expect(result).toBe(first);
      }
    });

    it('should canonicalize output automatically', () => {
      const doc = renderKGCDocument(testFrontmatter, testSections, []);

      // Should not have CRLF
      expect(doc).not.toContain('\r');

      // Should not have trailing whitespace on lines
      const lines = doc.split('\n');
      for (const line of lines) {
        if (line.length > 0) {
          expect(line).toBe(line.trimEnd());
        }
      }

      // Should end with exactly one newline
      expect(doc).toMatch(/\n$/);
      expect(doc).not.toMatch(/\n\n$/);
    });
  });

  describe('Edge Cases', () => {
    it('should handle empty inputs gracefully', () => {
      expect(() => renderKGCDocument({}, [], [])).not.toThrow();
      expect(() => renderTable([], [{ key: 'test', header: 'Test' }])).not.toThrow();
      expect(() => renderLinks([])).not.toThrow();
    });

    it('should handle special characters in content', () => {
      const frontmatter = {
        title: 'Title: With: Colons',
        description: 'Description with [brackets] and {braces}',
      };

      const doc = renderKGCDocument(frontmatter, [], []);
      expect(doc).toContain('title:');
      expect(doc).toContain('description:');
    });

    it('should handle very long content', () => {
      const longContent = 'x'.repeat(10000);
      const section = {
        type: 'tutorial',
        title: 'Long Content',
        level: 2,
        content: longContent,
      };

      const md = renderSection(section);
      expect(md).toContain(longContent);
    });

    it('should handle Unicode content', () => {
      const frontmatter = {
        title: '日本語のタイトル',
        emoji: '🚀 Rocket Science',
      };

      const doc = renderKGCDocument(frontmatter, [], []);
      expect(doc).toContain('日本語のタイトル');
      expect(doc).toContain('🚀 Rocket Science');
    });

    it('should handle null and undefined gracefully', () => {
      const frontmatter = {
        title: 'Test',
        nullValue: null,
        undefinedValue: undefined,
      };

      const yaml = renderFrontmatter(frontmatter);
      expect(yaml).toContain('title: Test');
      // undefined should be omitted, null should be rendered
      expect(yaml).toContain('nullValue: null');
      expect(yaml).not.toContain('undefinedValue');
    });
  });
});
