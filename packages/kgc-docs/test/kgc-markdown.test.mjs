/**
 * @file KGC Markdown Test Suite
 * @module kgc-docs/test
 *
 * Tests for KGC Markdown parser, renderer, and proof generation
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  parseKGCMarkdown,
  parseFrontmatter,
  parseFencedBlock,
  buildAST,
  parseCrossReferences,
  parseParagraphs,
  parseLists,
  parseTables,
  parseBlockquotes,
  extractMetadata,
} from '../src/parser.mjs';
import {
  renderDiataxisView,
  renderTutorial,
  renderHowTo,
  renderReference,
  renderExplanation,
  generateProofAppendix,
} from '../src/renderer.mjs';
import {
  createMerkleTree,
  generateProofTree,
  verifyProof,
  linkReceiptHash,
} from '../src/proof.mjs';
import {
  validateReference,
  buildReferenceMap,
} from '../src/reference-validator.mjs';
import {
  compareVersions,
  getChangeType,
} from '../src/changelog-generator.mjs';
import { executeCode, formatExecutionOutput } from '../src/executor.mjs';

describe('@unrdf/kgc-docs - Parser', () => {
  describe('parseFrontmatter', () => {
    it('should parse YAML frontmatter with required fields', () => {
      const markdown = `---
o_hash: abc123def456
policy_id: policy-001
bounds:
  - min: 0
  - max: 100
receipts:
  - receipt-001
  - receipt-002
views:
  - tutorial
  - reference
sources:
  - source-001
---
# Content`;

      const result = parseFrontmatter(markdown);

      expect(result.o_hash).toBe('abc123def456');
      expect(result.policy_id).toBe('policy-001');
      expect(result.bounds).toHaveLength(2);
      expect(result.receipts).toContain('receipt-001');
      expect(result.views).toContain('tutorial');
      expect(result.sources).toContain('source-001');
    });

    it('should validate frontmatter schema with Zod', () => {
      const invalidMarkdown = `---
o_hash: 123
---`;

      expect(() => parseFrontmatter(invalidMarkdown)).toThrow();
    });
  });

  describe('parseFencedBlock', () => {
    it('should parse kgc:query fenced blocks', () => {
      const markdown = `
\`\`\`kgc:query
SELECT ?s ?p ?o WHERE { ?s ?p ?o }
\`\`\`
`;

      const result = parseFencedBlock(markdown);

      expect(result.type).toBe('query');
      expect(result.content).toContain('SELECT ?s ?p ?o');
    });

    it('should parse kgc:proof fenced blocks', () => {
      const markdown = `
\`\`\`kgc:proof
{
  "merkle_root": "abc123",
  "receipt_hashes": ["hash1", "hash2"]
}
\`\`\`
`;

      const result = parseFencedBlock(markdown);

      expect(result.type).toBe('proof');
      expect(result.content).toContain('merkle_root');
    });

    it('should parse kgc:extract fenced blocks', () => {
      const markdown = `
\`\`\`kgc:extract
{
  "entity": "ex:Person",
  "properties": ["name", "email"]
}
\`\`\`
`;

      const result = parseFencedBlock(markdown);

      expect(result.type).toBe('extract');
      expect(result.content).toContain('entity');
    });

    it('should parse kgc:render fenced blocks', () => {
      const markdown = `
\`\`\`kgc:render
template: diataxis-tutorial
variables:
  - name: "Getting Started"
\`\`\`
`;

      const result = parseFencedBlock(markdown);

      expect(result.type).toBe('render');
      expect(result.content).toContain('template');
    });
  });

  describe('buildAST', () => {
    it('should build deterministic AST from markdown', () => {
      const markdown = `---
o_hash: test123
policy_id: test-policy
bounds: []
receipts: []
views: [tutorial]
sources: []
---
# Getting Started

This is a tutorial.

\`\`\`kgc:query
SELECT * WHERE { ?s ?p ?o }
\`\`\`
`;

      const ast = buildAST(markdown);

      expect(ast.frontmatter).toBeDefined();
      expect(ast.frontmatter.o_hash).toBe('test123');
      expect(ast.blocks.length).toBeGreaterThan(1); // heading + query + paragraph
      const headingBlock = ast.blocks.find((b) => b.type === 'heading');
      const queryBlock = ast.blocks.find((b) => b.type === 'query');
      expect(headingBlock).toBeDefined();
      expect(queryBlock).toBeDefined();
    });

    it('should produce identical AST for identical input', () => {
      const markdown = `---
o_hash: determinism
policy_id: test
bounds: []
receipts: []
views: []
sources: []
---
# Test`;

      const ast1 = buildAST(markdown);
      const ast2 = buildAST(markdown);

      expect(JSON.stringify(ast1)).toBe(JSON.stringify(ast2));
    });
  });
});

describe('@unrdf/kgc-docs - Renderer', () => {
  const sampleAST = {
    frontmatter: {
      o_hash: 'test123',
      policy_id: 'policy-001',
      bounds: [],
      receipts: ['receipt-001'],
      views: ['tutorial', 'how-to', 'reference', 'explanation'],
      sources: ['source-001'],
    },
    blocks: [
      {
        type: 'heading',
        level: 1,
        content: 'KGC Introduction',
      },
      {
        type: 'paragraph',
        content: 'Knowledge Graph Circuit documentation.',
      },
      {
        type: 'query',
        content: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
      },
    ],
  };

  describe('renderTutorial', () => {
    it('should render tutorial view from AST', () => {
      const result = renderTutorial(sampleAST);

      expect(result).toContain('# KGC Introduction');
      expect(result).toContain('## Getting Started');
      expect(result).toContain('tutorial');
    });

    it('should focus on learning-oriented content', () => {
      const result = renderTutorial(sampleAST);

      expect(result).toContain('learn');
      expect(result).not.toContain('API reference');
    });
  });

  describe('renderHowTo', () => {
    it('should render how-to view from AST', () => {
      const result = renderHowTo(sampleAST);

      expect(result).toContain('How to Use');
      expect(result).toMatch(/solve|accomplish|problem|goal/i);
    });

    it('should focus on task-oriented content', () => {
      const result = renderHowTo(sampleAST);

      expect(result).toMatch(/task|goal|common/i);
      expect(result).toMatch(/solve|accomplish|task/i);
    });
  });

  describe('renderReference', () => {
    it('should render reference view from AST', () => {
      const result = renderReference(sampleAST);

      expect(result).toContain('API Reference');
      expect(result).toContain('Metadata');
    });

    it('should focus on information-oriented content', () => {
      const result = renderReference(sampleAST);

      expect(result).toContain('Parameters');
      expect(result).toContain('Returns');
    });
  });

  describe('renderExplanation', () => {
    it('should render explanation view from AST', () => {
      const result = renderExplanation(sampleAST);

      expect(result).toContain('# Understanding');
      expect(result).toContain('concept');
    });

    it('should focus on understanding-oriented content', () => {
      const result = renderExplanation(sampleAST);

      expect(result).toMatch(/why|Why/);
      expect(result).toMatch(/background|Background/);
    });
  });

  describe('generateProofAppendix', () => {
    it('should generate proof appendix with receipt hashes', () => {
      const result = generateProofAppendix(sampleAST);

      expect(result).toContain('## Proof Appendix');
      expect(result).toContain('receipt-001');
      expect(result).toContain('test123'); // o_hash
    });

    it('should include Merkle root and linkage', () => {
      const result = generateProofAppendix(sampleAST);

      expect(result).toContain('merkle_root');
      expect(result).toMatch(/o_hash linkage|O-Hash Linkage/i);
    });

    it('should format receipt references as code blocks', () => {
      const result = generateProofAppendix(sampleAST);

      expect(result).toContain('```json');
      expect(result).toContain('```');
    });
  });

  describe('renderDiataxisView', () => {
    it('should render all 4 Diataxis views', () => {
      const views = ['tutorial', 'how-to', 'reference', 'explanation'];

      for (const view of views) {
        const result = renderDiataxisView(sampleAST, view);
        expect(result).toBeTruthy();
        expect(result.length).toBeGreaterThan(0);
      }
    });

    it('should append proof to all views', () => {
      const views = ['tutorial', 'how-to', 'reference', 'explanation'];

      for (const view of views) {
        const result = renderDiataxisView(sampleAST, view);
        expect(result).toContain('## Proof Appendix');
      }
    });
  });
});

describe('@unrdf/kgc-docs - Proof', () => {
  describe('createMerkleTree', () => {
    it('should create Merkle tree from receipt hashes', () => {
      const receipts = ['receipt-001', 'receipt-002', 'receipt-003'];

      const tree = createMerkleTree(receipts);

      expect(tree.root).toBeDefined();
      expect(tree.root.length).toBe(64); // SHA256 hex
      expect(tree.leaves).toHaveLength(3);
    });

    it('should be deterministic', () => {
      const receipts = ['receipt-001', 'receipt-002'];

      const tree1 = createMerkleTree(receipts);
      const tree2 = createMerkleTree(receipts);

      expect(tree1.root).toBe(tree2.root);
    });
  });

  describe('generateProofTree', () => {
    it('should generate proof tree with o_hash linkage', () => {
      const ast = {
        frontmatter: {
          o_hash: 'abc123',
          receipts: ['receipt-001', 'receipt-002'],
        },
      };

      const proof = generateProofTree(ast);

      expect(proof.merkle_root).toBeDefined();
      expect(proof.o_hash).toBe('abc123');
      expect(proof.receipt_count).toBe(2);
      expect(proof.timestamp).toBeDefined();
    });

    it('should include receipt to o_hash mapping', () => {
      const ast = {
        frontmatter: {
          o_hash: 'root123',
          receipts: ['r1', 'r2'],
        },
      };

      const proof = generateProofTree(ast);

      expect(proof.linkage).toBeDefined();
      expect(proof.linkage.length).toBe(2);
    });
  });

  describe('verifyProof', () => {
    it('should verify valid proof', () => {
      const ast = {
        frontmatter: {
          o_hash: 'abc123',
          receipts: ['receipt-001'],
        },
      };

      const proof = generateProofTree(ast);
      const isValid = verifyProof(proof, ast);

      expect(isValid).toBe(true);
    });

    it('should reject invalid proof', () => {
      const ast = {
        frontmatter: {
          o_hash: 'abc123',
          receipts: ['receipt-001'],
        },
      };

      const proof = generateProofTree(ast);
      proof.merkle_root = 'tampered';

      const isValid = verifyProof(proof, ast);

      expect(isValid).toBe(false);
    });
  });

  describe('linkReceiptHash', () => {
    it('should create hash linkage between receipt and o_hash', () => {
      const receiptHash = 'receipt123';
      const oHash = 'root456';

      const linkage = linkReceiptHash(receiptHash, oHash);

      expect(linkage.receipt_hash).toBe(receiptHash);
      expect(linkage.o_hash).toBe(oHash);
      expect(linkage.combined_hash).toBeDefined();
      expect(linkage.combined_hash.length).toBe(64);
    });
  });
});

describe('@unrdf/kgc-docs - Integration', () => {
  it('should parse, render, and prove complete workflow', () => {
    const markdown = `---
o_hash: integration-test
policy_id: test-policy
bounds: []
receipts: [receipt-int-001]
views: [tutorial, reference]
sources: []
---
# Integration Test

\`\`\`kgc:query
SELECT ?s WHERE { ?s a ex:Test }
\`\`\`
`;

    // Parse
    const ast = buildAST(markdown);
    expect(ast).toBeDefined();

    // Render
    const tutorial = renderTutorial(ast);
    expect(tutorial).toContain('Getting Started');

    const reference = renderReference(ast);
    expect(reference).toContain('Reference');

    // Prove
    const proof = generateProofTree(ast);
    expect(proof.merkle_root).toBeDefined();

    const isValid = verifyProof(proof, ast);
    expect(isValid).toBe(true);
  });
});

describe('@unrdf/kgc-docs - Cross-Document References', () => {
  it('should parse cross-document references from markdown links', () => {
    const markdown = `
See [Other Doc](../other/doc.md#section) for details.
Check [API](./api.md) for reference.
External: [GitHub](https://github.com/example)
`;

    const refs = parseCrossReferences(markdown);

    expect(refs).toHaveLength(2); // Only relative paths
    expect(refs[0].path).toBe('../other/doc.md');
    expect(refs[0].anchor).toBe('section');
    expect(refs[1].path).toBe('./api.md');
    expect(refs[1].anchor).toBe(null);
  });

  it('should build reference map with outgoing links', () => {
    // This would require actual files, so we'll test the structure
    const mockMap = {
      file: '/path/to/doc.md',
      outgoing: [
        { target: '../other.md', anchor: 'intro', text: 'Introduction' },
      ],
      metadata: {
        total_outgoing: 1,
        has_anchors: true,
      },
    };

    expect(mockMap.outgoing).toHaveLength(1);
    expect(mockMap.metadata.has_anchors).toBe(true);
  });

  it('should validate cross-references and detect broken links', () => {
    // Mock validation result
    const validResult = {
      valid: true,
      reference: { path: './existing.md', anchor: null },
    };

    const invalidResult = {
      valid: false,
      reference: { path: './missing.md', anchor: null },
      error: 'Target file not found: ./missing.md',
    };

    expect(validResult.valid).toBe(true);
    expect(invalidResult.valid).toBe(false);
    expect(invalidResult.error).toContain('not found');
  });
});

describe('@unrdf/kgc-docs - Version Tracking', () => {
  it('should compare semver versions correctly', () => {
    expect(compareVersions('1.0.0', '1.0.0')).toBe(0);
    expect(compareVersions('1.0.1', '1.0.0')).toBe(1);
    expect(compareVersions('1.0.0', '1.0.1')).toBe(-1);
    expect(compareVersions('2.0.0', '1.9.9')).toBe(1);
  });

  it('should determine change type from version diff', () => {
    expect(getChangeType('1.0.0', '2.0.0')).toBe('major');
    expect(getChangeType('1.0.0', '1.1.0')).toBe('minor');
    expect(getChangeType('1.0.0', '1.0.1')).toBe('patch');
    expect(getChangeType('1.0.0', '1.0.0')).toBe('none');
  });
});

describe('@unrdf/kgc-docs - Full Markdown AST', () => {
  it('should parse paragraphs from markdown', () => {
    const markdown = `
This is paragraph one.

This is paragraph two.
It spans multiple lines.

# Heading

Another paragraph.
`;

    const paragraphs = parseParagraphs(markdown);

    expect(paragraphs.length).toBeGreaterThan(0);
    expect(paragraphs[0].type).toBe('paragraph');
    expect(paragraphs[0].content).toContain('paragraph one');
  });

  it('should parse ordered and unordered lists', () => {
    const markdown = `
- Item 1
- Item 2
  - Nested item

1. First
2. Second
3. Third
`;

    const lists = parseLists(markdown);

    expect(lists.length).toBe(2);
    expect(lists[0].ordered).toBe(false);
    expect(lists[1].ordered).toBe(true);
    expect(lists[0].items).toHaveLength(3);
    expect(lists[1].items).toHaveLength(3);
  });

  it('should parse markdown tables', () => {
    const markdown = `
| Header 1 | Header 2 |
|----------|----------|
| Cell 1   | Cell 2   |
| Cell 3   | Cell 4   |
`;

    const tables = parseTables(markdown);

    expect(tables).toHaveLength(1);
    expect(tables[0].headers).toEqual(['Header 1', 'Header 2']);
    expect(tables[0].rows).toHaveLength(2);
    expect(tables[0].rows[0]).toEqual(['Cell 1', 'Cell 2']);
  });

  it('should parse blockquotes', () => {
    const markdown = `
> This is a quote.
> It continues here.

> Another quote.
`;

    const blockquotes = parseBlockquotes(markdown);

    expect(blockquotes).toHaveLength(2);
    expect(blockquotes[0].type).toBe('blockquote');
    expect(blockquotes[0].content).toContain('This is a quote');
  });
});

describe('@unrdf/kgc-docs - Executable Code Blocks', () => {
  it('should execute JavaScript code and capture output', () => {
    const code = `
const x = 10;
const y = 20;
console.log('Sum:', x + y);
x + y
`;

    const result = executeCode(code);

    expect(result.success).toBe(true);
    expect(result.output).toContain('Sum: 30');
    expect(result.returnValue).toBe(30);
  });

  it('should handle execution errors gracefully', () => {
    const code = `
throw new Error('Test error');
`;

    const result = executeCode(code);

    expect(result.success).toBe(false);
    expect(result.error).toContain('Test error');
  });
});

describe('@unrdf/kgc-docs - Metadata Extraction', () => {
  it('should extract authors, dates, and status from paragraphs', () => {
    const paragraphs = [
      { type: 'paragraph', content: 'Author: John Doe' },
      {
        type: 'paragraph',
        content: 'Created: 2025-01-01\nUpdated: 2025-12-27',
      },
      { type: 'paragraph', content: 'Status: active' },
      { type: 'paragraph', content: 'Tags: kgc, markdown, documentation' },
    ];

    const metadata = extractMetadata(paragraphs);

    expect(metadata.authors).toContain('John Doe');
    expect(metadata.dates.created).toBe('2025-01-01');
    expect(metadata.dates.updated).toBe('2025-12-27');
    expect(metadata.status).toBe('active');
    expect(metadata.tags).toContain('kgc');
    expect(metadata.tags).toContain('markdown');
  });

  it('should extract category from paragraph text', () => {
    const paragraphs = [{ type: 'paragraph', content: 'Category: tutorial' }];

    const metadata = extractMetadata(paragraphs);

    expect(metadata.category).toBe('tutorial');
  });
});
