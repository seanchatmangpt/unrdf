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
      expect(ast.blocks).toHaveLength(2); // heading + query block
      expect(ast.blocks[0].type).toBe('heading');
      expect(ast.blocks[1].type).toBe('query');
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

      expect(result).toContain('# How to');
      expect(result).toContain('problem-solving');
    });

    it('should focus on task-oriented content', () => {
      const result = renderHowTo(sampleAST);

      expect(result).toContain('step');
      expect(result).toMatch(/how to|solve|accomplish/i);
    });
  });

  describe('renderReference', () => {
    it('should render reference view from AST', () => {
      const result = renderReference(sampleAST);

      expect(result).toContain('# Reference');
      expect(result).toContain('API');
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

      expect(result).toContain('why');
      expect(result).toContain('background');
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
      expect(result).toContain('o_hash linkage');
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
