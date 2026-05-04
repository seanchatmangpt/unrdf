/**
 * @file Tests for KGC Diataxis Documentation Projection System
 */

import { describe, it, expect } from 'vitest';
import {
  projectToTutorial,
  projectToHowTo,
  projectToReference,
  projectToExplanation,
  projectToAllViews,
  unifyFrontmatter,
  diataxisProjectionMatrix,
  validateProjection,
  serializeProjection,
  parseSourceDoc,
} from '../src/kgc-docs-diataxis.mjs';

// =============================================================================
// Test Fixtures
// =============================================================================

const canonicalMarkdown = `---
title: KGC Store Operations
description: Learn how to work with KGC-4D time-traveling triple stores
o_hash: abc123def456
policy_id: pol_789xyz
receipts:
  - rcpt_001
  - rcpt_002
views:
  - canonical
createdAt: 2024-01-01T00:00:00.000Z
lastProved: 2024-01-02T00:00:00.000Z
tags:
  - kgc
  - triple-store
  - time-travel
prerequisites:
  - Basic JavaScript knowledge
  - Understanding of RDF triples
  - Node.js installed
---

## Overview

KGC-4D provides a time-traveling triple store that allows you to query your knowledge graph at any point in its history. This is essential for forensic analysis, debugging, and understanding how your data evolved.

## Getting Started

To begin working with KGC stores, follow these steps:

1. Import the KGCStore class from @unrdf/kgc-4d
2. Create a new store instance with optional Git backbone
3. Add triples using the standard RDF quad format
4. Freeze the universe to create snapshots
5. Query historical states using time-travel

## Installation

\`\`\`bash
pnpm add @unrdf/kgc-4d
\`\`\`

## Basic Usage

\`\`\`javascript
import { KGCStore, freezeUniverse } from '@unrdf/kgc-4d';

// Create store
const store = new KGCStore({ enableGit: true });

// Add triple
await store.add({
  subject: 'http://example.org/alice',
  predicate: 'http://xmlns.com/foaf/0.1/name',
  object: '"Alice"',
  graph: 'http://example.org/graph1'
});

// Freeze universe (create snapshot)
const snapshot = await freezeUniverse(store);
console.log('Snapshot hash:', snapshot.hash);
\`\`\`

## Advanced Features

### Time-Travel Queries

You can query the store at any historical point:

\`\`\`javascript
import { reconstructState } from '@unrdf/kgc-4d';

// Reconstruct state at specific time
const historicalState = await reconstructState(store, '2024-01-01T12:00:00Z');
\`\`\`

### Git Integration

The Git backbone provides persistent snapshots:

\`\`\`javascript
import { GitBackbone } from '@unrdf/kgc-4d';

const backbone = new GitBackbone({ repoPath: './kgc-snapshots' });
await backbone.initialize();
await backbone.snapshot(store, 'Added user Alice');
\`\`\`

## Design Rationale

KGC-4D uses a 4-dimensional coordinate system (workflow, time, Git, HDIT) to track every change to the knowledge graph. This design was chosen because:

- Workflow dimension: Captures logical phases of data processing
- Time dimension: Precise nanosecond timestamps for causal ordering
- Git dimension: Persistent snapshots with diff capabilities
- HDIT dimension: High-dimensional information tracking for ML features

This multi-dimensional approach enables forensic analysis that would be impossible with traditional triple stores.

## Common Issues

### Issue: Store fills up memory

Solution: Use the freezeUniverse() function regularly to create snapshots and clear working memory.

### Issue: Git snapshots are slow

Solution: Adjust the snapshot frequency or use enableGit: false for non-critical data.

### Issue: Time-travel queries return unexpected results

Solution: Ensure you're using nanosecond precision timestamps. Use the now() function from @unrdf/kgc-4d.

## Alternatives

Traditional triple stores like Oxigraph, N3, or RDFLib don't provide time-travel capabilities. If you don't need historical queries, those stores may be more performant for simple SPARQL queries.

## Tradeoffs

- Memory overhead: KGC-4D uses more memory to track change history
- Write performance: Slightly slower writes due to coordinate tracking
- Complexity: More complex API than simple triple stores

The benefits of forensic analysis and debugging typically outweigh these costs in production knowledge graph applications.
`;

const apiManifest = {
  name: '@unrdf/kgc-4d',
  version: '1.0.0',
  functions: [
    {
      name: 'freezeUniverse',
      signature: 'freezeUniverse(store: KGCStore, metadata?: Object): Promise<Snapshot>',
      description: 'Create an immutable snapshot of the current universe state',
      parameters: [
        {
          name: 'store',
          type: 'KGCStore',
          required: true,
          description: 'The KGC store to snapshot',
        },
        {
          name: 'metadata',
          type: 'Object',
          required: false,
          defaultValue: '{}',
          description: 'Optional metadata to attach to snapshot',
        },
      ],
      returns: {
        type: 'Promise<Snapshot>',
        description: 'Snapshot object with hash and timestamp',
      },
      throws: [
        'SnapshotError if store is in invalid state',
      ],
      examples: [
        'const snapshot = await freezeUniverse(store, { author: "alice" });',
      ],
    },
    {
      name: 'reconstructState',
      signature: 'reconstructState(store: KGCStore, timestamp: string): Promise<KGCStore>',
      description: 'Reconstruct store state at a specific point in time',
      parameters: [
        {
          name: 'store',
          type: 'KGCStore',
          required: true,
          description: 'The KGC store to query',
        },
        {
          name: 'timestamp',
          type: 'string',
          required: true,
          description: 'ISO 8601 timestamp',
        },
      ],
      returns: {
        type: 'Promise<KGCStore>',
        description: 'New store instance with historical state',
      },
      examples: [
        'const past = await reconstructState(store, "2024-01-01T00:00:00Z");',
      ],
    },
  ],
  classes: [
    {
      name: 'KGCStore',
      description: 'Time-traveling triple store with Git backbone',
      methods: [
        { name: 'add', description: 'Add a quad to the store' },
        { name: 'delete', description: 'Remove a quad from the store' },
        { name: 'match', description: 'Query quads matching pattern' },
      ],
      properties: [
        { name: 'size', type: 'number', description: 'Number of quads in store' },
        { name: 'enableGit', type: 'boolean', description: 'Whether Git backbone is enabled' },
      ],
    },
  ],
  constants: [
    {
      name: 'GRAPHS',
      type: 'Object',
      description: 'Named graph URIs for KGC system graphs',
    },
    {
      name: 'EVENT_TYPES',
      type: 'Object',
      description: 'Standard event type identifiers',
    },
  ],
};

// =============================================================================
// Projection Tests
// =============================================================================

describe('KGC Diataxis Projection System', () => {
  describe('parseSourceDoc', () => {
    it('should parse markdown with frontmatter', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);

      expect(source.frontmatter.title).toBe('KGC Store Operations');
      expect(source.frontmatter.o_hash).toBe('abc123def456');
      expect(source.frontmatter.policy_id).toBe('pol_789xyz');
      expect(source.frontmatter.receipts).toEqual(['rcpt_001', 'rcpt_002']);
      expect(source.frontmatter.prerequisites).toContain('Basic JavaScript knowledge');
      expect(source.content).toContain('KGC-4D provides a time-traveling triple store');
    });
  });

  describe('projectToTutorial', () => {
    it('should create tutorial projection with encouraging tone', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToTutorial(source, apiManifest);

      expect(result.view).toBe('tutorial');
      expect(result.frontmatter.views).toEqual(['tutorial']);
      expect(result.frontmatter.difficulty).toBe('beginner');
      expect(result.frontmatter.title).toContain('Tutorial');

      // Should have tutorial sections
      expect(result.content).toContain('Why This Matters');
      expect(result.content).toContain('Before You Begin');
      expect(result.content).toContain('Step-by-Step Guide');
      expect(result.content).toContain('Complete Working Example');

      // Should have encouraging tone
      expect(result.content.toLowerCase()).toMatch(/let's|you'll|tip:|💡/);

      // Should preserve provenance
      expect(result.frontmatter.o_hash).toBe('abc123def456');
      expect(result.frontmatter.policy_id).toBe('pol_789xyz');
      expect(result.frontmatter.receipts).toEqual(['rcpt_001', 'rcpt_002']);
    });

    it('should include code examples', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToTutorial(source, apiManifest);

      expect(result.content).toContain('```');
      expect(result.content).toContain('KGCStore');
    });
  });

  describe('projectToHowTo', () => {
    it('should create how-to projection with problem-solution structure', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToHowTo(source, apiManifest);

      expect(result.view).toBe('howto');
      expect(result.frontmatter.views).toEqual(['howto']);
      expect(result.frontmatter.title).toContain('How to');

      // Should have how-to sections
      expect(result.content).toContain('Problem');
      expect(result.content).toContain('Solution');

      // Should preserve provenance
      expect(result.frontmatter.o_hash).toBe('abc123def456');
      expect(result.frontmatter.createdAt).toBe('2024-01-01T00:00:00.000Z');
    });

    it('should include troubleshooting section', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToHowTo(source, apiManifest);

      expect(result.content).toContain('Troubleshooting');
    });
  });

  describe('projectToReference', () => {
    it('should create reference projection with API tables', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToReference(source, apiManifest);

      expect(result.view).toBe('reference');
      expect(result.frontmatter.views).toEqual(['reference']);
      expect(result.frontmatter.title).toContain('Reference');

      // Should have reference sections
      expect(result.content).toContain('API Reference');
      expect(result.content).toContain('Functions');

      // Should have API tables
      expect(result.content).toContain('|');
      expect(result.content).toContain('freezeUniverse');
      expect(result.content).toContain('reconstructState');

      // Should preserve provenance
      expect(result.frontmatter.lastProved).toBe('2024-01-02T00:00:00.000Z');
    });

    it('should include parameter tables', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToReference(source, apiManifest);

      expect(result.content).toContain('Parameters:');
      expect(result.content).toContain('| Name | Type | Required | Default | Description |');
    });

    it('should sort functions alphabetically', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToReference(source, apiManifest);

      const freezeIndex = result.content.indexOf('freezeUniverse');
      const reconstructIndex = result.content.indexOf('reconstructState');

      // Functions should be in alphabetical order
      expect(freezeIndex).toBeLessThan(reconstructIndex);
    });
  });

  describe('projectToExplanation', () => {
    it('should create explanation projection with conceptual focus', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToExplanation(source, apiManifest);

      expect(result.view).toBe('explanation');
      expect(result.frontmatter.views).toEqual(['explanation']);
      expect(result.frontmatter.title).toContain('Understanding');

      // Should have explanation sections
      expect(result.content).toContain('Core Concepts');
      expect(result.content).toContain('Design Rationale');

      // Should focus on concepts, not code
      expect(result.content).toContain('4-dimensional coordinate system');
      expect(result.content).toContain('Workflow dimension');

      // Should preserve provenance
      expect(result.frontmatter.o_hash).toBe('abc123def456');
    });

    it('should include tradeoffs', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToExplanation(source, apiManifest);

      expect(result.content).toContain('Tradeoffs');
      expect(result.content).toContain('Memory overhead');
    });
  });

  describe('projectToAllViews', () => {
    it('should create all 4 projections with validation', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const results = projectToAllViews(source, apiManifest);

      expect(results.tutorial.view).toBe('tutorial');
      expect(results.howto.view).toBe('howto');
      expect(results.reference.view).toBe('reference');
      expect(results.explanation.view).toBe('explanation');

      // All should have validation results
      expect(results.tutorial.validation.valid).toBe(true);
      expect(results.howto.validation.valid).toBe(true);
      expect(results.reference.validation.valid).toBe(true);
      expect(results.explanation.validation.valid).toBe(true);
    });

    it('should preserve same o_hash across all views', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const results = projectToAllViews(source, apiManifest);

      const oHash = 'abc123def456';
      expect(results.tutorial.frontmatter.o_hash).toBe(oHash);
      expect(results.howto.frontmatter.o_hash).toBe(oHash);
      expect(results.reference.frontmatter.o_hash).toBe(oHash);
      expect(results.explanation.frontmatter.o_hash).toBe(oHash);
    });

    it('should preserve same receipt chain across all views', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const results = projectToAllViews(source, apiManifest);

      const receipts = ['rcpt_001', 'rcpt_002'];
      expect(results.tutorial.frontmatter.receipts).toEqual(receipts);
      expect(results.howto.frontmatter.receipts).toEqual(receipts);
      expect(results.reference.frontmatter.receipts).toEqual(receipts);
      expect(results.explanation.frontmatter.receipts).toEqual(receipts);
    });
  });

  describe('unifyFrontmatter', () => {
    it('should preserve immutable provenance fields', () => {
      const base = {
        title: 'Test',
        o_hash: 'hash123',
        policy_id: 'pol_456',
        receipts: ['rcpt_1', 'rcpt_2'],
        createdAt: '2024-01-01T00:00:00Z',
        lastProved: '2024-01-02T00:00:00Z',
        views: ['canonical'],
      };

      const unified = unifyFrontmatter(base, 'tutorial');

      expect(unified.o_hash).toBe('hash123');
      expect(unified.policy_id).toBe('pol_456');
      expect(unified.receipts).toEqual(['rcpt_1', 'rcpt_2']);
      expect(unified.createdAt).toBe('2024-01-01T00:00:00Z');
      expect(unified.lastProved).toBe('2024-01-02T00:00:00Z');
      expect(unified.views).toEqual(['tutorial']);
    });
  });

  describe('diataxisProjectionMatrix', () => {
    it('should return transformation rules for all 4 views', () => {
      const matrix = diataxisProjectionMatrix();

      expect(matrix.tutorial).toBeDefined();
      expect(matrix.howto).toBeDefined();
      expect(matrix.reference).toBeDefined();
      expect(matrix.explanation).toBeDefined();

      expect(matrix.tutorial.tone).toBe('encouraging, forgiving');
      expect(matrix.howto.tone).toBe('direct, practical');
      expect(matrix.reference.tone).toBe('formal, precise');
      expect(matrix.explanation.tone).toBe('reflective, contextual');
    });

    it('should define sections for each view', () => {
      const matrix = diataxisProjectionMatrix();

      expect(matrix.tutorial.sections).toContain('step-by-step guide');
      expect(matrix.howto.sections).toContain('solution (code + explanation)');
      expect(matrix.reference.sections).toContain('API tables (summary)');
      expect(matrix.explanation.sections).toContain('design rationale');
    });

    it('should define audience and goal for each view', () => {
      const matrix = diataxisProjectionMatrix();

      expect(matrix.tutorial.audience).toBe('beginners');
      expect(matrix.howto.audience).toBe('practitioners with basic knowledge');
      expect(matrix.reference.audience).toBe('developers looking up specifics');
      expect(matrix.explanation.audience).toBe('developers seeking deeper understanding');

      expect(matrix.tutorial.goal).toContain('understanding');
      expect(matrix.howto.goal).toContain('solving');
      expect(matrix.reference.goal).toContain('finding');
      expect(matrix.explanation.goal).toContain('understanding');
    });
  });

  describe('validateProjection', () => {
    it('should validate tutorial has encouraging tone', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToTutorial(source, apiManifest);
      const validation = validateProjection(source, 'tutorial', result);

      expect(validation.valid).toBe(true);
      expect(validation.warnings.length).toBeGreaterThanOrEqual(0);
    });

    it('should warn if required sections are missing', () => {
      const source = parseSourceDoc('---\ntitle: Test\nviews:\n  - canonical\n---\n\nSome content', null);
      const result = projectToTutorial(source, null);
      const validation = validateProjection(source, 'tutorial', result);

      // Should have warnings about missing sections
      expect(validation.warnings.some(w => w.includes('section'))).toBe(true);
    });

    it('should error if provenance fields are not preserved', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToTutorial(source, apiManifest);

      // Tamper with o_hash
      result.frontmatter.o_hash = 'different_hash';

      const validation = validateProjection(source, 'tutorial', result);

      expect(validation.valid).toBe(false);
      expect(validation.errors).toBeDefined();
      expect(validation.errors.some(e => e.includes('o_hash'))).toBe(true);
    });
  });

  describe('serializeProjection', () => {
    it('should serialize projection to complete markdown', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToTutorial(source, apiManifest);
      const markdown = serializeProjection(result);

      expect(markdown).toContain('---');
      expect(markdown).toContain('title:');
      expect(markdown).toContain('views:');
      expect(markdown).toContain('  - tutorial');
      expect(markdown).toContain('o_hash: abc123def456');
      expect(markdown).toContain('Why This Matters');
    });

    it('should round-trip parse and serialize', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToTutorial(source, apiManifest);
      const markdown = serializeProjection(result);

      const reparsed = parseSourceDoc(markdown, apiManifest);

      expect(reparsed.frontmatter.title).toBe(result.frontmatter.title);
      expect(reparsed.frontmatter.o_hash).toBe(result.frontmatter.o_hash);
      expect(reparsed.frontmatter.views).toEqual(result.frontmatter.views);
    });
  });

  describe('Content Quality', () => {
    it('tutorial should have working code examples', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToTutorial(source, apiManifest);

      expect(result.content).toContain('```javascript');
      expect(result.content).toContain('KGCStore');
      expect(result.content).toContain('freezeUniverse');
    });

    it('how-to should have practical solutions', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToHowTo(source, apiManifest);

      expect(result.content).toContain('Solution');
      expect(result.content).toContain('```');
    });

    it('reference should have structured API docs', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToReference(source, apiManifest);

      expect(result.content).toContain('| Function | Description | Returns |');
      expect(result.content).toContain('freezeUniverse');
      expect(result.content).toContain('Parameters:');
    });

    it('explanation should focus on concepts not code', () => {
      const source = parseSourceDoc(canonicalMarkdown, apiManifest);
      const result = projectToExplanation(source, apiManifest);

      // Should have more conceptual words than code blocks
      const conceptWords = (result.content.match(/concept|design|rationale|tradeoff|dimension/gi) || []).length;
      const codeBlocks = (result.content.match(/```/g) || []).length / 2;

      expect(conceptWords).toBeGreaterThan(codeBlocks);
    });
  });
});
