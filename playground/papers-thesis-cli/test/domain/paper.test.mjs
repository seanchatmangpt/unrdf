/**
 * @fileoverview Paper domain model tests
 *
 * @description
 * Unit tests for paper model, validation, and factory functions.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createPaper,
  PaperSchema,
  CreatePaperInputSchema,
  listPaperFamilies,
  paperToRdf,
  PaperFamily,
  PAPER_FAMILY_CONFIG
} from '../../src/domain/models/paper.mjs';

describe('Paper Model', () => {
  describe('createPaper', () => {
    it('should create a valid IMRAD paper with minimal input', () => {
      const paper = createPaper({
        title: 'Test Paper',
        authors: [{ name: 'Alice' }]
      });

      expect(paper).toBeDefined();
      expect(paper.title).toBe('Test Paper');
      expect(paper.family).toBe('imrad');
      expect(paper.authors).toHaveLength(1);
      expect(paper.authors[0].name).toBe('Alice');
      expect(paper.sections).toHaveLength(5); // IMRAD has 5 sections
    });

    it('should create a DSR paper with all sections', () => {
      const paper = createPaper({
        family: 'dsr',
        title: 'DSR Paper',
        authors: [{ name: 'Bob', affiliation: 'MIT' }]
      });

      expect(paper.family).toBe('dsr');
      expect(paper.sections).toHaveLength(6); // DSR has 6 sections
      expect(paper.sections[0].heading).toBe('Problem Identification');
    });

    it('should use custom sections when provided', () => {
      const paper = createPaper({
        title: 'Custom Paper',
        authors: [{ name: 'Carol' }],
        customSections: [
          { heading: 'Custom Section 1' },
          { heading: 'Custom Section 2', content: 'Some content' }
        ]
      });

      expect(paper.sections).toHaveLength(2);
      expect(paper.sections[0].heading).toBe('Custom Section 1');
      expect(paper.sections[1].content).toBe('Some content');
    });

    it('should set timestamps on creation', () => {
      const paper = createPaper({
        title: 'Test',
        authors: [{ name: 'Test Author' }]
      });

      expect(paper.createdAt).toBeDefined();
      expect(paper.lastModified).toBeDefined();
      expect(new Date(paper.createdAt)).toBeInstanceOf(Date);
    });

    it('should generate unique IDs', () => {
      const paper1 = createPaper({ title: 'Paper 1', authors: [{ name: 'A' }] });
      const paper2 = createPaper({ title: 'Paper 2', authors: [{ name: 'B' }] });

      expect(paper1.id).not.toBe(paper2.id);
    });
  });

  describe('PaperSchema validation', () => {
    it('should validate a complete paper object', () => {
      const paper = {
        id: 'paper-123',
        family: 'imrad',
        title: 'Valid Paper',
        abstract: 'This is abstract',
        keywords: ['test', 'paper'],
        authors: [{ name: 'Test', affiliation: 'Uni', role: 'primary' }],
        sections: [{ heading: 'Intro', content: '', order: 1, level: 1 }],
        createdAt: new Date().toISOString()
      };

      const result = PaperSchema.safeParse(paper);
      expect(result.success).toBe(true);
    });

    it('should reject paper without title', () => {
      const paper = {
        id: 'paper-123',
        family: 'imrad',
        authors: [{ name: 'Test', role: 'primary' }],
        sections: [],
        createdAt: new Date().toISOString()
      };

      const result = PaperSchema.safeParse(paper);
      expect(result.success).toBe(false);
    });

    it('should reject invalid paper family', () => {
      const paper = {
        id: 'paper-123',
        family: 'invalid',
        title: 'Test',
        authors: [{ name: 'Test', role: 'primary' }],
        sections: [],
        createdAt: new Date().toISOString()
      };

      const result = PaperSchema.safeParse(paper);
      expect(result.success).toBe(false);
    });
  });

  describe('CreatePaperInputSchema validation', () => {
    it('should validate minimal input', () => {
      const input = {
        title: 'Test',
        authors: [{ name: 'Author' }]
      };

      const result = CreatePaperInputSchema.safeParse(input);
      expect(result.success).toBe(true);
    });

    it('should require at least one author', () => {
      const input = {
        title: 'Test',
        authors: []
      };

      const result = CreatePaperInputSchema.safeParse(input);
      expect(result.success).toBe(false);
    });
  });

  describe('listPaperFamilies', () => {
    it('should return all paper families', () => {
      const families = listPaperFamilies();

      expect(families).toHaveLength(4);
      expect(families.map(f => f.id)).toContain('imrad');
      expect(families.map(f => f.id)).toContain('dsr');
      expect(families.map(f => f.id)).toContain('argument');
      expect(families.map(f => f.id)).toContain('contribution');
    });

    it('should include sections for each family', () => {
      const families = listPaperFamilies();

      for (const family of families) {
        expect(family.sections).toBeDefined();
        expect(Array.isArray(family.sections)).toBe(true);
        expect(family.sections.length).toBeGreaterThan(0);
      }
    });
  });

  describe('paperToRdf', () => {
    it('should convert paper to RDF-compatible object', () => {
      const paper = createPaper({
        title: 'RDF Test Paper',
        authors: [{ name: 'RDF Author', affiliation: 'RDF Uni' }],
        abstract: 'Test abstract'
      });

      const rdf = paperToRdf(paper);

      expect(rdf['@id']).toContain(paper.id);
      expect(rdf['@type']).toContain('IMRADPaper');
      expect(rdf['http://papers-thesis.org/ontology#hasTitle']).toBe(paper.title);
    });
  });

  describe('PaperFamily enum', () => {
    it('should have all family values', () => {
      expect(PaperFamily.IMRAD).toBe('imrad');
      expect(PaperFamily.DSR).toBe('dsr');
      expect(PaperFamily.ARGUMENT).toBe('argument');
      expect(PaperFamily.CONTRIBUTION).toBe('contribution');
    });
  });

  describe('PAPER_FAMILY_CONFIG', () => {
    it('should have configuration for all families', () => {
      expect(PAPER_FAMILY_CONFIG.imrad).toBeDefined();
      expect(PAPER_FAMILY_CONFIG.dsr).toBeDefined();
      expect(PAPER_FAMILY_CONFIG.argument).toBeDefined();
      expect(PAPER_FAMILY_CONFIG.contribution).toBeDefined();
    });

    it('should have name, description, and sections for each family', () => {
      for (const [key, config] of Object.entries(PAPER_FAMILY_CONFIG)) {
        expect(config.name).toBeDefined();
        expect(config.description).toBeDefined();
        expect(config.sections).toBeDefined();
        expect(Array.isArray(config.sections)).toBe(true);
      }
    });
  });
});
