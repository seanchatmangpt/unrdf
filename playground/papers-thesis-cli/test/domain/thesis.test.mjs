/**
 * @fileoverview Thesis domain model tests
 *
 * @description
 * Unit tests for thesis model, validation, and factory functions.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createThesis,
  ThesisSchema,
  CreateThesisInputSchema,
  listThesisTypes,
  thesisToRdf,
  addMilestone,
  updateMilestoneStatus,
  calculateProgress,
  ThesisType,
  DegreeType,
  THESIS_TYPE_CONFIG
} from '../../src/domain/models/thesis.mjs';

describe('Thesis Model', () => {
  describe('createThesis', () => {
    it('should create a valid monograph thesis with minimal input', () => {
      const thesis = createThesis({
        title: 'Test Thesis',
        author: { name: 'Alice' }
      });

      expect(thesis).toBeDefined();
      expect(thesis.title).toBe('Test Thesis');
      expect(thesis.type).toBe('monograph');
      expect(thesis.author.name).toBe('Alice');
      expect(thesis.chapters).toHaveLength(6); // Monograph has 6 chapters
      expect(thesis.degree).toBe('PhD');
    });

    it('should create a narrative thesis with all chapters', () => {
      const thesis = createThesis({
        type: 'narrative',
        title: 'Narrative Thesis',
        author: { name: 'Bob' }
      });

      expect(thesis.type).toBe('narrative');
      expect(thesis.chapters).toHaveLength(5); // Narrative has 5 chapters
      expect(thesis.chapters[0].heading).toBe('Prologue');
    });

    it('should create a contribution thesis', () => {
      const thesis = createThesis({
        type: 'contribution',
        title: 'Publication-based Thesis',
        author: { name: 'Carol' }
      });

      expect(thesis.type).toBe('contribution');
      expect(thesis.chapters).toHaveLength(6);
      expect(thesis.chapters.some(c => c.heading.includes('Paper'))).toBe(true);
    });

    it('should include supervisor when provided', () => {
      const thesis = createThesis({
        title: 'Test',
        author: { name: 'Student' },
        supervisor: { name: 'Dr. Supervisor' }
      });

      expect(thesis.supervisor).toBeDefined();
      expect(thesis.supervisor.name).toBe('Dr. Supervisor');
      expect(thesis.supervisor.role).toBe('primary');
    });

    it('should include institution and department', () => {
      const thesis = createThesis({
        title: 'Test',
        author: { name: 'Student' },
        institution: 'MIT',
        department: 'Computer Science'
      });

      expect(thesis.institution).toBe('MIT');
      expect(thesis.department).toBe('Computer Science');
    });

    it('should use custom chapters when provided', () => {
      const thesis = createThesis({
        title: 'Custom Thesis',
        author: { name: 'Author' },
        customChapters: [
          { heading: 'Custom Chapter 1' },
          { heading: 'Custom Chapter 2', content: 'Content' }
        ]
      });

      expect(thesis.chapters).toHaveLength(2);
      expect(thesis.chapters[0].heading).toBe('Custom Chapter 1');
    });

    it('should initialize empty schedule', () => {
      const thesis = createThesis({
        title: 'Test',
        author: { name: 'Author' }
      });

      expect(thesis.schedule).toBeDefined();
      expect(thesis.schedule.milestones).toEqual([]);
      expect(thesis.schedule.defenseDate).toBeUndefined();
    });

    it('should include schedule when provided', () => {
      const thesis = createThesis({
        title: 'Test',
        author: { name: 'Author' },
        schedule: {
          defenseDate: '2025-06-15',
          milestones: [
            { name: 'Draft', date: '2025-03-01' }
          ]
        }
      });

      expect(thesis.schedule.defenseDate).toBe('2025-06-15');
      expect(thesis.schedule.milestones).toHaveLength(1);
    });
  });

  describe('ThesisSchema validation', () => {
    it('should validate a complete thesis object', () => {
      const thesis = {
        id: 'thesis-123',
        type: 'monograph',
        title: 'Valid Thesis',
        abstract: 'Abstract',
        author: { name: 'Author', affiliation: 'Uni', role: 'primary' },
        degree: 'PhD',
        chapters: [{ heading: 'Intro', content: '', order: 1, completion: 0 }],
        schedule: { milestones: [] },
        createdAt: new Date().toISOString()
      };

      const result = ThesisSchema.safeParse(thesis);
      expect(result.success).toBe(true);
    });

    it('should reject thesis without author', () => {
      const thesis = {
        id: 'thesis-123',
        type: 'monograph',
        title: 'Test',
        chapters: [],
        schedule: { milestones: [] },
        createdAt: new Date().toISOString()
      };

      const result = ThesisSchema.safeParse(thesis);
      expect(result.success).toBe(false);
    });
  });

  describe('listThesisTypes', () => {
    it('should return all thesis types', () => {
      const types = listThesisTypes();

      expect(types).toHaveLength(3);
      expect(types.map(t => t.id)).toContain('monograph');
      expect(types.map(t => t.id)).toContain('narrative');
      expect(types.map(t => t.id)).toContain('contribution');
    });

    it('should include chapters for each type', () => {
      const types = listThesisTypes();

      for (const type of types) {
        expect(type.chapters).toBeDefined();
        expect(Array.isArray(type.chapters)).toBe(true);
        expect(type.chapters.length).toBeGreaterThan(0);
      }
    });
  });

  describe('addMilestone', () => {
    it('should add a milestone to thesis schedule', () => {
      const thesis = createThesis({
        title: 'Test',
        author: { name: 'Author' }
      });

      const updated = addMilestone(thesis, {
        name: 'New Milestone',
        date: '2025-05-01'
      });

      expect(updated.schedule.milestones).toHaveLength(1);
      expect(updated.schedule.milestones[0].name).toBe('New Milestone');
      expect(updated.schedule.milestones[0].status).toBe('pending');
    });

    it('should preserve existing milestones', () => {
      let thesis = createThesis({
        title: 'Test',
        author: { name: 'Author' }
      });

      thesis = addMilestone(thesis, { name: 'First', date: '2025-01-01' });
      thesis = addMilestone(thesis, { name: 'Second', date: '2025-02-01' });

      expect(thesis.schedule.milestones).toHaveLength(2);
    });
  });

  describe('updateMilestoneStatus', () => {
    it('should update milestone status', () => {
      let thesis = createThesis({
        title: 'Test',
        author: { name: 'Author' }
      });

      thesis = addMilestone(thesis, { name: 'Task', date: '2025-01-01' });
      thesis = updateMilestoneStatus(thesis, 0, 'completed');

      expect(thesis.schedule.milestones[0].status).toBe('completed');
    });

    it('should throw for invalid index', () => {
      const thesis = createThesis({
        title: 'Test',
        author: { name: 'Author' }
      });

      expect(() => updateMilestoneStatus(thesis, 5, 'completed')).toThrow();
    });
  });

  describe('calculateProgress', () => {
    it('should calculate 0% for empty chapters', () => {
      const thesis = createThesis({
        title: 'Test',
        author: { name: 'Author' }
      });

      const progress = calculateProgress(thesis);
      expect(progress).toBe(0);
    });

    it('should calculate average completion', () => {
      const thesis = createThesis({
        title: 'Test',
        author: { name: 'Author' },
        customChapters: [
          { heading: 'Ch1' },
          { heading: 'Ch2' }
        ]
      });

      // Manually set completion
      thesis.chapters[0].completion = 100;
      thesis.chapters[1].completion = 50;

      const progress = calculateProgress(thesis);
      expect(progress).toBe(75);
    });
  });

  describe('thesisToRdf', () => {
    it('should convert thesis to RDF-compatible object', () => {
      const thesis = createThesis({
        title: 'RDF Test Thesis',
        author: { name: 'RDF Author' },
        institution: 'Test University'
      });

      const rdf = thesisToRdf(thesis);

      expect(rdf['@id']).toContain(thesis.id);
      expect(rdf['@type']).toContain('Monograph');
      expect(rdf['http://papers-thesis.org/ontology#hasTitle']).toBe(thesis.title);
    });

    it('should include supervisor in RDF', () => {
      const thesis = createThesis({
        title: 'Test',
        author: { name: 'Student' },
        supervisor: { name: 'Supervisor' }
      });

      const rdf = thesisToRdf(thesis);

      expect(rdf['http://papers-thesis.org/ontology#hasSupervisor']).toBeDefined();
    });
  });

  describe('ThesisType enum', () => {
    it('should have all type values', () => {
      expect(ThesisType.MONOGRAPH).toBe('monograph');
      expect(ThesisType.NARRATIVE).toBe('narrative');
      expect(ThesisType.CONTRIBUTION).toBe('contribution');
    });
  });

  describe('DegreeType enum', () => {
    it('should have all degree values', () => {
      expect(DegreeType.PHD).toBe('PhD');
      expect(DegreeType.MASTER).toBe('Master');
      expect(DegreeType.BACHELOR).toBe('Bachelor');
    });
  });

  describe('THESIS_TYPE_CONFIG', () => {
    it('should have configuration for all types', () => {
      expect(THESIS_TYPE_CONFIG.monograph).toBeDefined();
      expect(THESIS_TYPE_CONFIG.narrative).toBeDefined();
      expect(THESIS_TYPE_CONFIG.contribution).toBeDefined();
    });
  });
});
