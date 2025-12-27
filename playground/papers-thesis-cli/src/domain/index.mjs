/**
 * @fileoverview Domain layer exports
 *
 * @description
 * Exports all domain models, validation schemas, and business logic.
 *
 * @module domain
 * @version 1.0.0
 * @license MIT
 */

// Paper model and related exports
export {
  PaperSchema,
  AuthorSchema,
  SectionSchema,
  CreatePaperInputSchema,
  createPaper,
  paperToRdf,
  listPaperFamilies,
  generatePaperId,
  PaperFamily,
  PAPER_FAMILY_CONFIG
} from './models/paper.mjs';

// Thesis model and related exports
export {
  ThesisSchema,
  ChapterSchema,
  MilestoneSchema,
  ScheduleSchema,
  CreateThesisInputSchema,
  createThesis,
  thesisToRdf,
  listThesisTypes,
  generateThesisId,
  addMilestone,
  updateMilestoneStatus,
  calculateProgress,
  ThesisType,
  DegreeType,
  THESIS_TYPE_CONFIG
} from './models/thesis.mjs';
