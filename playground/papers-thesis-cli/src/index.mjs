/**
 * @fileoverview Papers-Thesis CLI - Main entry point
 *
 * @description
 * Main module exports for the Papers-Thesis CLI library.
 * Provides programmatic access to paper/thesis generation,
 * template rendering, and knowledge graph operations.
 *
 * @module papers-thesis-cli
 * @version 1.0.0
 * @license MIT
 */

// Domain models
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
  PAPER_FAMILY_CONFIG,
} from './domain/models/paper.mjs';

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
  THESIS_TYPE_CONFIG,
} from './domain/models/thesis.mjs';

// Integration layer
export {
  createTemplateEngine,
  templateEngine,
  renderPaper,
  renderThesis,
  texEscape,
  toBibtexKey,
  formatDate,
  wrapText,
} from './integration/templates.mjs';

export {
  createKnowledgeGraph,
  knowledgeGraph,
  NAMED_QUERIES,
} from './integration/knowledge-graph.mjs';
