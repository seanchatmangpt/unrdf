/**
 * @fileoverview Thesis domain model with Zod validation
 *
 * @description
 * Defines the Thesis domain model, validation schema, and factory functions.
 * Theses represent academic thesis documents with various structural types.
 *
 * @module domain/models/thesis
 * @version 1.0.0
 * @license MIT
 */

import { z } from 'zod';
import { AuthorSchema } from './paper.mjs';

/**
 * Thesis type enum values
 * @readonly
 * @enum {string}
 */
export const ThesisType = {
  MONOGRAPH: 'monograph',
  NARRATIVE: 'narrative',
  CONTRIBUTION: 'contribution'
};

/**
 * Degree type enum values
 * @readonly
 * @enum {string}
 */
export const DegreeType = {
  PHD: 'PhD',
  MASTER: 'Master',
  BACHELOR: 'Bachelor'
};

/**
 * Thesis type configurations with chapter templates
 * @type {Record<string, {name: string, description: string, chapters: string[]}>}
 */
export const THESIS_TYPE_CONFIG = {
  [ThesisType.MONOGRAPH]: {
    name: 'Monograph',
    description: 'Traditional monograph-style thesis',
    chapters: ['Introduction', 'Literature Review', 'Methodology', 'Results', 'Discussion', 'Conclusion']
  },
  [ThesisType.NARRATIVE]: {
    name: 'Narrative',
    description: 'Narrative-style thesis',
    chapters: ['Prologue', 'Background', 'Story', 'Analysis', 'Epilogue']
  },
  [ThesisType.CONTRIBUTION]: {
    name: 'Contribution-based',
    description: 'Publication-based thesis with multiple papers',
    chapters: ['Introduction', 'Paper 1', 'Paper 2', 'Paper 3', 'Synthesis', 'Conclusion']
  }
};

/**
 * Zod schema for Milestone
 */
export const MilestoneSchema = z.object({
  /** Milestone name */
  name: z.string().min(1, 'Milestone name is required'),
  /** Scheduled date */
  date: z.string().regex(/^\d{4}-\d{2}-\d{2}$/, 'Date must be YYYY-MM-DD format'),
  /** Status */
  status: z.enum(['pending', 'in-progress', 'completed']).default('pending'),
  /** Notes */
  notes: z.string().optional()
});

/**
 * Zod schema for Schedule
 */
export const ScheduleSchema = z.object({
  /** Defense date */
  defenseDate: z.string().regex(/^\d{4}-\d{2}-\d{2}$/).optional(),
  /** Milestones */
  milestones: z.array(MilestoneSchema).default([])
});

/**
 * Zod schema for Chapter
 */
export const ChapterSchema = z.object({
  /** Chapter heading/title */
  heading: z.string().min(1, 'Chapter heading is required'),
  /** Chapter content (can be empty for template) */
  content: z.string().default(''),
  /** Order within document */
  order: z.number().int().positive(),
  /** Word count (estimated or actual) */
  wordCount: z.number().int().nonnegative().optional(),
  /** Completion percentage */
  completion: z.number().min(0).max(100).default(0)
});

/**
 * Zod schema for Thesis
 */
export const ThesisSchema = z.object({
  /** Unique identifier (URI) */
  id: z.string().min(1),
  /** Thesis structural type */
  type: z.enum(['monograph', 'narrative', 'contribution']),
  /** Thesis title */
  title: z.string().min(1, 'Title is required'),
  /** Thesis abstract */
  abstract: z.string().default(''),
  /** Thesis author */
  author: AuthorSchema,
  /** Supervisor */
  supervisor: AuthorSchema.optional(),
  /** Institution */
  institution: z.string().optional(),
  /** Department */
  department: z.string().optional(),
  /** Degree type */
  degree: z.enum(['PhD', 'Master', 'Bachelor']).default('PhD'),
  /** Thesis chapters */
  chapters: z.array(ChapterSchema).default([]),
  /** Thesis schedule */
  schedule: ScheduleSchema.default({ milestones: [] }),
  /** Acknowledgements */
  acknowledgements: z.string().optional(),
  /** Creation timestamp */
  createdAt: z.string().datetime(),
  /** Last modification timestamp */
  lastModified: z.string().datetime().optional()
});

/**
 * Input schema for creating a new thesis (partial)
 */
export const CreateThesisInputSchema = z.object({
  type: z.enum(['monograph', 'narrative', 'contribution']).default('monograph'),
  title: z.string().min(1, 'Title is required'),
  abstract: z.string().optional(),
  author: z.object({
    name: z.string().min(1),
    affiliation: z.string().optional(),
    email: z.string().email().optional()
  }),
  supervisor: z.object({
    name: z.string().min(1),
    affiliation: z.string().optional(),
    email: z.string().email().optional()
  }).optional(),
  institution: z.string().optional(),
  department: z.string().optional(),
  degree: z.enum(['PhD', 'Master', 'Bachelor']).optional(),
  customChapters: z.array(z.object({
    heading: z.string(),
    content: z.string().optional()
  })).optional(),
  schedule: z.object({
    defenseDate: z.string().optional(),
    milestones: z.array(z.object({
      name: z.string(),
      date: z.string()
    })).optional()
  }).optional()
});

/**
 * @typedef {z.infer<typeof MilestoneSchema>} Milestone
 */

/**
 * @typedef {z.infer<typeof ScheduleSchema>} Schedule
 */

/**
 * @typedef {z.infer<typeof ChapterSchema>} Chapter
 */

/**
 * @typedef {z.infer<typeof ThesisSchema>} Thesis
 */

/**
 * @typedef {z.infer<typeof CreateThesisInputSchema>} CreateThesisInput
 */

/**
 * Generate a unique thesis ID
 * @returns {string} Unique thesis ID
 */
export function generateThesisId() {
  return `thesis-${Date.now()}-${Math.random().toString(36).substring(2, 9)}`;
}

/**
 * Create a new thesis with default chapters based on type
 * @param {CreateThesisInput} input - Thesis creation input
 * @returns {Thesis} Created thesis object
 */
export function createThesis(input) {
  // Validate input
  const validated = CreateThesisInputSchema.parse(input);

  // Get type configuration
  const typeConfig = THESIS_TYPE_CONFIG[validated.type];
  if (!typeConfig) {
    throw new Error(`Unknown thesis type: ${validated.type}`);
  }

  // Generate chapters from type template or use custom chapters
  const chapters = validated.customChapters
    ? validated.customChapters.map((c, i) => ({
        heading: c.heading,
        content: c.content || '',
        order: i + 1,
        completion: 0
      }))
    : typeConfig.chapters.map((heading, i) => ({
        heading,
        content: '',
        order: i + 1,
        completion: 0
      }));

  // Normalize author
  const author = {
    name: validated.author.name,
    affiliation: validated.author.affiliation || validated.institution || 'Unknown',
    email: validated.author.email,
    role: 'primary'
  };

  // Normalize supervisor if provided
  const supervisor = validated.supervisor
    ? {
        name: validated.supervisor.name,
        affiliation: validated.supervisor.affiliation || validated.institution,
        email: validated.supervisor.email,
        role: 'supervisor'
      }
    : undefined;

  // Build schedule
  const schedule = {
    defenseDate: validated.schedule?.defenseDate,
    milestones: validated.schedule?.milestones?.map(m => ({
      name: m.name,
      date: m.date,
      status: 'pending'
    })) || []
  };

  const now = new Date().toISOString();

  const thesis = {
    id: generateThesisId(),
    type: validated.type,
    title: validated.title,
    abstract: validated.abstract || '',
    author,
    supervisor,
    institution: validated.institution,
    department: validated.department,
    degree: validated.degree || 'PhD',
    chapters,
    schedule,
    createdAt: now,
    lastModified: now
  };

  // Validate complete thesis
  return ThesisSchema.parse(thesis);
}

/**
 * Convert thesis to RDF-compatible object for knowledge graph
 * @param {Thesis} thesis - Thesis object
 * @returns {Object} RDF-compatible representation
 */
export function thesisToRdf(thesis) {
  const baseUri = 'http://papers-thesis.org/examples#';
  const ontologyUri = 'http://papers-thesis.org/ontology#';

  const rdf = {
    '@id': `${baseUri}${thesis.id}`,
    '@type': `${ontologyUri}${getTypeClass(thesis.type)}`,
    [`${ontologyUri}hasTitle`]: thesis.title,
    [`${ontologyUri}hasAbstract`]: thesis.abstract,
    [`${ontologyUri}thesisType`]: thesis.type,
    [`${ontologyUri}degree`]: thesis.degree,
    [`${ontologyUri}createdAt`]: {
      '@type': 'http://www.w3.org/2001/XMLSchema#dateTime',
      '@value': thesis.createdAt
    },
    [`${ontologyUri}hasAuthor`]: {
      '@id': `${baseUri}${thesis.id}-author`,
      '@type': `${ontologyUri}Author`,
      [`${ontologyUri}authorName`]: thesis.author.name,
      [`${ontologyUri}authorAffiliation`]: thesis.author.affiliation,
      [`${ontologyUri}authorRole`]: 'primary'
    }
  };

  if (thesis.institution) {
    rdf[`${ontologyUri}institution`] = thesis.institution;
  }

  if (thesis.department) {
    rdf[`${ontologyUri}department`] = thesis.department;
  }

  if (thesis.supervisor) {
    rdf[`${ontologyUri}hasSupervisor`] = {
      '@id': `${baseUri}${thesis.id}-supervisor`,
      '@type': `${ontologyUri}Author`,
      [`${ontologyUri}authorName`]: thesis.supervisor.name,
      [`${ontologyUri}authorAffiliation`]: thesis.supervisor.affiliation,
      [`${ontologyUri}authorRole`]: 'supervisor'
    };
  }

  if (thesis.schedule.defenseDate) {
    rdf[`${ontologyUri}hasSchedule`] = {
      '@id': `${baseUri}${thesis.id}-schedule`,
      '@type': `${ontologyUri}Schedule`,
      [`${ontologyUri}defenseDate`]: {
        '@type': 'http://www.w3.org/2001/XMLSchema#date',
        '@value': thesis.schedule.defenseDate
      },
      [`${ontologyUri}hasMilestone`]: thesis.schedule.milestones.map((m, i) => ({
        '@id': `${baseUri}${thesis.id}-milestone-${i}`,
        '@type': `${ontologyUri}Milestone`,
        [`${ontologyUri}milestoneName`]: m.name,
        [`${ontologyUri}milestoneDate`]: {
          '@type': 'http://www.w3.org/2001/XMLSchema#date',
          '@value': m.date
        },
        [`${ontologyUri}milestoneStatus`]: m.status
      }))
    };
  }

  return rdf;
}

/**
 * Get RDF class name for thesis type
 * @param {string} type - Thesis type
 * @returns {string} RDF class name
 */
function getTypeClass(type) {
  const classMap = {
    monograph: 'Monograph',
    narrative: 'NarrativeThesis',
    contribution: 'ContributionThesis'
  };
  return classMap[type] || 'Thesis';
}

/**
 * List all available thesis types
 * @returns {Array<{id: string, name: string, description: string, chapters: string[]}>}
 */
export function listThesisTypes() {
  return Object.entries(THESIS_TYPE_CONFIG).map(([id, config]) => ({
    id,
    ...config
  }));
}

/**
 * Add a milestone to a thesis schedule
 * @param {Thesis} thesis - Thesis object
 * @param {Object} milestone - Milestone to add
 * @returns {Thesis} Updated thesis
 */
export function addMilestone(thesis, milestone) {
  const validated = MilestoneSchema.parse({
    ...milestone,
    status: milestone.status || 'pending'
  });

  return {
    ...thesis,
    schedule: {
      ...thesis.schedule,
      milestones: [...thesis.schedule.milestones, validated]
    },
    lastModified: new Date().toISOString()
  };
}

/**
 * Update milestone status
 * @param {Thesis} thesis - Thesis object
 * @param {number} milestoneIndex - Index of milestone to update
 * @param {string} status - New status
 * @returns {Thesis} Updated thesis
 */
export function updateMilestoneStatus(thesis, milestoneIndex, status) {
  const milestones = [...thesis.schedule.milestones];

  if (milestoneIndex < 0 || milestoneIndex >= milestones.length) {
    throw new Error(`Invalid milestone index: ${milestoneIndex}`);
  }

  milestones[milestoneIndex] = {
    ...milestones[milestoneIndex],
    status
  };

  return {
    ...thesis,
    schedule: {
      ...thesis.schedule,
      milestones
    },
    lastModified: new Date().toISOString()
  };
}

/**
 * Calculate thesis progress based on chapter completion
 * @param {Thesis} thesis - Thesis object
 * @returns {number} Overall progress percentage (0-100)
 */
export function calculateProgress(thesis) {
  if (thesis.chapters.length === 0) return 0;

  const totalCompletion = thesis.chapters.reduce(
    (sum, chapter) => sum + (chapter.completion || 0),
    0
  );

  return Math.round(totalCompletion / thesis.chapters.length);
}
