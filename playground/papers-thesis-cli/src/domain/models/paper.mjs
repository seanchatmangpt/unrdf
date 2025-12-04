/**
 * @fileoverview Paper domain model with Zod validation
 *
 * @description
 * Defines the Paper domain model, validation schema, and factory functions.
 * Papers represent research papers with various structural families.
 *
 * @module domain/models/paper
 * @version 1.0.0
 * @license MIT
 */

import { z } from 'zod';

/**
 * Paper family enum values
 * @readonly
 * @enum {string}
 */
export const PaperFamily = {
  IMRAD: 'imrad',
  DSR: 'dsr',
  ARGUMENT: 'argument',
  CONTRIBUTION: 'contribution',
};

/**
 * Paper family configurations with section templates
 * @type {Record<string, {name: string, description: string, sections: string[]}>}
 */
export const PAPER_FAMILY_CONFIG = {
  [PaperFamily.IMRAD]: {
    name: 'IMRAD',
    description: 'Introduction, Methods, Results, and Discussion',
    sections: ['Introduction', 'Methods', 'Results', 'Discussion', 'Conclusion'],
  },
  [PaperFamily.DSR]: {
    name: 'Design Science Research',
    description: 'Design Science Research methodology',
    sections: [
      'Problem Identification',
      'Objectives',
      'Design & Development',
      'Demonstration',
      'Evaluation',
      'Communication',
    ],
  },
  [PaperFamily.ARGUMENT]: {
    name: 'Argument-based',
    description: 'Argument-based paper structure',
    sections: ['Thesis Statement', 'Premises', 'Arguments', 'Counter-arguments', 'Conclusion'],
  },
  [PaperFamily.CONTRIBUTION]: {
    name: 'Contribution',
    description: 'Research contribution structure',
    sections: [
      'Motivation',
      'Background',
      'Contribution',
      'Validation',
      'Related Work',
      'Conclusion',
    ],
  },
};

/**
 * Zod schema for Author
 */
export const AuthorSchema = z.object({
  /** Author's full name */
  name: z.string().min(1, 'Author name is required'),
  /** Institutional affiliation */
  affiliation: z.string().optional(),
  /** Email address */
  email: z.string().email().optional(),
  /** Author role (primary, co-author, etc.) */
  role: z.enum(['primary', 'co-author', 'corresponding']).default('primary'),
  /** ORCID identifier */
  orcid: z
    .string()
    .regex(/^\d{4}-\d{4}-\d{4}-\d{3}[\dX]$/)
    .optional(),
});

/**
 * Zod schema for Section
 */
export const SectionSchema = z.object({
  /** Section heading/title */
  heading: z.string().min(1, 'Section heading is required'),
  /** Section content (can be empty for template) */
  content: z.string().default(''),
  /** Order within document */
  order: z.number().int().positive(),
  /** Nesting level (1=section, 2=subsection, 3=subsubsection) */
  level: z.number().int().min(1).max(3).default(1),
  /** Parent section ID (for nested sections) */
  parentId: z.string().optional(),
});

/**
 * Zod schema for Paper
 */
export const PaperSchema = z.object({
  /** Unique identifier (URI) */
  id: z.string().min(1),
  /** Paper structural family */
  family: z.enum(['imrad', 'dsr', 'argument', 'contribution']),
  /** Paper title */
  title: z.string().min(1, 'Title is required'),
  /** Paper abstract */
  abstract: z.string().default(''),
  /** Paper keywords */
  keywords: z.array(z.string()).default([]),
  /** Paper authors */
  authors: z.array(AuthorSchema).min(1, 'At least one author is required'),
  /** Paper sections */
  sections: z.array(SectionSchema).default([]),
  /** Publication venue */
  venue: z.string().optional(),
  /** DOI */
  doi: z.string().optional(),
  /** Creation timestamp */
  createdAt: z.string().datetime(),
  /** Last modification timestamp */
  lastModified: z.string().datetime().optional(),
});

/**
 * Input schema for creating a new paper (partial)
 */
export const CreatePaperInputSchema = z.object({
  family: z.enum(['imrad', 'dsr', 'argument', 'contribution']).default('imrad'),
  title: z.string().min(1, 'Title is required'),
  abstract: z.string().optional(),
  keywords: z.array(z.string()).optional(),
  authors: z
    .array(
      z.object({
        name: z.string().min(1),
        affiliation: z.string().optional(),
        email: z.string().email().optional(),
        role: z.enum(['primary', 'co-author', 'corresponding']).optional(),
      })
    )
    .min(1, 'At least one author is required'),
  venue: z.string().optional(),
  customSections: z
    .array(
      z.object({
        heading: z.string(),
        content: z.string().optional(),
      })
    )
    .optional(),
});

/**
 * @typedef {z.infer<typeof AuthorSchema>} Author
 */

/**
 * @typedef {z.infer<typeof SectionSchema>} Section
 */

/**
 * @typedef {z.infer<typeof PaperSchema>} Paper
 */

/**
 * @typedef {z.infer<typeof CreatePaperInputSchema>} CreatePaperInput
 */

/**
 * Generate a unique paper ID
 * @returns {string} Unique paper ID
 */
export function generatePaperId() {
  return `paper-${Date.now()}-${Math.random().toString(36).substring(2, 9)}`;
}

/**
 * Create a new paper with default sections based on family
 * @param {CreatePaperInput} input - Paper creation input
 * @returns {Paper} Created paper object
 */
export function createPaper(input) {
  // Validate input
  const validated = CreatePaperInputSchema.parse(input);

  // Get family configuration
  const familyConfig = PAPER_FAMILY_CONFIG[validated.family];
  if (!familyConfig) {
    throw new Error(`Unknown paper family: ${validated.family}`);
  }

  // Generate sections from family template or use custom sections
  const sections = validated.customSections
    ? validated.customSections.map((s, i) => ({
        heading: s.heading,
        content: s.content || '',
        order: i + 1,
        level: 1,
      }))
    : familyConfig.sections.map((heading, i) => ({
        heading,
        content: '',
        order: i + 1,
        level: 1,
      }));

  // Normalize authors with defaults
  const authors = validated.authors.map((a, i) => ({
    name: a.name,
    affiliation: a.affiliation || 'Unknown',
    email: a.email,
    role: a.role || (i === 0 ? 'primary' : 'co-author'),
  }));

  const now = new Date().toISOString();

  const paper = {
    id: generatePaperId(),
    family: validated.family,
    title: validated.title,
    abstract: validated.abstract || '',
    keywords: validated.keywords || [],
    authors,
    sections,
    venue: validated.venue,
    createdAt: now,
    lastModified: now,
  };

  // Validate complete paper
  return PaperSchema.parse(paper);
}

/**
 * Convert paper to RDF-compatible object for knowledge graph
 * @param {Paper} paper - Paper object
 * @returns {Object} RDF-compatible representation
 */
export function paperToRdf(paper) {
  const baseUri = 'http://papers-thesis.org/examples#';
  const ontologyUri = 'http://papers-thesis.org/ontology#';

  return {
    '@id': `${baseUri}${paper.id}`,
    '@type': `${ontologyUri}${getFamilyClass(paper.family)}`,
    [`${ontologyUri}hasTitle`]: paper.title,
    [`${ontologyUri}hasAbstract`]: paper.abstract,
    [`${ontologyUri}paperFamily`]: paper.family,
    [`${ontologyUri}createdAt`]: {
      '@type': 'http://www.w3.org/2001/XMLSchema#dateTime',
      '@value': paper.createdAt,
    },
    [`${ontologyUri}hasAuthor`]: paper.authors.map((author, i) => ({
      '@id': `${baseUri}${paper.id}-author-${i}`,
      '@type': `${ontologyUri}Author`,
      [`${ontologyUri}authorName`]: author.name,
      [`${ontologyUri}authorAffiliation`]: author.affiliation,
      [`${ontologyUri}authorRole`]: author.role,
    })),
    [`${ontologyUri}hasSection`]: paper.sections.map(section => ({
      '@id': `${baseUri}${paper.id}-section-${section.order}`,
      '@type': `${ontologyUri}Section`,
      [`${ontologyUri}sectionHeading`]: section.heading,
      [`${ontologyUri}sectionContent`]: section.content,
      [`${ontologyUri}sectionOrder`]: section.order,
    })),
  };
}

/**
 * Get RDF class name for paper family
 * @param {string} family - Paper family
 * @returns {string} RDF class name
 */
function getFamilyClass(family) {
  const classMap = {
    imrad: 'IMRADPaper',
    dsr: 'DSRPaper',
    argument: 'ArgumentPaper',
    contribution: 'ContributionPaper',
  };
  return classMap[family] || 'Paper';
}

/**
 * List all available paper families
 * @returns {Array<{id: string, name: string, description: string, sections: string[]}>}
 */
export function listPaperFamilies() {
  return Object.entries(PAPER_FAMILY_CONFIG).map(([id, config]) => ({
    id,
    ...config,
  }));
}
