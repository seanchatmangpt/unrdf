/**
 * Zod schemas for domain type validation
 * @module domain/types
 */

import { z } from 'zod';
import {
  PAPER_FAMILY_NAMES,
  THESIS_TYPE_NAMES,
  OUTPUT_FORMAT_NAMES,
  SHELL_TYPE_NAMES,
} from './constants.mjs';

/**
 * Paper family schema
 * @type {import('zod').ZodEnum<['IMRaD', 'Argument', 'Contribution', 'DSR', 'Monograph', 'Narrative']>}
 */
export const PaperFamilySchema = z.enum(/** @type {[string, ...string[]]} */ (PAPER_FAMILY_NAMES));

/**
 * Thesis type schema
 * @type {import('zod').ZodEnum<['Masters', 'PhD', 'Postdoctoral', 'MBA', 'Professional']>}
 */
export const ThesisTypeSchema = z.enum(/** @type {[string, ...string[]]} */ (THESIS_TYPE_NAMES));

/**
 * Output format schema
 * @type {import('zod').ZodEnum<['json', 'json-pretty', 'yaml', 'table']>}
 */
export const OutputFormatSchema = z.enum(
  /** @type {[string, ...string[]]} */ (OUTPUT_FORMAT_NAMES)
);

/**
 * Shell type schema
 * @type {import('zod').ZodEnum<['bash', 'zsh', 'fish', 'powershell']>}
 */
export const ShellTypeSchema = z.enum(/** @type {[string, ...string[]]} */ (SHELL_TYPE_NAMES));

/**
 * Section schema for paper/thesis sections
 */
export const SectionSchema = z.object({
  name: z.string().min(1),
  content: z.string().optional(),
  order: z.number().int().nonnegative().optional(),
});

/**
 * Paper schema for validation
 */
export const PaperSchema = z.object({
  family: PaperFamilySchema,
  title: z.string().optional(),
  abstract: z.string().optional(),
  sections: z.array(z.string()).optional(),
  metadata: z.record(z.unknown()).optional(),
  createdAt: z.string().datetime().optional(),
  updatedAt: z.string().datetime().optional(),
});

/**
 * Schedule item schema for thesis timeline
 */
export const ScheduleItemSchema = z.object({
  milestone: z.string().min(1),
  dueDate: z.string(),
  completed: z.boolean().default(false),
  notes: z.string().optional(),
});

/**
 * Thesis schema for validation
 */
export const ThesisSchema = z.object({
  type: ThesisTypeSchema,
  title: z.string().optional(),
  schedule: z.array(ScheduleItemSchema).optional(),
  defenseDate: z.string().optional(),
  sections: z.array(z.string()).optional(),
  committee: z.array(z.string()).optional(),
  metadata: z.record(z.unknown()).optional(),
  createdAt: z.string().datetime().optional(),
  updatedAt: z.string().datetime().optional(),
});

/**
 * Config schema for configuration values
 */
export const ConfigSchema = z.object({
  outputFormat: OutputFormatSchema.default('json'),
  shell: ShellTypeSchema.optional(),
  verbose: z.boolean().default(false),
  color: z.boolean().default(true),
  timeout: z.number().positive().default(30000),
  maxResults: z.number().int().positive().default(100),
  customSettings: z.record(z.unknown()).optional(),
});

/**
 * @typedef {z.infer<typeof PaperFamilySchema>} PaperFamily
 * @typedef {z.infer<typeof ThesisTypeSchema>} ThesisType
 * @typedef {z.infer<typeof OutputFormatSchema>} OutputFormat
 * @typedef {z.infer<typeof ShellTypeSchema>} ShellType
 * @typedef {z.infer<typeof SectionSchema>} Section
 * @typedef {z.infer<typeof PaperSchema>} PaperData
 * @typedef {z.infer<typeof ScheduleItemSchema>} ScheduleItem
 * @typedef {z.infer<typeof ThesisSchema>} ThesisData
 * @typedef {z.infer<typeof ConfigSchema>} ConfigData
 */
