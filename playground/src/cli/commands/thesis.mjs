/**
 * @fileoverview Thesis command - Manage thesis documents
 *
 * @description
 * CLI commands for generating, listing, and managing thesis documents.
 * Supports multiple thesis types: Monograph, Narrative, Contribution.
 * Includes schedule and committee management.
 *
 * @module cli/commands/thesis
 * @version 2.0.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { z } from 'zod';

// =============================================================================
// Constants
// =============================================================================

/**
 * Thesis types and their configurations
 * @type {Object.<string, {name: string, description: string, chapters: string[]}>}
 */
export const THESIS_TYPES = {
  monograph: {
    name: 'Monograph',
    description: 'Traditional monograph-style thesis',
    chapters: ['Introduction', 'Literature Review', 'Methodology', 'Results', 'Discussion', 'Conclusion']
  },
  narrative: {
    name: 'Narrative',
    description: 'Narrative-style thesis',
    chapters: ['Prologue', 'Background', 'Story', 'Analysis', 'Epilogue']
  },
  contribution: {
    name: 'Contribution-based',
    description: 'Publication-based thesis with multiple papers',
    chapters: ['Introduction', 'Paper 1', 'Paper 2', 'Paper 3', 'Synthesis', 'Conclusion']
  }
};

/**
 * Committee roles
 * @type {string[]}
 */
const COMMITTEE_ROLES = ['chair', 'advisor', 'member', 'external', 'reader'];

/**
 * Milestone statuses
 * @type {string[]}
 */
const MILESTONE_STATUSES = ['pending', 'in-progress', 'completed', 'overdue'];

// =============================================================================
// Zod Schemas
// =============================================================================

/**
 * Zod schema for thesis generation input
 */
const ThesisInputSchema = z.object({
  type: z.enum(['monograph', 'narrative', 'contribution']).default('monograph'),
  title: z.string().min(1, 'Title is required'),
  author: z.string().min(1, 'Author is required'),
  institution: z.string().optional(),
  department: z.string().optional(),
  supervisor: z.string().optional(),
  degree: z.enum(['PhD', 'Master', 'Bachelor']).default('PhD')
});

/**
 * Zod schema for schedule milestone
 */
const MilestoneSchema = z.object({
  name: z.string().min(1),
  date: z.string().regex(/^\d{4}-\d{2}-\d{2}$/, 'Date must be YYYY-MM-DD format'),
  description: z.string().optional()
});

/**
 * Zod schema for committee member
 */
const CommitteeMemberSchema = z.object({
  name: z.string().min(1),
  role: z.enum(['chair', 'advisor', 'member', 'external', 'reader']),
  institution: z.string().optional(),
  email: z.string().email().optional()
});

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Print progress indicator
 * @param {string} message - Progress message
 * @param {string} type - Message type
 */
function printProgress(message, type = 'info') {
  const icons = {
    info: '\x1b[34mi\x1b[0m',
    success: '\x1b[32m+\x1b[0m',
    warning: '\x1b[33m!\x1b[0m',
    error: '\x1b[31mx\x1b[0m',
    progress: '\x1b[36m~\x1b[0m'
  };
  console.log(`${icons[type] || icons.info} ${message}`);
}

/**
 * Simple YAML formatter
 * @param {Object} obj - Object to format
 * @param {number} indent - Current indentation level
 * @returns {string} YAML-formatted string
 */
function toSimpleYaml(obj, indent = 0) {
  const spaces = '  '.repeat(indent);
  let result = '';

  for (const [key, value] of Object.entries(obj)) {
    if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
      result += `${spaces}${key}:\n${toSimpleYaml(value, indent + 1)}`;
    } else if (Array.isArray(value)) {
      result += `${spaces}${key}:\n`;
      for (const item of value) {
        if (typeof item === 'object') {
          result += `${spaces}  -\n${toSimpleYaml(item, indent + 2)}`;
        } else {
          result += `${spaces}  - ${item}\n`;
        }
      }
    } else {
      result += `${spaces}${key}: ${value}\n`;
    }
  }

  return result;
}

// =============================================================================
// In-Memory State (would be persisted in production)
// =============================================================================

let currentSchedule = {
  defenseDate: null,
  milestones: [
    { name: 'Thesis Proposal', date: '2024-09-01', status: 'completed' },
    { name: 'Literature Review', date: '2024-12-01', status: 'completed' },
    { name: 'Complete Draft', date: '2025-03-01', status: 'in-progress' },
    { name: 'Final Review', date: '2025-05-15', status: 'pending' },
    { name: 'Thesis Defense', date: '2025-06-15', status: 'pending' }
  ]
};

let committee = [];

// =============================================================================
// Generate Command
// =============================================================================

/**
 * Generate thesis subcommand
 */
const generateCommand = defineCommand({
  meta: {
    name: 'generate',
    description: 'Generate thesis structure'
  },
  args: {
    type: {
      type: 'positional',
      description: 'Thesis type (monograph, narrative, contribution)',
      required: false,
      default: 'monograph'
    },
    title: {
      type: 'string',
      alias: 't',
      description: 'Thesis title',
      required: true
    },
    author: {
      type: 'string',
      alias: 'a',
      description: 'Author name',
      required: true
    },
    institution: {
      type: 'string',
      description: 'Institution name'
    },
    department: {
      type: 'string',
      description: 'Department name'
    },
    supervisor: {
      type: 'string',
      alias: 's',
      description: 'Supervisor name'
    },
    advisor: {
      type: 'string',
      description: 'Primary advisor (alias for supervisor)'
    },
    degree: {
      type: 'string',
      alias: 'd',
      description: 'Degree type (PhD, Master, Bachelor)',
      default: 'PhD'
    },
    schedule: {
      type: 'boolean',
      description: 'Generate with default schedule',
      default: false
    },
    output: {
      type: 'string',
      alias: 'o',
      description: 'Output file path'
    },
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (latex, json, yaml)',
      default: 'latex'
    },
    quiet: {
      type: 'boolean',
      alias: 'q',
      description: 'Suppress progress output',
      default: false
    }
  },
  async run({ args }) {
    try {
      const input = ThesisInputSchema.parse({
        type: args.type || 'monograph',
        title: args.title,
        author: args.author,
        institution: args.institution,
        department: args.department,
        supervisor: args.supervisor || args.advisor,
        degree: args.degree || 'PhD'
      });

      const typeConfig = THESIS_TYPES[input.type];

      if (!typeConfig) {
        printProgress(`Unknown thesis type: ${input.type}`, 'error');
        console.error(`Available types: ${Object.keys(THESIS_TYPES).join(', ')}`);
        process.exit(2);
      }

      if (!args.quiet) {
        printProgress(`Generating ${typeConfig.name} thesis...`, 'progress');
        printProgress(`  Title: ${input.title}`, 'info');
        printProgress(`  Author: ${input.author}`, 'info');
        printProgress(`  Degree: ${input.degree}`, 'info');
        if (input.institution) printProgress(`  Institution: ${input.institution}`, 'info');
        if (input.department) printProgress(`  Department: ${input.department}`, 'info');
        if (input.supervisor) printProgress(`  Supervisor: ${input.supervisor}`, 'info');
        printProgress(`  Type: ${typeConfig.name}`, 'info');
        printProgress(`  Chapters: ${typeConfig.chapters.join(', ')}`, 'info');
      }

      // Generate thesis object
      const thesis = {
        id: `thesis-${Date.now()}`,
        type: input.type,
        typeName: typeConfig.name,
        title: input.title,
        author: {
          name: input.author,
          institution: input.institution || 'Unknown',
          department: input.department
        },
        supervisor: input.supervisor ? { name: input.supervisor } : null,
        degree: input.degree,
        chapters: typeConfig.chapters.map((name, index) => ({
          heading: name,
          order: index + 1,
          content: ''
        })),
        schedule: args.schedule ? { ...currentSchedule } : {
          milestones: [],
          defenseDate: null
        },
        committee: [],
        metadata: {
          format: args.format,
          createdAt: new Date().toISOString()
        }
      };

      if (args.format === 'json') {
        console.log(JSON.stringify(thesis, null, 2));
      } else if (args.format === 'yaml') {
        console.log(toSimpleYaml(thesis));
      } else {
        if (!args.quiet) {
          printProgress('Thesis structure generated successfully!', 'success');
          printProgress(`Output: ${args.output || `./output/${thesis.id}.tex`}`, 'info');
        }
      }

      return thesis;

    } catch (error) {
      if (error instanceof z.ZodError) {
        printProgress('Validation error:', 'error');
        error.errors.forEach(err => {
          console.error(`  - ${err.path.join('.')}: ${err.message}`);
        });
      } else {
        printProgress(`Error generating thesis: ${error.message}`, 'error');
      }
      process.exit(1);
    }
  }
});

// =============================================================================
// List Command
// =============================================================================

/**
 * List thesis types subcommand
 */
const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List available thesis types'
  },
  args: {
    verbose: {
      type: 'boolean',
      alias: 'v',
      description: 'Show detailed information',
      default: false
    },
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (table, json, yaml)',
      default: 'table'
    }
  },
  async run({ args }) {
    const types = Object.entries(THESIS_TYPES).map(([key, value]) => ({
      id: key,
      name: value.name,
      description: value.description,
      chapters: value.chapters,
      chapterCount: value.chapters.length
    }));

    if (args.format === 'json') {
      console.log(JSON.stringify(types, null, 2));
      return types;
    }

    if (args.format === 'yaml') {
      console.log(toSimpleYaml({ types }));
      return types;
    }

    console.log('Available Thesis Types:\n');

    for (const type of types) {
      console.log(`  \x1b[36m${type.id.padEnd(15)}\x1b[0m ${type.name}`);

      if (args.verbose) {
        console.log(`  ${''.padEnd(15)} ${type.description}`);
        console.log(`  ${''.padEnd(15)} Chapters: ${type.chapters.join(' > ')}`);
        console.log('');
      }
    }

    if (!args.verbose) {
      console.log('\nUse --verbose for detailed information');
    }

    return types;
  }
});

// =============================================================================
// Schedule Commands
// =============================================================================

/**
 * Schedule list subcommand
 */
const scheduleListCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'Show current schedule'
  },
  args: {
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (table, json)',
      default: 'table'
    }
  },
  async run({ args }) {
    if (args.format === 'json') {
      console.log(JSON.stringify(currentSchedule, null, 2));
      return currentSchedule;
    }

    console.log('Thesis Schedule:\n');
    if (currentSchedule.defenseDate) {
      console.log(`  Defense Date: ${currentSchedule.defenseDate}\n`);
    }
    console.log('  Milestones:');

    for (const milestone of currentSchedule.milestones) {
      const statusIcon = milestone.status === 'completed' ? '\x1b[32m[x]\x1b[0m' :
                         milestone.status === 'in-progress' ? '\x1b[33m[~]\x1b[0m' :
                         milestone.status === 'overdue' ? '\x1b[31m[!]\x1b[0m' : '[ ]';
      console.log(`    ${statusIcon} ${milestone.date}  ${milestone.name}`);
    }

    return currentSchedule;
  }
});

/**
 * Schedule add subcommand
 */
const scheduleAddCommand = defineCommand({
  meta: {
    name: 'add',
    description: 'Add milestone to schedule'
  },
  args: {
    milestone: {
      type: 'positional',
      description: 'Milestone name',
      required: true
    },
    date: {
      type: 'string',
      alias: 'd',
      description: 'Milestone date (YYYY-MM-DD)',
      required: true
    },
    description: {
      type: 'string',
      description: 'Milestone description'
    }
  },
  async run({ args }) {
    try {
      const milestone = MilestoneSchema.parse({
        name: args.milestone,
        date: args.date,
        description: args.description
      });

      currentSchedule.milestones.push({
        ...milestone,
        status: 'pending'
      });

      // Sort by date
      currentSchedule.milestones.sort((a, b) => a.date.localeCompare(b.date));

      printProgress(`Milestone added: ${milestone.name} (${milestone.date})`, 'success');
      return milestone;

    } catch (error) {
      if (error instanceof z.ZodError) {
        printProgress('Validation error:', 'error');
        error.errors.forEach(err => {
          console.error(`  - ${err.path.join('.')}: ${err.message}`);
        });
      } else {
        printProgress(`Error adding milestone: ${error.message}`, 'error');
      }
      process.exit(1);
    }
  }
});

/**
 * Schedule complete subcommand
 */
const scheduleCompleteCommand = defineCommand({
  meta: {
    name: 'complete',
    description: 'Mark milestone as complete'
  },
  args: {
    milestone: {
      type: 'positional',
      description: 'Milestone name',
      required: true
    }
  },
  async run({ args }) {
    const milestone = currentSchedule.milestones.find(
      m => m.name.toLowerCase() === args.milestone.toLowerCase()
    );

    if (!milestone) {
      printProgress(`Milestone not found: ${args.milestone}`, 'error');
      console.log('Available milestones:');
      currentSchedule.milestones.forEach(m => {
        console.log(`  - ${m.name}`);
      });
      process.exit(1);
    }

    milestone.status = 'completed';
    printProgress(`Milestone completed: ${milestone.name}`, 'success');
    return milestone;
  }
});

/**
 * Schedule subcommand
 */
const scheduleCommand = defineCommand({
  meta: {
    name: 'schedule',
    description: 'Manage thesis schedule'
  },
  subCommands: {
    list: scheduleListCommand,
    add: scheduleAddCommand,
    complete: scheduleCompleteCommand
  }
});

// =============================================================================
// Defense Command
// =============================================================================

/**
 * Set defense date subcommand
 */
const defenseCommand = defineCommand({
  meta: {
    name: 'defense',
    description: 'Set thesis defense date'
  },
  args: {
    date: {
      type: 'positional',
      description: 'Defense date (YYYY-MM-DD)',
      required: true
    },
    time: {
      type: 'string',
      alias: 't',
      description: 'Defense time (HH:MM)'
    },
    location: {
      type: 'string',
      alias: 'l',
      description: 'Defense location'
    }
  },
  async run({ args }) {
    const dateRegex = /^\d{4}-\d{2}-\d{2}$/;
    if (!dateRegex.test(args.date)) {
      printProgress('Invalid date format. Use YYYY-MM-DD', 'error');
      process.exit(1);
    }

    currentSchedule.defenseDate = args.date;

    // Add defense to milestones if not present
    const hasDefenseMilestone = currentSchedule.milestones.some(
      m => m.name.toLowerCase().includes('defense')
    );

    if (!hasDefenseMilestone) {
      currentSchedule.milestones.push({
        name: 'Thesis Defense',
        date: args.date,
        status: 'pending'
      });
      currentSchedule.milestones.sort((a, b) => a.date.localeCompare(b.date));
    }

    printProgress(`Defense date set: ${args.date}`, 'success');
    if (args.time) printProgress(`Time: ${args.time}`, 'info');
    if (args.location) printProgress(`Location: ${args.location}`, 'info');

    return { date: args.date, time: args.time, location: args.location };
  }
});

// =============================================================================
// Committee Command
// =============================================================================

/**
 * Add committee member subcommand
 */
const committeeCommand = defineCommand({
  meta: {
    name: 'committee',
    description: 'Add committee member'
  },
  args: {
    member: {
      type: 'positional',
      description: 'Committee member name',
      required: true
    },
    role: {
      type: 'positional',
      description: `Role (${COMMITTEE_ROLES.join(', ')})`,
      required: true
    },
    institution: {
      type: 'string',
      alias: 'i',
      description: 'Member institution'
    },
    email: {
      type: 'string',
      alias: 'e',
      description: 'Member email'
    },
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (table, json)',
      default: 'table'
    }
  },
  async run({ args }) {
    try {
      const member = CommitteeMemberSchema.parse({
        name: args.member,
        role: args.role,
        institution: args.institution,
        email: args.email
      });

      committee.push(member);
      printProgress(`Committee member added: ${member.name} (${member.role})`, 'success');

      if (args.format === 'json') {
        console.log(JSON.stringify(committee, null, 2));
      } else {
        console.log('\nCurrent Committee:');
        for (const m of committee) {
          console.log(`  - ${m.name} (${m.role})${m.institution ? ` - ${m.institution}` : ''}`);
        }
      }

      return member;

    } catch (error) {
      if (error instanceof z.ZodError) {
        printProgress('Validation error:', 'error');
        error.errors.forEach(err => {
          console.error(`  - ${err.path.join('.')}: ${err.message}`);
        });
        console.log(`\nValid roles: ${COMMITTEE_ROLES.join(', ')}`);
      } else {
        printProgress(`Error adding committee member: ${error.message}`, 'error');
      }
      process.exit(1);
    }
  }
});

// =============================================================================
// Main Thesis Command Export
// =============================================================================

/**
 * Thesis command with subcommands
 * @type {import('citty').CommandDef}
 */
export const thesisCommand = defineCommand({
  meta: {
    name: 'thesis',
    description: 'Manage thesis documents'
  },
  subCommands: {
    generate: generateCommand,
    list: listCommand,
    schedule: scheduleCommand,
    defense: defenseCommand,
    committee: committeeCommand
  }
});
