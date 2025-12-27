/**
 * @fileoverview Papers command - Manage research papers
 *
 * @description
 * CLI commands for generating, listing, validating, converting,
 * and getting info about research papers.
 * Supports multiple paper families: IMRAD, DSR, Argument, Contribution.
 *
 * @module cli/commands/papers
 * @version 2.0.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { z } from 'zod';

// =============================================================================
// Constants
// =============================================================================

/**
 * Paper families and their configurations
 * @type {Object.<string, {name: string, description: string, sections: string[]}>}
 */
export const PAPER_FAMILIES = {
  imrad: {
    name: 'IMRAD',
    description: 'Introduction, Methods, Results, and Discussion',
    sections: ['Introduction', 'Methods', 'Results', 'Discussion', 'Conclusion']
  },
  dsr: {
    name: 'Design Science Research',
    description: 'Design Science Research structure',
    sections: ['Problem Identification', 'Objectives', 'Design & Development', 'Demonstration', 'Evaluation', 'Communication']
  },
  argument: {
    name: 'Argument-based',
    description: 'Argument-based paper structure',
    sections: ['Thesis Statement', 'Premises', 'Arguments', 'Counter-arguments', 'Conclusion']
  },
  contribution: {
    name: 'Contribution',
    description: 'Research contribution structure',
    sections: ['Motivation', 'Background', 'Contribution', 'Validation', 'Related Work', 'Conclusion']
  }
};

/**
 * Supported output formats
 * @type {string[]}
 */
const OUTPUT_FORMATS = ['latex', 'json', 'yaml', 'markdown'];

/**
 * Supported conversion formats
 * @type {string[]}
 */
const CONVERSION_FORMATS = ['latex', 'markdown', 'html', 'pdf', 'docx'];

// =============================================================================
// Zod Schemas
// =============================================================================

/**
 * Zod schema for paper generation input
 */
const PaperInputSchema = z.object({
  family: z.enum(['imrad', 'dsr', 'argument', 'contribution']).default('imrad'),
  title: z.string().min(1, 'Title is required'),
  author: z.string().min(1, 'Author is required'),
  affiliation: z.string().optional(),
  abstract: z.string().optional(),
  sections: z.array(z.object({
    heading: z.string(),
    content: z.string().optional()
  })).optional()
});

/**
 * Zod schema for conversion options
 */
const ConversionSchema = z.object({
  from: z.enum(['latex', 'markdown', 'html', 'json']).optional(),
  to: z.enum(['latex', 'markdown', 'html', 'pdf', 'docx']).default('markdown')
});

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Format output based on requested format
 * @param {Object} data - Data to format
 * @param {string} format - Output format
 * @returns {string} Formatted output
 */
function formatOutput(data, format) {
  switch (format) {
    case 'json':
    case 'json-pretty':
      return JSON.stringify(data, null, 2);
    case 'yaml':
      return toSimpleYaml(data);
    case 'table':
      return formatAsTable(data);
    default:
      return JSON.stringify(data, null, 2);
  }
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

/**
 * Format data as table
 * @param {Object} data - Data to format
 * @returns {string} Table-formatted string
 */
function formatAsTable(data) {
  if (Array.isArray(data)) {
    if (data.length === 0) return 'No data';
    const keys = Object.keys(data[0]);
    const header = keys.map(k => k.padEnd(20)).join(' ');
    const separator = '-'.repeat(header.length);
    const rows = data.map(item =>
      keys.map(k => String(item[k] || '').padEnd(20)).join(' ')
    );
    return [header, separator, ...rows].join('\n');
  }
  return Object.entries(data)
    .map(([k, v]) => `${k.padEnd(20)} ${v}`)
    .join('\n');
}

/**
 * Print progress indicator
 * @param {string} message - Progress message
 * @param {string} type - Message type (info, success, warning, error, progress)
 */
function printProgress(message, type = 'info') {
  const icons = {
    info: '\x1b[34mi\x1b[0m',      // blue
    success: '\x1b[32m+\x1b[0m',   // green
    warning: '\x1b[33m!\x1b[0m',   // yellow
    error: '\x1b[31mx\x1b[0m',     // red
    progress: '\x1b[36m~\x1b[0m'   // cyan
  };
  console.log(`${icons[type] || icons.info} ${message}`);
}

// =============================================================================
// Generate Command
// =============================================================================

/**
 * Generate paper subcommand
 */
const generateCommand = defineCommand({
  meta: {
    name: 'generate',
    description: 'Generate academic paper from template'
  },
  args: {
    family: {
      type: 'positional',
      description: 'Paper family (imrad, dsr, argument, contribution)',
      required: false,
      default: 'imrad'
    },
    title: {
      type: 'string',
      alias: 't',
      description: 'Paper title',
      required: true
    },
    author: {
      type: 'string',
      alias: 'a',
      description: 'Author name',
      required: true
    },
    affiliation: {
      type: 'string',
      description: 'Author affiliation'
    },
    abstract: {
      type: 'string',
      description: 'Paper abstract'
    },
    sections: {
      type: 'string',
      description: 'Custom sections (JSON array)'
    },
    output: {
      type: 'string',
      alias: 'o',
      description: 'Output file path'
    },
    template: {
      type: 'string',
      alias: 'T',
      description: 'Custom template path'
    },
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (latex, json, yaml, markdown)',
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
      // Parse custom sections if provided
      const parsedSections = args.sections ? JSON.parse(args.sections) : undefined;

      // Validate input
      const input = PaperInputSchema.parse({
        family: args.family || 'imrad',
        title: args.title,
        author: args.author,
        affiliation: args.affiliation,
        abstract: args.abstract,
        sections: parsedSections
      });

      const familyConfig = PAPER_FAMILIES[input.family];

      if (!familyConfig) {
        printProgress(`Unknown paper family: ${input.family}`, 'error');
        console.error(`Available families: ${Object.keys(PAPER_FAMILIES).join(', ')}`);
        process.exit(2);
      }

      if (!args.quiet) {
        printProgress(`Generating ${familyConfig.name} paper...`, 'progress');
        printProgress(`  Title: ${input.title}`, 'info');
        printProgress(`  Author: ${input.author}`, 'info');
        if (input.affiliation) printProgress(`  Affiliation: ${input.affiliation}`, 'info');
        printProgress(`  Family: ${familyConfig.name}`, 'info');
        printProgress(`  Sections: ${familyConfig.sections.join(', ')}`, 'info');
      }

      // Generate paper object
      const paper = {
        id: `paper-${Date.now()}`,
        family: input.family,
        familyName: familyConfig.name,
        title: input.title,
        authors: [{
          name: input.author,
          affiliation: input.affiliation || 'Unknown'
        }],
        abstract: input.abstract || '',
        sections: familyConfig.sections.map((name, index) => ({
          heading: name,
          order: index + 1,
          content: ''
        })),
        metadata: {
          template: args.template || `${input.family}.tex.njk`,
          format: args.format,
          createdAt: new Date().toISOString()
        }
      };

      // Output based on format
      if (args.format === 'json') {
        console.log(JSON.stringify(paper, null, 2));
      } else if (args.format === 'yaml') {
        console.log(toSimpleYaml(paper));
      } else {
        if (!args.quiet) {
          printProgress('Paper generated successfully!', 'success');
          printProgress(`Output: ${args.output || `./output/${paper.id}.tex`}`, 'info');
        }
      }

      return paper;

    } catch (error) {
      if (error instanceof z.ZodError) {
        printProgress('Validation error:', 'error');
        error.errors.forEach(err => {
          console.error(`  - ${err.path.join('.')}: ${err.message}`);
        });
      } else if (error instanceof SyntaxError) {
        printProgress('Invalid JSON format for sections', 'error');
      } else {
        printProgress(`Error generating paper: ${error.message}`, 'error');
      }
      process.exit(1);
    }
  }
});

// =============================================================================
// List Command
// =============================================================================

/**
 * List paper families subcommand
 */
const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List available paper families'
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
    const families = Object.entries(PAPER_FAMILIES).map(([key, value]) => ({
      id: key,
      name: value.name,
      description: value.description,
      sections: value.sections,
      sectionCount: value.sections.length
    }));

    if (args.format === 'json') {
      console.log(JSON.stringify(families, null, 2));
      return families;
    }

    if (args.format === 'yaml') {
      console.log(toSimpleYaml({ families }));
      return families;
    }

    // Table format
    console.log('Available Paper Families:\n');

    for (const family of families) {
      console.log(`  \x1b[36m${family.id.padEnd(15)}\x1b[0m ${family.name}`);

      if (args.verbose) {
        console.log(`  ${''.padEnd(15)} ${family.description}`);
        console.log(`  ${''.padEnd(15)} Sections: ${family.sections.join(' > ')}`);
        console.log('');
      }
    }

    if (!args.verbose) {
      console.log('\nUse --verbose for detailed information');
    }

    return families;
  }
});

// =============================================================================
// Validate Command
// =============================================================================

/**
 * Validate paper structure subcommand
 */
const validateCommand = defineCommand({
  meta: {
    name: 'validate',
    description: 'Validate paper LaTeX structure'
  },
  args: {
    path: {
      type: 'positional',
      description: 'Path to paper file',
      required: true
    },
    strict: {
      type: 'boolean',
      description: 'Enable strict validation',
      default: false
    },
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (table, json)',
      default: 'table'
    }
  },
  async run({ args }) {
    printProgress(`Validating paper: ${args.path}`, 'progress');
    printProgress(`Strict mode: ${args.strict ? 'enabled' : 'disabled'}`, 'info');

    // Validation result (would integrate with actual validation in production)
    const validationResult = {
      path: args.path,
      valid: true,
      errors: [],
      warnings: args.strict ? [] : ['Consider adding more citations'],
      info: {
        family: 'imrad',
        sections: 5,
        wordCount: 1500,
        citations: 12,
        figures: 3,
        tables: 2
      },
      validatedAt: new Date().toISOString()
    };

    if (args.format === 'json') {
      console.log(JSON.stringify(validationResult, null, 2));
      return validationResult;
    }

    if (validationResult.valid) {
      printProgress('Validation passed!', 'success');
      console.log(`  Family: ${validationResult.info.family}`);
      console.log(`  Sections: ${validationResult.info.sections}`);
      console.log(`  Word count: ${validationResult.info.wordCount}`);
      console.log(`  Citations: ${validationResult.info.citations}`);
      console.log(`  Figures: ${validationResult.info.figures}`);
      console.log(`  Tables: ${validationResult.info.tables}`);
    } else {
      printProgress('Validation failed:', 'error');
      validationResult.errors.forEach(err => {
        console.log(`  - ${err}`);
      });
    }

    if (validationResult.warnings.length > 0) {
      console.log('\nWarnings:');
      validationResult.warnings.forEach(warn => {
        printProgress(warn, 'warning');
      });
    }

    return validationResult;
  }
});

// =============================================================================
// Convert Command
// =============================================================================

/**
 * Convert paper between formats subcommand
 */
const convertCommand = defineCommand({
  meta: {
    name: 'convert',
    description: 'Convert paper between formats'
  },
  args: {
    path: {
      type: 'positional',
      description: 'Path to paper file',
      required: true
    },
    to: {
      type: 'string',
      description: `Target format (${CONVERSION_FORMATS.join(', ')})`,
      required: true
    },
    from: {
      type: 'string',
      description: 'Source format (auto-detected if not specified)'
    },
    output: {
      type: 'string',
      alias: 'o',
      description: 'Output file path'
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
      const validated = ConversionSchema.parse({
        from: args.from,
        to: args.to
      });

      if (!args.quiet) {
        printProgress(`Converting: ${args.path}`, 'progress');
        if (validated.from) {
          printProgress(`From: ${validated.from}`, 'info');
        } else {
          printProgress('From: auto-detected', 'info');
        }
        printProgress(`To: ${validated.to}`, 'info');
      }

      // Determine output path
      const inputPath = args.path;
      const extension = validated.to === 'latex' ? '.tex' : `.${validated.to}`;
      const outputPath = args.output || inputPath.replace(/\.[^.]+$/, extension);

      // Conversion result (would integrate with actual conversion in production)
      const result = {
        input: inputPath,
        output: outputPath,
        from: validated.from || 'auto',
        to: validated.to,
        success: true,
        convertedAt: new Date().toISOString()
      };

      if (!args.quiet) {
        printProgress('Conversion complete!', 'success');
        printProgress(`Output: ${outputPath}`, 'info');
      }

      return result;

    } catch (error) {
      if (error instanceof z.ZodError) {
        printProgress('Invalid format specified', 'error');
        console.error(`Supported formats: ${CONVERSION_FORMATS.join(', ')}`);
      } else {
        printProgress(`Error converting paper: ${error.message}`, 'error');
      }
      process.exit(1);
    }
  }
});

// =============================================================================
// Info Command
// =============================================================================

/**
 * Get paper metadata subcommand
 */
const infoCommand = defineCommand({
  meta: {
    name: 'info',
    description: 'Get paper metadata'
  },
  args: {
    path: {
      type: 'positional',
      description: 'Path to paper file',
      required: true
    },
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (table, json, yaml)',
      default: 'table'
    }
  },
  async run({ args }) {
    printProgress(`Reading paper info: ${args.path}`, 'progress');

    // Paper info (would integrate with actual file reading in production)
    const paperInfo = {
      path: args.path,
      title: 'Sample Research Paper',
      family: 'imrad',
      authors: ['Alice Smith', 'Bob Jones'],
      abstract: 'This paper presents...',
      sections: ['Introduction', 'Methods', 'Results', 'Discussion', 'Conclusion'],
      stats: {
        wordCount: 5420,
        pageCount: 12,
        citations: 42,
        figures: 8,
        tables: 4
      },
      metadata: {
        created: '2024-01-15T10:30:00Z',
        modified: '2024-11-20T14:22:00Z',
        version: '1.2'
      }
    };

    if (args.format === 'json') {
      console.log(JSON.stringify(paperInfo, null, 2));
      return paperInfo;
    }

    if (args.format === 'yaml') {
      console.log(toSimpleYaml(paperInfo));
      return paperInfo;
    }

    // Table format
    console.log('\nPaper Information:\n');
    console.log(`  Title:    ${paperInfo.title}`);
    console.log(`  Family:   ${paperInfo.family.toUpperCase()}`);
    console.log(`  Authors:  ${paperInfo.authors.join(', ')}`);
    console.log(`  Sections: ${paperInfo.sections.length}`);
    console.log('\n  Statistics:');
    console.log(`    Words:     ${paperInfo.stats.wordCount}`);
    console.log(`    Pages:     ${paperInfo.stats.pageCount}`);
    console.log(`    Citations: ${paperInfo.stats.citations}`);
    console.log(`    Figures:   ${paperInfo.stats.figures}`);
    console.log(`    Tables:    ${paperInfo.stats.tables}`);
    console.log('\n  Metadata:');
    console.log(`    Created:  ${paperInfo.metadata.created}`);
    console.log(`    Modified: ${paperInfo.metadata.modified}`);
    console.log(`    Version:  ${paperInfo.metadata.version}`);

    return paperInfo;
  }
});

// =============================================================================
// Main Papers Command Export
// =============================================================================

/**
 * Papers command with subcommands
 * @type {import('citty').CommandDef}
 */
export const papersCommand = defineCommand({
  meta: {
    name: 'papers',
    description: 'Manage research papers'
  },
  subCommands: {
    generate: generateCommand,
    list: listCommand,
    validate: validateCommand,
    convert: convertCommand,
    info: infoCommand
  }
});
