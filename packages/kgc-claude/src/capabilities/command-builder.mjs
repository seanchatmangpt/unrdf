/**
 * Command Builder - DSL for programmatic slash command creation
 *
 * Hyper-advanced capability for building Claude Code slash commands
 * programmatically with validation, type safety, and receipt generation.
 *
 * @module @unrdf/kgc-claude/capabilities/command-builder
 */

import { z } from 'zod';
import { writeFile, mkdir } from 'node:fs/promises';
import { dirname } from 'node:path';
import { blake3 } from 'hash-wasm';

/**
 * Argument schema for command parameters
 */
const ArgumentSchema = z.object({
  name: z.string().min(1).regex(/^[a-z][a-z0-9_]*$/i, 'Argument name must be alphanumeric with underscores'),
  description: z.string().min(1),
  required: z.boolean().default(false),
  default: z.string().optional(),
  type: z.enum(['string', 'number', 'boolean', 'array', 'object']).default('string'),
  validation: z.string().optional(), // Optional regex pattern
  examples: z.array(z.string()).optional(),
});

/**
 * Hook configuration schema
 */
const HookSchema = z.object({
  pre_execute: z.string().optional(),
  post_execute: z.string().optional(),
  on_error: z.string().optional(),
  on_success: z.string().optional(),
});

/**
 * Command metadata schema
 */
const CommandMetadataSchema = z.object({
  name: z.string().min(1).regex(/^[a-z][a-z0-9-]*$/i, 'Command name must be lowercase with hyphens'),
  description: z.string().min(1).max(200),
  version: z.string().regex(/^\d+\.\d+\.\d+$/).default('1.0.0'),
  arguments: z.array(ArgumentSchema).default([]),
  agents: z.array(z.string()).optional(),
  requires: z.array(z.string()).optional(),
  hooks: HookSchema.optional(),
  tags: z.array(z.string()).optional(),
  category: z.string().optional(),
  priority: z.enum(['low', 'medium', 'high', 'critical']).default('medium'),
});

/**
 * Command body section schema
 */
const CommandSectionSchema = z.object({
  title: z.string().min(1),
  content: z.string().min(1),
  level: z.number().int().min(1).max(6).default(2), // Heading level
  order: z.number().int().default(0),
});

/**
 * Complete command definition schema
 */
export const CommandDefinitionSchema = CommandMetadataSchema.extend({
  sections: z.array(CommandSectionSchema).default([]),
  examples: z.array(z.string()).optional(),
  relatedCommands: z.array(z.string()).optional(),
});

/**
 * @typedef {z.infer<typeof CommandDefinitionSchema>} CommandDefinition
 * @typedef {z.infer<typeof ArgumentSchema>} ArgumentDef
 * @typedef {z.infer<typeof CommandSectionSchema>} CommandSection
 */

/**
 * Command Builder - Fluent API for building commands
 *
 * @example
 * const cmd = new CommandBuilder('research-explore')
 *   .description('Systematically explore a capability area')
 *   .addArgument({ name: 'topic', description: 'The capability to explore', required: true })
 *   .addSection({ title: 'Objectives', content: '1. Discover features\n2. Test edge cases' })
 *   .setVersion('1.0.0')
 *   .build();
 */
export class CommandBuilder {
  /**
   * @param {string} name - Command name (e.g., 'research-explore')
   */
  constructor(name) {
    /** @type {Partial<CommandDefinition>} */
    this.command = {
      name,
      description: '',
      version: '1.0.0',
      arguments: [],
      sections: [],
      priority: 'medium',
    };
  }

  /**
   * Set command description
   * @param {string} description
   * @returns {CommandBuilder}
   */
  description(description) {
    this.command.description = description;
    return this;
  }

  /**
   * Set command version
   * @param {string} version - Semver version string
   * @returns {CommandBuilder}
   */
  setVersion(version) {
    this.command.version = version;
    return this;
  }

  /**
   * Add an argument to the command
   * @param {ArgumentDef} arg - Argument definition
   * @returns {CommandBuilder}
   */
  addArgument(arg) {
    this.command.arguments = this.command.arguments || [];
    this.command.arguments.push(arg);
    return this;
  }

  /**
   * Add multiple arguments at once
   * @param {ArgumentDef[]} args - Array of argument definitions
   * @returns {CommandBuilder}
   */
  addArguments(args) {
    args.forEach((arg) => this.addArgument(arg));
    return this;
  }

  /**
   * Add a content section
   * @param {CommandSection} section - Section definition
   * @returns {CommandBuilder}
   */
  addSection(section) {
    this.command.sections = this.command.sections || [];
    this.command.sections.push(section);
    return this;
  }

  /**
   * Add agents to be used by this command
   * @param {string[]} agents - Agent names
   * @returns {CommandBuilder}
   */
  setAgents(agents) {
    this.command.agents = agents;
    return this;
  }

  /**
   * Add dependencies/requirements
   * @param {string[]} requires - Package or module dependencies
   * @returns {CommandBuilder}
   */
  setRequires(requires) {
    this.command.requires = requires;
    return this;
  }

  /**
   * Configure hooks
   * @param {z.infer<typeof HookSchema>} hooks - Hook configuration
   * @returns {CommandBuilder}
   */
  setHooks(hooks) {
    this.command.hooks = hooks;
    return this;
  }

  /**
   * Set command tags for categorization
   * @param {string[]} tags - Tags
   * @returns {CommandBuilder}
   */
  setTags(tags) {
    this.command.tags = tags;
    return this;
  }

  /**
   * Set command category
   * @param {string} category - Category name
   * @returns {CommandBuilder}
   */
  setCategory(category) {
    this.command.category = category;
    return this;
  }

  /**
   * Set command priority
   * @param {'low' | 'medium' | 'high' | 'critical'} priority
   * @returns {CommandBuilder}
   */
  setPriority(priority) {
    this.command.priority = priority;
    return this;
  }

  /**
   * Add example usage
   * @param {string} example - Example command or code
   * @returns {CommandBuilder}
   */
  addExample(example) {
    this.command.examples = this.command.examples || [];
    this.command.examples.push(example);
    return this;
  }

  /**
   * Link to related commands
   * @param {string[]} commands - Related command names
   * @returns {CommandBuilder}
   */
  setRelatedCommands(commands) {
    this.command.relatedCommands = commands;
    return this;
  }

  /**
   * Validate and build the command definition
   * @returns {CommandDefinition}
   * @throws {z.ZodError} If validation fails
   */
  build() {
    return CommandDefinitionSchema.parse(this.command);
  }
}

/**
 * Render command definition to markdown format
 *
 * @param {CommandDefinition} cmd - Command definition
 * @returns {string} Markdown content
 */
export function renderCommandMarkdown(cmd) {
  const lines = [];

  // YAML Frontmatter
  lines.push('---');
  lines.push(`name: ${cmd.name}`);
  lines.push(`description: ${cmd.description}`);
  if (cmd.version) lines.push(`version: ${cmd.version}`);

  if (cmd.arguments && cmd.arguments.length > 0) {
    lines.push('arguments:');
    cmd.arguments.forEach((arg) => {
      lines.push(`  - name: ${arg.name}`);
      lines.push(`    description: '${arg.description.replace(/'/g, "''")}'`);
      lines.push(`    required: ${arg.required}`);
      if (arg.default !== undefined) {
        lines.push(`    default: '${arg.default}'`);
      }
      if (arg.type !== 'string') {
        lines.push(`    type: ${arg.type}`);
      }
      if (arg.validation) {
        lines.push(`    validation: '${arg.validation}'`);
      }
    });
  }

  if (cmd.agents && cmd.agents.length > 0) {
    lines.push('agents:');
    cmd.agents.forEach((agent) => lines.push(`  - ${agent}`));
  }

  if (cmd.requires && cmd.requires.length > 0) {
    lines.push('requires:');
    cmd.requires.forEach((req) => lines.push(`  - '${req}'`));
  }

  if (cmd.hooks) {
    lines.push('hooks:');
    if (cmd.hooks.pre_execute) lines.push(`  pre_execute: ${cmd.hooks.pre_execute}`);
    if (cmd.hooks.post_execute) lines.push(`  post_execute: ${cmd.hooks.post_execute}`);
    if (cmd.hooks.on_error) lines.push(`  on_error: ${cmd.hooks.on_error}`);
    if (cmd.hooks.on_success) lines.push(`  on_success: ${cmd.hooks.on_success}`);
  }

  if (cmd.tags && cmd.tags.length > 0) {
    lines.push(`tags: [${cmd.tags.map((t) => `'${t}'`).join(', ')}]`);
  }

  if (cmd.category) lines.push(`category: ${cmd.category}`);
  if (cmd.priority) lines.push(`priority: ${cmd.priority}`);

  lines.push('---');
  lines.push('');

  // Main title
  lines.push(`# ${cmd.description}`);
  lines.push('');

  // Sections (sorted by order)
  const sortedSections = [...(cmd.sections || [])].sort((a, b) => a.order - b.order);
  sortedSections.forEach((section) => {
    const headingMarker = '#'.repeat(section.level);
    lines.push(`${headingMarker} ${section.title}`);
    lines.push('');
    lines.push(section.content);
    lines.push('');
  });

  // Examples section
  if (cmd.examples && cmd.examples.length > 0) {
    lines.push('## Examples');
    lines.push('');
    cmd.examples.forEach((example) => {
      lines.push('```bash');
      lines.push(example);
      lines.push('```');
      lines.push('');
    });
  }

  // Related commands
  if (cmd.relatedCommands && cmd.relatedCommands.length > 0) {
    lines.push('## Related Commands');
    lines.push('');
    cmd.relatedCommands.forEach((relCmd) => {
      lines.push(`- \`/${relCmd}\``);
    });
    lines.push('');
  }

  // Argument documentation
  if (cmd.arguments && cmd.arguments.length > 0) {
    lines.push('## Arguments');
    lines.push('');
    lines.push('| Name | Type | Required | Default | Description |');
    lines.push('|------|------|----------|---------|-------------|');
    cmd.arguments.forEach((arg) => {
      const defaultVal = arg.default !== undefined ? `\`${arg.default}\`` : '-';
      const required = arg.required ? '✓' : '';
      lines.push(
        `| \`${arg.name}\` | ${arg.type || 'string'} | ${required} | ${defaultVal} | ${arg.description} |`,
      );
    });
    lines.push('');
  }

  return lines.join('\n');
}

/**
 * Generate variable substitution map from arguments
 *
 * @param {CommandDefinition} cmd - Command definition
 * @returns {Map<string, string>} Map of $variable -> default value
 */
export function generateSubstitutionMap(cmd) {
  const map = new Map();
  if (cmd.arguments) {
    cmd.arguments.forEach((arg) => {
      const varName = `$${arg.name}`;
      const value = arg.default || `<${arg.name}>`;
      map.set(varName, value);
    });
  }
  return map;
}

/**
 * Compute content hash for command (for receipt generation)
 *
 * @param {CommandDefinition} cmd - Command definition
 * @returns {Promise<string>} BLAKE3 hash
 */
export async function computeCommandHash(cmd) {
  const normalized = {
    name: cmd.name,
    version: cmd.version,
    arguments: cmd.arguments?.map((a) => ({
      name: a.name,
      required: a.required,
      type: a.type,
    })),
    sections: cmd.sections?.map((s) => ({
      title: s.title,
      order: s.order,
    })),
  };
  const serialized = JSON.stringify(normalized, Object.keys(normalized).sort());
  return blake3(serialized);
}

/**
 * Write command to filesystem
 *
 * @param {CommandDefinition} cmd - Command definition
 * @param {string} basePath - Base directory (e.g., '.claude/commands')
 * @param {string} [namespace] - Optional namespace subdirectory
 * @returns {Promise<{ path: string, hash: string }>}
 */
export async function writeCommand(cmd, basePath, namespace = '') {
  const markdown = renderCommandMarkdown(cmd);
  const hash = await computeCommandHash(cmd);

  const dir = namespace ? `${basePath}/${namespace}` : basePath;
  const path = `${dir}/${cmd.name}.md`;

  // Ensure directory exists
  await mkdir(dirname(path), { recursive: true });

  // Write file
  await writeFile(path, markdown, 'utf-8');

  return { path, hash };
}

/**
 * Batch command builder for creating multiple related commands
 *
 * @example
 * const batch = new BatchCommandBuilder('.claude/commands/research');
 * batch.add(new CommandBuilder('explore').description('...').build());
 * batch.add(new CommandBuilder('validate').description('...').build());
 * await batch.writeAll();
 */
export class BatchCommandBuilder {
  /**
   * @param {string} basePath - Base directory
   * @param {string} [namespace] - Optional namespace
   */
  constructor(basePath, namespace = '') {
    this.basePath = basePath;
    this.namespace = namespace;
    /** @type {CommandDefinition[]} */
    this.commands = [];
  }

  /**
   * Add a command definition
   * @param {CommandDefinition} cmd
   * @returns {BatchCommandBuilder}
   */
  add(cmd) {
    this.commands.push(cmd);
    return this;
  }

  /**
   * Write all commands to filesystem
   * @returns {Promise<{ commands: Array<{ name: string, path: string, hash: string }> }>}
   */
  async writeAll() {
    const results = await Promise.all(
      this.commands.map(async (cmd) => {
        const { path, hash } = await writeCommand(cmd, this.basePath, this.namespace);
        return { name: cmd.name, path, hash };
      }),
    );

    return { commands: results };
  }

  /**
   * Get summary of batch
   * @returns {{ total: number, categories: Record<string, number> }}
   */
  getSummary() {
    const categories = {};
    this.commands.forEach((cmd) => {
      const cat = cmd.category || 'uncategorized';
      categories[cat] = (categories[cat] || 0) + 1;
    });

    return {
      total: this.commands.length,
      categories,
    };
  }
}

/**
 * Quick helper to create a command
 *
 * @param {string} name - Command name
 * @returns {CommandBuilder}
 */
export function command(name) {
  return new CommandBuilder(name);
}
