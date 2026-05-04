/**
 * @fileoverview Π_cli - Command-line Interface Projections
 * Transforms KGC data structures into CLI-friendly formats with ANSI colors and tables
 */

import { z } from 'zod';

/**
 * ANSI color codes for terminal output
 * @constant
 */
const COLORS = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  dim: '\x1b[2m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
  white: '\x1b[37m',
};

/**
 * CLI projection schema
 */
export const CLIProjectionSchema = z.object({
  type: z.literal('cli'),
  format: z.enum(['table', 'list', 'tree', 'summary']),
  colored: z.boolean().default(true),
  content: z.string(),
  metadata: z.record(z.any()).optional(),
});

/**
 * @typedef {z.infer<typeof CLIProjectionSchema>} CLIProjection
 */

/**
 * Colorize text with ANSI codes
 * @param {string} text - Text to colorize
 * @param {keyof typeof COLORS} color - Color name
 * @param {boolean} colored - Whether to apply colors
 * @returns {string} Colorized text
 */
function colorize(text, color, colored = true) {
  if (!colored) return text;
  return `${COLORS[color]}${text}${COLORS.reset}`;
}

/**
 * Project receipt to CLI-friendly format
 * @param {import('./receipt.mjs').Receipt} receipt - Receipt to project
 * @param {boolean} [colored=true] - Whether to use ANSI colors
 * @returns {CLIProjection} CLI projection
 */
export function projectReceiptToCLI(receipt, colored = true) {
  const lines = [
    colorize('━'.repeat(60), 'cyan', colored),
    colorize('Receipt:', 'bright', colored) + ` ${receipt.id}`,
    colorize('Operation:', 'blue', colored) + ` ${receipt.operation}`,
    colorize('Timestamp:', 'dim', colored) + ` ${receipt.timestamp}`,
    colorize('Hash:', 'magenta', colored) + ` ${receipt.hash.slice(0, 16)}...`,
  ];

  if (receipt.parentHash) {
    lines.push(colorize('Parent:', 'dim', colored) + ` ${receipt.parentHash.slice(0, 16)}...`);
  }

  lines.push(colorize('━'.repeat(60), 'cyan', colored));

  const content = lines.join('\n');

  return CLIProjectionSchema.parse({
    type: 'cli',
    format: 'summary',
    colored,
    content,
    metadata: { receiptId: receipt.id },
  });
}

/**
 * Project work item to CLI table format
 * @param {Array<{id: string, goal: string, state: string, priority?: number}>} workItems - Work items
 * @param {boolean} [colored=true] - Whether to use ANSI colors
 * @returns {CLIProjection} CLI projection
 */
export function projectWorkItemsToCLI(workItems, colored = true) {
  if (!Array.isArray(workItems) || workItems.length === 0) {
    return CLIProjectionSchema.parse({
      type: 'cli',
      format: 'table',
      colored,
      content: colorize('No work items', 'dim', colored),
    });
  }

  // Table header
  const header = [
    colorize('ID', 'bright', colored).padEnd(colored ? 25 : 15),
    colorize('Goal', 'bright', colored).padEnd(colored ? 45 : 35),
    colorize('State', 'bright', colored).padEnd(colored ? 20 : 10),
    colorize('Priority', 'bright', colored),
  ].join(' │ ');

  const separator = colorize('─'.repeat(100), 'dim', colored);

  // Table rows
  const rows = workItems.map((item) => {
    const stateColor =
      item.state === 'completed' ? 'green' :
      item.state === 'failed' ? 'red' :
      item.state === 'running' ? 'yellow' : 'dim';

    return [
      item.id.slice(0, 12).padEnd(15),
      item.goal.slice(0, 32).padEnd(35),
      colorize(item.state.padEnd(10), stateColor, colored),
      (item.priority ?? 0).toString(),
    ].join(' │ ');
  });

  const content = [header, separator, ...rows, separator].join('\n');

  return CLIProjectionSchema.parse({
    type: 'cli',
    format: 'table',
    colored,
    content,
    metadata: { count: workItems.length },
  });
}

/**
 * Project nested structure to tree format
 * @param {Record<string, any>} data - Nested data structure
 * @param {boolean} [colored=true] - Whether to use ANSI colors
 * @param {string} [prefix=''] - Internal prefix for recursion
 * @param {boolean} [isLast=true] - Internal flag for recursion
 * @returns {string} Tree representation
 */
function renderTree(data, colored = true, prefix = '', isLast = true) {
  const lines = [];
  const entries = Object.entries(data);

  entries.forEach(([key, value], index) => {
    const isLastEntry = index === entries.length - 1;
    const connector = isLast ? '└── ' : '├── ';
    const extension = isLast ? '    ' : '│   ';

    if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
      lines.push(prefix + connector + colorize(key, 'cyan', colored));
      lines.push(renderTree(value, colored, prefix + extension, isLastEntry));
    } else {
      const displayValue = Array.isArray(value) ? `[${value.length} items]` : String(value);
      lines.push(
        prefix + connector +
        colorize(key, 'blue', colored) + ': ' +
        colorize(displayValue, 'white', colored)
      );
    }
  });

  return lines.join('\n');
}

/**
 * Project object to tree format
 * @param {Record<string, any>} obj - Object to project
 * @param {boolean} [colored=true] - Whether to use ANSI colors
 * @returns {CLIProjection} CLI projection
 */
export function projectObjectToTree(obj, colored = true) {
  const content = renderTree(obj, colored);

  return CLIProjectionSchema.parse({
    type: 'cli',
    format: 'tree',
    colored,
    content,
  });
}

/**
 * Project array to numbered list
 * @param {Array<string | {label: string, description?: string}>} items - Items to project
 * @param {boolean} [colored=true] - Whether to use ANSI colors
 * @returns {CLIProjection} CLI projection
 */
export function projectArrayToList(items, colored = true) {
  const lines = items.map((item, index) => {
    const num = colorize(`${index + 1}.`, 'cyan', colored);

    if (typeof item === 'string') {
      return `${num} ${item}`;
    }

    const label = colorize(item.label, 'bright', colored);
    const desc = item.description ?
      '\n   ' + colorize(item.description, 'dim', colored) : '';

    return `${num} ${label}${desc}`;
  });

  const content = lines.join('\n');

  return CLIProjectionSchema.parse({
    type: 'cli',
    format: 'list',
    colored,
    content,
    metadata: { count: items.length },
  });
}

/**
 * Flatten nested object for CLI display
 * @param {Record<string, any>} obj - Object to flatten
 * @param {string} [prefix=''] - Key prefix for recursion
 * @returns {Record<string, any>} Flattened object
 */
export function flattenForCLI(obj, prefix = '') {
  const flattened = {};

  for (const [key, value] of Object.entries(obj)) {
    const fullKey = prefix ? `${prefix}.${key}` : key;

    if (value !== null && typeof value === 'object' && !Array.isArray(value)) {
      Object.assign(flattened, flattenForCLI(value, fullKey));
    } else {
      flattened[fullKey] = value;
    }
  }

  return flattened;
}
