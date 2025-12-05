/**
 * @file Output format validation
 * @module cli/utils/output-format
 *
 * Validates and normalizes output format arguments
 * FM-CLI-014: Invalid output format
 */

import { z } from 'zod';

/**
 * Supported output formats
 */
export const SUPPORTED_FORMATS = {
  table: {
    description: 'Human-readable table format',
    aliases: ['t', 'text', 'ascii']
  },
  json: {
    description: 'JSON format',
    aliases: ['j']
  },
  yaml: {
    description: 'YAML format',
    aliases: ['y', 'yml']
  },
  csv: {
    description: 'CSV format',
    aliases: ['c']
  },
  jsonl: {
    description: 'JSON Lines format (one JSON object per line)',
    aliases: ['ndjson', 'newline-delimited-json']
  },
  turtle: {
    description: 'Turtle RDF format',
    aliases: ['ttl']
  },
  ntriples: {
    description: 'N-Triples format',
    aliases: ['nt', 'ntriple']
  },
  nquads: {
    description: 'N-Quads format',
    aliases: ['nq', 'nquad']
  }
};

/**
 * Output format schema
 */
export const outputFormatSchema = z.enum([
  'table',
  'json',
  'yaml',
  'csv',
  'jsonl',
  'turtle',
  'ntriples',
  'nquads'
]);

/**
 * Validate and normalize output format
 */
export function validateOutputFormat(format, allowedFormats = null) {
  // Handle null/undefined
  if (!format) {
    return {
      valid: true,
      format: 'table',
      normalized: 'table',
      note: 'Using default format: table'
    };
  }

  // Convert to lowercase
  const normalized = String(format).toLowerCase().trim();

  // Check if it's a direct match
  if (SUPPORTED_FORMATS[normalized]) {
    // Check allowed list if provided
    if (allowedFormats && !allowedFormats.includes(normalized)) {
      return {
        valid: false,
        format: normalized,
        error: `Format "${normalized}" is not allowed for this command`,
        suggestion: `Allowed formats: ${allowedFormats.join(', ')}`
      };
    }

    return {
      valid: true,
      format: normalized,
      normalized
    };
  }

  // Check aliases
  for (const [formatName, config] of Object.entries(SUPPORTED_FORMATS)) {
    if (config.aliases.includes(normalized)) {
      // Check allowed list if provided
      if (allowedFormats && !allowedFormats.includes(formatName)) {
        return {
          valid: false,
          format: normalized,
          resolved: formatName,
          error: `Format "${formatName}" is not allowed for this command`,
          suggestion: `Allowed formats: ${allowedFormats.join(', ')}`
        };
      }

      return {
        valid: true,
        format: formatName,
        normalized: formatName,
        alias: normalized,
        note: `Using format: ${formatName}`
      };
    }
  }

  // Format not found
  const validFormats = Object.keys(SUPPORTED_FORMATS).join(', ');
  return {
    valid: false,
    format: normalized,
    error: `Unknown output format: "${normalized}"`,
    suggestion: `Valid formats: ${validFormats}`
  };
}

/**
 * Get supported formats list
 */
export function getSupportedFormats() {
  return Object.entries(SUPPORTED_FORMATS).map(([name, config]) => ({
    name,
    description: config.description,
    aliases: config.aliases
  }));
}

/**
 * Format validation result for user display
 */
export function formatOutputFormatError(validation) {
  if (validation.valid) {
    return '';
  }

  const lines = [];
  lines.push('');
  lines.push(`❌ ${validation.error}`);
  lines.push('');
  lines.push('📋 Supported Output Formats:');

  getSupportedFormats().forEach(fmt => {
    const aliases = fmt.aliases.length > 0
      ? ` (aliases: ${fmt.aliases.join(', ')})`
      : '';
    lines.push(`   • ${fmt.name}${aliases} - ${fmt.description}`);
  });

  if (validation.suggestion) {
    lines.push('');
    lines.push(`📖 ${validation.suggestion}`);
  }

  lines.push('');

  return lines.join('\n');
}

/**
 * Validate format against command's allowed formats
 */
export function validateFormatForCommand(format, commandName) {
  // Define allowed formats per command
  const commandFormats = {
    'store query': ['json', 'jsonl', 'csv', 'table'],
    'hook list': ['json', 'table', 'yaml'],
    'graph list': ['json', 'table', 'yaml'],
    'policy list': ['json', 'table', 'yaml'],
    'store import': ['json', 'table'],
    'sidecar health': ['json', 'yaml', 'table']
  };

  const allowed = commandFormats[commandName];
  if (!allowed) {
    // Command not in list, allow all
    return validateOutputFormat(format);
  }

  return validateOutputFormat(format, allowed);
}

export default {
  SUPPORTED_FORMATS,
  outputFormatSchema,
  validateOutputFormat,
  getSupportedFormats,
  formatOutputFormatError,
  validateFormatForCommand
};
