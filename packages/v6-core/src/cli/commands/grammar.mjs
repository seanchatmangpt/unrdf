/**
 * @fileoverview CLI Commands for Grammar Operations
 *
 * Commands:
 * - kgc grammar compile <file> - Compile grammar with complexity check
 * - kgc grammar validate <file> - Validate syntax only
 * - kgc grammar complexity <file> - Show complexity analysis
 *
 * @module @unrdf/v6-core/cli/commands/grammar
 * @version 6.0.0-alpha.1
 */

import { readFile } from 'fs/promises';
import { parseGrammar, getComplexityBounds } from '../../grammar/parser.mjs';
import { compileGrammar } from '../../grammar/compiler.mjs';

/**
 * Grammar compile command
 *
 * @param {string} filePath - Path to grammar file
 * @param {Object} [options] - Command options
 * @param {string} [options.type] - Grammar type (sparql|shacl|n3|owl|shex)
 * @param {boolean} [options.strict] - Strict bounds enforcement
 * @returns {Promise<void>}
 */
export async function grammarCompileCommand(filePath, options = {}) {
  try {
    // Read input file
    const input = await readFile(filePath, 'utf-8');

    // Detect grammar type if not provided
    const grammarType = options.type || detectGrammarType(filePath, input);

    console.log(`Compiling ${grammarType} grammar from ${filePath}...`);

    // Parse
    const parseResult = parseGrammar(input, grammarType);
    if (!parseResult.success) {
      console.error('Parse failed:');
      parseResult.errors.forEach(err => {
        console.error(`  - ${err.message}`, err.line ? `(line ${err.line})` : '');
      });
      process.exit(1);
    }

    console.log('✓ Parse successful');
    console.log(`  AST nodes: ${parseResult.complexity.astNodeCount}`);
    console.log(`  Estimated time: ${parseResult.complexity.estimatedTimeMs}ms`);

    // Compile
    const compileResult = compileGrammar(parseResult.ast, {
      strict: options.strict !== false,
    });

    if (!compileResult.success) {
      console.error('\n✗ Compilation DENIED:');
      console.error(`  Reason: ${compileResult.denial.reason}`);
      if (compileResult.denialReceipt) {
        console.error(`  Constraint: ${compileResult.denialReceipt.details.constraint}`);
        console.error(`  Limit: ${compileResult.denialReceipt.details.limit}`);
        console.error(`  Actual: ${compileResult.denialReceipt.details.actual}`);
        console.error(`  Suggestion: ${compileResult.denialReceipt.details.suggestion}`);
        console.error(`\nDenial Receipt:`);
        console.error(`  Merkle Proof: ${compileResult.denialReceipt.merkleProof}`);
        console.error(`  Timestamp: ${compileResult.denialReceipt.timestamp}`);
      }
      process.exit(1);
    }

    console.log('\n✓ Compilation ACCEPTED');
    console.log(`  Compile time: ${compileResult.compileReceipt.compileTimeMs}ms`);
    console.log(`  Decision: ${compileResult.compileReceipt.decision}`);
    console.log(`  Grammar type: ${compileResult.compileReceipt.grammarType}`);

    if (options.output) {
      const outputData = JSON.stringify(compileResult.compiled, null, 2);
      await writeFile(options.output, outputData);
      console.log(`\nCompiled output written to: ${options.output}`);
    }

    console.log('\n✓ Grammar compilation complete');
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

/**
 * Grammar validate command (syntax check only)
 *
 * @param {string} filePath - Path to grammar file
 * @param {Object} [options] - Command options
 * @returns {Promise<void>}
 */
export async function grammarValidateCommand(filePath, options = {}) {
  try {
    const input = await readFile(filePath, 'utf-8');
    const grammarType = options.type || detectGrammarType(filePath, input);

    console.log(`Validating ${grammarType} syntax in ${filePath}...`);

    const parseResult = parseGrammar(input, grammarType);

    if (!parseResult.success) {
      console.error('\n✗ Validation FAILED:');
      parseResult.errors.forEach(err => {
        console.error(`  - ${err.message}`, err.line ? `(line ${err.line})` : '');
      });
      process.exit(1);
    }

    console.log('\n✓ Syntax validation PASSED');
    console.log(`  Parser: ${parseResult.parseReceipt.parser}`);
    console.log(`  Grammar version: ${parseResult.parseReceipt.grammarVersion}`);
    console.log(`  Timestamp: ${parseResult.parseReceipt.timestamp}`);
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

/**
 * Grammar complexity command (analysis only)
 *
 * @param {string} filePath - Path to grammar file
 * @param {Object} [options] - Command options
 * @returns {Promise<void>}
 */
export async function grammarComplexityCommand(filePath, options = {}) {
  try {
    const input = await readFile(filePath, 'utf-8');
    const grammarType = options.type || detectGrammarType(filePath, input);

    console.log(`Analyzing ${grammarType} complexity in ${filePath}...\n`);

    const parseResult = parseGrammar(input, grammarType);

    if (!parseResult.success) {
      console.error('Parse failed - cannot analyze complexity');
      process.exit(1);
    }

    const complexity = parseResult.complexity;

    console.log('Complexity Analysis:');
    console.log('===================');
    console.log(`Grammar Type: ${grammarType}`);
    console.log(`AST Nodes: ${complexity.astNodeCount}`);
    console.log(`Max Depth: ${complexity.maxDepth}`);
    console.log(`Estimated Time: ${complexity.estimatedTimeMs}ms`);

    // Grammar-specific metrics
    if (complexity.triplePatterns !== undefined) {
      console.log(`\nSPARQL Metrics:`);
      console.log(`  Triple Patterns: ${complexity.triplePatterns}`);
      console.log(`  Join Depth: ${complexity.joinDepth}`);
      console.log(`  Filter Complexity: ${complexity.filterComplexity}`);
    }

    if (complexity.shapesDepth !== undefined) {
      console.log(`\nSHACL Metrics:`);
      console.log(`  Shapes Depth: ${complexity.shapesDepth}`);
    }

    if (complexity.ruleDepth !== undefined) {
      console.log(`\nN3 Metrics:`);
      console.log(`  Rule Depth: ${complexity.ruleDepth}`);
    }

    // Check against bounds
    const { COMPLEXITY_BOUNDS } = await import('../../grammar/compiler.mjs');
    const bounds = COMPLEXITY_BOUNDS[grammarType];

    if (bounds) {
      console.log(`\nComplexity Bounds Check:`);
      console.log(`========================`);

      const checks = [];

      if (bounds.estimatedTimeMs) {
        const withinBounds = complexity.estimatedTimeMs <= bounds.estimatedTimeMs;
        checks.push({
          metric: 'Estimated Time',
          value: complexity.estimatedTimeMs,
          limit: bounds.estimatedTimeMs,
          status: withinBounds ? '✓' : '✗',
        });
      }

      if (bounds.maxTriplePatterns && complexity.triplePatterns !== undefined) {
        const withinBounds = complexity.triplePatterns <= bounds.maxTriplePatterns;
        checks.push({
          metric: 'Triple Patterns',
          value: complexity.triplePatterns,
          limit: bounds.maxTriplePatterns,
          status: withinBounds ? '✓' : '✗',
        });
      }

      checks.forEach(check => {
        const pct = ((check.value / check.limit) * 100).toFixed(1);
        console.log(`  ${check.status} ${check.metric}: ${check.value}/${check.limit} (${pct}%)`);
      });

      const allPass = checks.every(c => c.status === '✓');
      console.log(`\nOverall: ${allPass ? '✓ WITHIN BOUNDS' : '✗ EXCEEDS BOUNDS'}`);
    }
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

/**
 * Detect grammar type from file extension or content
 *
 * @param {string} filePath - File path
 * @param {string} content - File content
 * @returns {string} Grammar type
 */
function detectGrammarType(filePath, content) {
  // Check file extension
  if (filePath.endsWith('.sparql') || filePath.endsWith('.rq')) {
    return 'sparql';
  }
  if (filePath.endsWith('.shacl') || filePath.endsWith('.ttl')) {
    if (content.includes('sh:NodeShape') || content.includes('shacl')) {
      return 'shacl';
    }
  }
  if (filePath.endsWith('.n3')) {
    return 'n3';
  }
  if (filePath.endsWith('.owl')) {
    return 'owl';
  }
  if (filePath.endsWith('.shex')) {
    return 'shex';
  }

  // Check content
  if (content.includes('SELECT') || content.includes('CONSTRUCT')) {
    return 'sparql';
  }
  if (content.includes('sh:NodeShape')) {
    return 'shacl';
  }
  if (content.includes('=>')) {
    return 'n3';
  }
  if (content.includes('owl:Class')) {
    return 'owl';
  }

  // Default to SPARQL
  return 'sparql';
}

/**
 * CLI command exports for KGC
 */
export const grammarCommands = {
  compile: grammarCompileCommand,
  validate: grammarValidateCommand,
  complexity: grammarComplexityCommand,
};

/**
 * Grammar extension for V6 CLI.
 */
import { z } from 'zod';

const CompileArgsSchema = z.object({
  file: z.string().describe('Grammar file path'),
  type: z.enum(['sparql', 'shacl', 'n3', 'owl', 'shex']).optional(),
  strict: z.boolean().optional().default(true),
  output: z.string().optional()
});

const ValidateArgsSchema = z.object({
  file: z.string().describe('Grammar file path'),
  type: z.enum(['sparql', 'shacl', 'n3', 'owl', 'shex']).optional()
});

const ParseArgsSchema = z.object({
  file: z.string().describe('Grammar file path'),
  type: z.enum(['sparql', 'shacl', 'n3', 'owl', 'shex']).optional()
});

export const grammarExtension = {
  id: '@unrdf/v6-core/grammar',
  nouns: {
    grammar: {
      description: 'SPARQL/SHACL/N3/OWL grammar operations',
      verbs: {
        compile: {
          description: 'Compile grammar with complexity bounds checking',
          handler: async (args) => {
            await grammarCompileCommand(args.file, args);
            return { compiled: true };
          },
          argsSchema: CompileArgsSchema,
          meta: {}
        },
        validate: {
          description: 'Validate grammar syntax',
          handler: async (args) => {
            await grammarValidateCommand(args.file, args);
            return { valid: true };
          },
          argsSchema: ValidateArgsSchema,
          meta: {}
        },
        parse: {
          description: 'Parse grammar and show AST',
          handler: async (args) => {
            const { readFile } = await import('fs/promises');
            const input = await readFile(args.file, 'utf-8');
            const grammarType = args.type || detectGrammarType(args.file, input);
            const parseResult = parseGrammar(input, grammarType);
            return {
              parsed: parseResult.success,
              grammarType,
              complexity: parseResult.complexity
            };
          },
          argsSchema: ParseArgsSchema,
          meta: {}
        },
        export: {
          description: 'Export grammar analysis',
          handler: async (args) => {
            await grammarComplexityCommand(args.file, args);
            return { exported: true };
          },
          argsSchema: z.object({
            file: z.string(),
            type: z.enum(['sparql', 'shacl', 'n3', 'owl', 'shex']).optional()
          }),
          meta: {}
        }
      }
    }
  },
  priority: 100
};

export default grammarExtension;
