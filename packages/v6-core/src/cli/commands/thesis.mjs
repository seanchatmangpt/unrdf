/**
 * KGC Thesis Commands - Documentation thesis generation
 * @module @unrdf/v6-core/cli/commands/thesis
 */

import { resolve } from 'node:path';
import { readdir } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { buildThesis, renderFromOntology, exportThesis } from '../../docs/thesis-builder.mjs';

/**
 * Build thesis from package documentation
 * @param {Object} options - Command options
 * @param {string} [options.output='./thesis'] - Output directory
 * @param {string} [options.packages='packages/*'] - Package glob pattern
 * @param {boolean} [options.pdf=false] - Generate PDF
 * @returns {Promise<void>}
 */
export async function thesisBuild(options = {}) {
  const {
    output = './thesis',
    packages: packagesPattern = 'packages/*',
    pdf = false
  } = options;
  
  console.log('[kgc thesis build]');
  console.log(`Output: ${output}`);
  console.log(`Generate PDF: ${pdf}`);
  
  // Find package directories
  const packageDirs = await findPackages(packagesPattern);
  
  if (packageDirs.length === 0) {
    console.error('No packages found matching pattern:', packagesPattern);
    process.exit(1);
  }
  
  console.log(`Found ${packageDirs.length} packages`);
  
  // Build thesis
  const result = await buildThesis({
    packageDirs,
    outputDir: resolve(output),
    generatePDF: pdf,
    emitReceipts: true
  });
  
  console.log('\n=== Build Summary ===');
  console.log(`Packages: ${result.stats.packages}`);
  console.log(`Tutorials: ${result.stats.tutorials}`);
  console.log(`How-tos: ${result.stats.howtos}`);
  console.log(`Reference: ${result.stats.reference}`);
  console.log(`Explanation: ${result.stats.explanation}`);
  console.log(`Total docs: ${result.stats.totalDocs}`);
  console.log(`Duration: ${result.stats.duration}ms`);
  console.log(`Merkle root: ${result.stats.merkleRoot}`);
  console.log(`\nLaTeX: ${result.mainTexFile}`);
  
  if (result.pdfFile) {
    console.log(`PDF: ${result.pdfFile}`);
  }
  
  console.log(`\nReceipts: ${result.receipts.length} stages`);
}

/**
 * Render documentation from ontology
 * @param {Object} options - Command options
 * @param {string} options.ontology - Path to .ttl ontology
 * @param {string} [options.output='./rendered-docs'] - Output directory
 * @returns {Promise<void>}
 */
export async function thesisRender(options) {
  const { ontology, output = './rendered-docs' } = options;
  
  if (!ontology) {
    console.error('Error: --ontology required');
    process.exit(1);
  }
  
  console.log('[kgc thesis render]');
  console.log(`Ontology: ${ontology}`);
  console.log(`Output: ${output}`);
  
  const result = await renderFromOntology(resolve(ontology), resolve(output));
  
  console.log('Status:', result.status);
}

/**
 * Export thesis to specified format
 * @param {Object} options - Command options
 * @param {string} [options.format='pdf'] - Export format (pdf|latex|html)
 * @param {string} [options.input='./thesis'] - Thesis directory
 * @param {string} [options.output] - Output file path
 * @returns {Promise<void>}
 */
export async function thesisExport(options = {}) {
  const {
    format = 'pdf',
    input = './thesis',
    output
  } = options;
  
  console.log('[kgc thesis export]');
  console.log(`Format: ${format}`);
  console.log(`Input: ${input}`);
  
  const outputPath = output || `./thesis.${format}`;
  
  const result = await exportThesis(resolve(input), format, resolve(outputPath));
  
  console.log(`Exported: ${result}`);
}

/**
 * Find package directories matching pattern
 * @param {string} pattern - Glob pattern
 * @returns {Promise<string[]>} Package directory paths
 */
async function findPackages(pattern) {
  // Simple implementation: assume pattern is "packages/*"
  const baseDir = pattern.replace('/*', '');
  
  if (!existsSync(baseDir)) {
    return [];
  }
  
  const entries = await readdir(baseDir, { withFileTypes: true });
  const dirs = [];
  
  for (const entry of entries) {
    if (entry.isDirectory() && !entry.name.startsWith('.')) {
      const pkgDir = resolve(baseDir, entry.name);
      const pkgJsonPath = resolve(pkgDir, 'package.json');
      
      // Only include if has package.json
      if (existsSync(pkgJsonPath)) {
        dirs.push(pkgDir);
      }
    }
  }
  
  return dirs;
}

/**
 * Thesis extension for V6 CLI.
 */
import { z } from 'zod';

const BuildArgsSchema = z.object({
  output: z.string().optional().default('./thesis'),
  packages: z.string().optional().default('packages/*'),
  pdf: z.boolean().optional().default(false)
});

const RenderArgsSchema = z.object({
  ontology: z.string().describe('Path to .ttl ontology'),
  output: z.string().optional().default('./rendered-docs')
});

const ExportArgsSchema = z.object({
  format: z.enum(['pdf', 'latex', 'html']).optional().default('pdf'),
  input: z.string().optional().default('./thesis'),
  output: z.string().optional()
});

const ValidateArgsSchema = z.object({
  input: z.string().optional().default('./thesis')
});

export const thesisExtension = {
  id: '@unrdf/v6-core/thesis',
  nouns: {
    thesis: {
      description: 'Documentation and LaTeX thesis operations',
      verbs: {
        render: {
          description: 'Render documentation from ontology',
          handler: async (args) => {
            await thesisRender(args);
            return { rendered: true };
          },
          argsSchema: RenderArgsSchema,
          meta: {}
        },
        compile: {
          description: 'Build thesis from package documentation',
          handler: async (args) => {
            await thesisBuild(args);
            return { compiled: true };
          },
          argsSchema: BuildArgsSchema,
          meta: {}
        },
        validate: {
          description: 'Validate thesis structure',
          handler: async (args) => {
            // Validate thesis directory structure
            const { existsSync } = await import('fs');
            const exists = existsSync(args.input);
            return {
              valid: exists,
              path: args.input
            };
          },
          argsSchema: ValidateArgsSchema,
          meta: {}
        },
        export: {
          description: 'Export thesis to format',
          handler: async (args) => {
            await thesisExport(args);
            return { exported: true };
          },
          argsSchema: ExportArgsSchema,
          meta: {}
        }
      }
    }
  },
  priority: 100
};

export default thesisExtension;

