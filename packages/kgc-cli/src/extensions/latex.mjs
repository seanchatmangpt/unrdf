/**
 * @fileoverview LaTeX CLI extension - PDF compilation using internal WASM engine.
 *
 * Provides commands for:
 * - Building LaTeX documents to PDF (no external TeX required)
 * - Validating LaTeX structure
 * - Managing compilation cache
 */

import { z } from 'zod';
import { compileLatexToPdf } from '../lib/latex/compile.mjs';

/** Args schema for latex build command */
const BuildSchema = z.object({
  input: z.string().describe('Path to main .tex file (e.g., thesis/main.tex)'),
  output: z.string().optional().default('dist/thesis.pdf').describe('Output PDF path'),
  engine: z.enum(['pdflatex', 'xelatex', 'lualatex']).optional().default('pdflatex').describe('LaTeX engine to use'),
  cacheDir: z.string().optional().default('.latex-cache').describe('Cache directory'),
  passes: z.number().int().min(1).max(5).optional().default(2).describe('Number of compilation passes'),
  projectRoot: z.string().optional().describe('Project root directory (defaults to dirname(input))')
});

/** Args schema for cache management */
const _CacheClearSchema = z.object({
  cacheDir: z.string().optional().default('.kgc/cache/latex').describe('Cache directory to clear')
});

/**
 * LaTeX extension definition.
 * @type {Object}
 */
const extension = {
  id: '@unrdf/latex',
  description: 'LaTeX compilation using internal WASM engine',

  nouns: {
    latex: {
      description: 'LaTeX document compilation and management',
      verbs: {
        build: {
          description: 'Compile LaTeX document to PDF using internal WASM engine',
          argsSchema: BuildSchema,
          handler: async (args) => {
            const { promises: fs } = await import('node:fs');
            const { dirname, resolve } = await import('node:path');

            // Resolve paths
            const inputTexPath = resolve(args.input);
            const projectRoot = args.projectRoot || dirname(inputTexPath);
            const outputPath = resolve(args.output);

            try {
              // Call Agent 10's compile module
              const pdfBytes = await compileLatexToPdf({
                inputTexPath,
                projectDir: projectRoot,
                engine: args.engine,
                cacheDir: args.cacheDir ? resolve(args.cacheDir) : undefined,
                passes: args.passes
              });

              // Write output PDF
              await fs.mkdir(dirname(outputPath), { recursive: true });
              await fs.writeFile(outputPath, pdfBytes);

              return {
                success: true,
                output: outputPath,
                size: pdfBytes.length,
                message: `Successfully compiled to ${outputPath} (${pdfBytes.length} bytes)`
              };
            } catch (error) {
              // Re-throw with structured error
              const err = new Error(error.message || 'Compilation failed');
              err.code = error.code || 'COMPILATION_FAILED';
              err.details = error.details || { error: error.toString() };
              throw err;
            }
          }
          // meta omitted due to zod version conflicts
        },

        validate: {
          description: 'Validate LaTeX document structure (dry-run compilation)',
          handler: async (args) => {
            return {
              valid: true,
              input: args.input || 'unknown',
              message: 'Validation not yet implemented - placeholder'
            };
          }
        },

        clean: {
          description: 'Clear LaTeX compilation cache',
          handler: async (args) => {
            return {
              cacheDir: args.cacheDir || '.latex-cache',
              cleared: false,
              message: 'Cache clearing not yet implemented - placeholder'
            };
          }
        }
      }
    }
  },

  priority: 15 // High priority - core infrastructure
};

export default extension;
