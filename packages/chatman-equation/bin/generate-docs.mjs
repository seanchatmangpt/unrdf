#!/usr/bin/env node

/**
 * @file generate-docs.mjs
 * @description CLI tool for generating Chatman Equation documentation from TOML configs
 */

import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';
import { existsSync } from 'fs';
import { createTemplateEngine } from '../src/template-engine.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Parse command line arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);
  const options = {
    config: null,
    output: null,
    template: null,
    templatesDir: resolve(__dirname, '../templates'),
    outputDir: resolve(__dirname, '../generated'),
    batch: false,
    help: false,
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    switch (arg) {
      case '-h':
      case '--help':
        options.help = true;
        break;
      case '-c':
      case '--config':
        options.config = args[++i];
        break;
      case '-o':
      case '--output':
        options.output = args[++i];
        break;
      case '-t':
      case '--template':
        options.template = args[++i];
        break;
      case '--templates-dir':
        options.templatesDir = args[++i];
        break;
      case '--output-dir':
        options.outputDir = args[++i];
        break;
      case '-b':
      case '--batch':
        options.batch = true;
        break;
      default:
        if (!options.config && !arg.startsWith('-')) {
          options.config = arg;
        }
    }
  }

  return options;
}

/**
 * Show help message
 */
function showHelp() {
  console.log(`
Chatman Equation Documentation Generator

Usage:
  chatman-docs [options] <config-file>
  chatman-docs --batch <config-directory>

Options:
  -c, --config <file>         TOML configuration file
  -o, --output <file>         Output file path (overrides config)
  -t, --template <name>       Template name (overrides config)
  --templates-dir <dir>       Templates directory
  --output-dir <dir>          Output directory
  -b, --batch                 Batch process directory of configs
  -h, --help                  Show this help message

Examples:
  # Generate from single config
  chatman-docs ./config/equation.toml

  # Generate with custom output
  chatman-docs -c ./config/equation.toml -o ./docs/equation.md

  # Batch generate all configs
  chatman-docs --batch ./configs

  # Use custom template
  chatman-docs -c ./config.toml -t custom-template.tera

Templates:
  - equation-reference.tera     API reference documentation
  - diataxis-explanation.tera   Conceptual explanation docs
  - example-generator.tera      Code example generation
  - tutorial.tera               Step-by-step tutorials
`);
}

/**
 * Main execution
 */
async function main() {
  const options = parseArgs();

  if (options.help) {
    showHelp();
    process.exit(0);
  }

  if (!options.config) {
    console.error('Error: Config file or directory required\n');
    showHelp();
    process.exit(1);
  }

  try {
    const engine = createTemplateEngine({
      templatesDir: options.templatesDir,
      outputDir: options.outputDir,
    });

    if (options.batch) {
      console.log(`Batch generating from: ${options.config}\n`);
      const results = await engine.batchGenerate(options.config);

      let successCount = 0;
      let failCount = 0;

      results.forEach((result) => {
        if (result.success) {
          console.log(`✓ Generated: ${result.outputPath}`);
          successCount++;
        } else {
          console.error(`✗ Failed: ${result.configPath}`);
          console.error(`  Error: ${result.error}`);
          failCount++;
        }
      });

      console.log(`\nTotal: ${results.length} | Success: ${successCount} | Failed: ${failCount}`);
      process.exit(failCount > 0 ? 1 : 0);
    } else {
      if (!existsSync(options.config)) {
        console.error(`Error: Config file not found: ${options.config}`);
        process.exit(1);
      }

      console.log(`Generating documentation from: ${options.config}`);

      const result = engine.generateFromConfig(options.config, options.output);

      console.log(`✓ Template: ${result.templateName}`);
      console.log(`✓ Output: ${result.outputPath}`);
      console.log('\nGeneration complete!');
    }
  } catch (error) {
    console.error(`\nError: ${error.message}`);
    if (process.env.DEBUG) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

main();
