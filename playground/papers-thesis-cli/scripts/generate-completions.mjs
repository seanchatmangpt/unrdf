#!/usr/bin/env node
/**
 * @fileoverview Generate Shell Completions Script
 *
 * @description
 * Generates shell completion scripts for all supported shells
 * (bash, zsh, fish, powershell). Can generate for a specific shell
 * or all shells at once.
 *
 * @module scripts/generate-completions
 * @version 1.0.0
 * @license MIT
 *
 * @example
 * # Generate all completions
 * node scripts/generate-completions.mjs
 *
 * @example
 * # Generate for specific shell
 * node scripts/generate-completions.mjs --shell bash
 *
 * @example
 * # Generate with custom output directory
 * node scripts/generate-completions.mjs --output ./completions
 *
 * @example
 * # Install completions for current shell
 * node scripts/generate-completions.mjs --install
 */

import { writeFileSync, mkdirSync, copyFileSync, existsSync } from 'node:fs';
import { join, dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { homedir } from 'node:os';

// Import completion generators
import {
  generateCompletions,
  generateAllCompletions,
  getInstallInstructions,
  detectShell,
  getCompletionFilename,
  SUPPORTED_SHELLS,
  DEFAULT_FILENAMES,
  DEFAULT_PATHS
} from '../packages/cli/completions/index.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Parse command line arguments
 * @returns {Object} Parsed arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);
  const options = {
    shell: null,
    output: join(__dirname, '..', 'output', 'completions'),
    install: false,
    help: false,
    cliName: 'playground',
    verbose: false
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    switch (arg) {
      case '--shell':
      case '-s':
        options.shell = args[++i];
        break;
      case '--output':
      case '-o':
        options.output = resolve(args[++i]);
        break;
      case '--install':
      case '-i':
        options.install = true;
        break;
      case '--help':
      case '-h':
        options.help = true;
        break;
      case '--name':
      case '-n':
        options.cliName = args[++i];
        break;
      case '--verbose':
      case '-v':
        options.verbose = true;
        break;
      default:
        // Positional argument - treat as shell name
        if (!arg.startsWith('-') && !options.shell) {
          options.shell = arg;
        }
    }
  }

  return options;
}

/**
 * Print help message
 */
function printHelp() {
  console.log(`
Generate Shell Completions for playground CLI

Usage:
  node scripts/generate-completions.mjs [options] [shell]

Options:
  -s, --shell <shell>    Generate for specific shell (bash, zsh, fish, powershell)
  -o, --output <dir>     Output directory (default: ./output/completions)
  -i, --install          Install completions for current shell
  -n, --name <name>      CLI name (default: playground)
  -v, --verbose          Verbose output
  -h, --help             Show this help message

Supported Shells:
  ${SUPPORTED_SHELLS.join(', ')}

Examples:
  # Generate all completions
  node scripts/generate-completions.mjs

  # Generate bash completions only
  node scripts/generate-completions.mjs bash

  # Generate and install for current shell
  node scripts/generate-completions.mjs --install

  # Generate to custom directory
  node scripts/generate-completions.mjs -o ./my-completions

Installation Paths:
  bash:       ${DEFAULT_PATHS.bash}
  zsh:        ${DEFAULT_PATHS.zsh}
  fish:       ${DEFAULT_PATHS.fish}
  powershell: ${DEFAULT_PATHS.powershell}
`.trim());
}

/**
 * Get user installation path for a shell
 * @param {string} shell - Shell name
 * @returns {string|null} Installation path or null
 */
function getUserInstallPath(shell) {
  const home = homedir();

  switch (shell) {
    case 'bash':
      return join(home, '.bash_completion.d');
    case 'zsh':
      return join(home, '.zsh', 'completions');
    case 'fish':
      return join(home, '.config', 'fish', 'completions');
    case 'powershell':
      // PowerShell profile varies by platform
      if (process.platform === 'win32') {
        return join(home, 'Documents', 'WindowsPowerShell');
      }
      return join(home, '.config', 'powershell');
    default:
      return null;
  }
}

/**
 * Install completions for a shell
 * @param {string} shell - Shell to install for
 * @param {string} sourcePath - Path to completion file
 * @param {boolean} verbose - Verbose output
 * @returns {boolean} Success
 */
function installCompletions(shell, sourcePath, verbose) {
  const installPath = getUserInstallPath(shell);

  if (!installPath) {
    console.error(`Cannot determine install path for ${shell}`);
    return false;
  }

  try {
    // Create directory if it doesn't exist
    mkdirSync(installPath, { recursive: true });

    const filename = DEFAULT_FILENAMES[shell];
    const destPath = join(installPath, filename);

    copyFileSync(sourcePath, destPath);

    if (verbose) {
      console.log(`Installed ${shell} completions to: ${destPath}`);
    }

    // Print shell-specific instructions
    console.log(`\n${getInstallInstructions(shell, destPath)}`);

    return true;
  } catch (err) {
    console.error(`Failed to install ${shell} completions: ${err.message}`);
    return false;
  }
}

/**
 * Generate completions for a single shell
 * @param {string} shell - Shell name
 * @param {string} outputDir - Output directory
 * @param {Object} options - Generation options
 * @returns {string} Path to generated file
 */
function generateForShell(shell, outputDir, options) {
  const { cliName, verbose } = options;

  const script = generateCompletions(shell, { cliName });
  const filename = getCompletionFilename(shell, cliName);
  const outputPath = join(outputDir, filename);

  writeFileSync(outputPath, script, 'utf-8');

  if (verbose) {
    console.log(`Generated ${shell} completions: ${outputPath}`);
    console.log(`  Size: ${script.length} bytes`);
  } else {
    console.log(`Generated: ${outputPath}`);
  }

  return outputPath;
}

/**
 * Main function
 */
async function main() {
  const options = parseArgs();

  if (options.help) {
    printHelp();
    process.exit(0);
  }

  console.log('Papers-Thesis CLI Shell Completions Generator\n');

  // Create output directory
  try {
    mkdirSync(options.output, { recursive: true });
  } catch (err) {
    console.error(`Failed to create output directory: ${err.message}`);
    process.exit(1);
  }

  // Validate shell if specified
  if (options.shell && !SUPPORTED_SHELLS.includes(options.shell.toLowerCase())) {
    console.error(`Unknown shell: ${options.shell}`);
    console.error(`Supported shells: ${SUPPORTED_SHELLS.join(', ')}`);
    process.exit(1);
  }

  const generatedFiles = {};

  try {
    if (options.shell) {
      // Generate for specific shell
      const shell = options.shell.toLowerCase();
      const outputPath = generateForShell(shell, options.output, options);
      generatedFiles[shell] = outputPath;
    } else {
      // Generate for all shells
      console.log('Generating completions for all shells...\n');

      for (const shell of SUPPORTED_SHELLS) {
        const outputPath = generateForShell(shell, options.output, options);
        generatedFiles[shell] = outputPath;
      }
    }

    console.log(`\nGenerated ${Object.keys(generatedFiles).length} completion file(s)`);

    // Install if requested
    if (options.install) {
      const currentShell = options.shell?.toLowerCase() || detectShell();

      if (currentShell) {
        console.log(`\nInstalling completions for ${currentShell}...`);

        const sourcePath = generatedFiles[currentShell];
        if (sourcePath) {
          installCompletions(currentShell, sourcePath, options.verbose);
        } else {
          // Generate if not already generated
          const outputPath = generateForShell(currentShell, options.output, options);
          installCompletions(currentShell, outputPath, options.verbose);
        }
      } else {
        console.log('\nCould not detect current shell.');
        console.log('Please specify shell with --shell option or install manually.');
      }
    } else {
      // Print installation instructions
      console.log('\nInstallation Instructions:');
      console.log('==========================\n');

      for (const [shell, path] of Object.entries(generatedFiles)) {
        console.log(`${shell.toUpperCase()}:`);
        console.log(`  File: ${path}`);
        console.log(`  Install: See --help or run with --install\n`);
      }

      console.log('Quick install for current shell:');
      console.log('  node scripts/generate-completions.mjs --install\n');
    }

  } catch (error) {
    console.error(`Error generating completions: ${error.message}`);
    if (options.verbose) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

main().catch(err => {
  console.error('Unexpected error:', err.message);
  process.exit(1);
});
