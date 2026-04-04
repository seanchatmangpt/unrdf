#!/usr/bin/env node

/**
 * @file MCP Generator - Auto-generate Turtle ontology from CLI commands
 * @module daemon/mcp/generate
 * @description Discovers all CLI commands via citty introspection and renders cli-commands.ttl
 */

import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';
import { readFileSync, writeFileSync } from 'fs';
import nunjucks from 'nunjucks';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Load the main CLI to discover all commands
 */
async function loadCliCommands() {
  // Import the main CLI command definition
  const { main } = await import('../../../cli/src/cli/main.mjs');
  return main;
}

/**
 * Recursively walk command tree to extract leaf commands
 * @param {string} name - Command name
 * @param {Object} cmd - Command object with meta, args, subCommands
 * @param {string} parentPath - Parent command path (for nesting)
 * @returns {Array<Object>} Array of leaf command definitions
 */
function walkCommands(name, cmd, parentPath = '') {
  const cliPath = parentPath ? `${parentPath} ${name}` : name;

  // If this command has subcommands, recurse into them
  if (cmd.subCommands && Object.keys(cmd.subCommands).length > 0) {
    const results = [];
    for (const [subName, subCmd] of Object.entries(cmd.subCommands)) {
      results.push(...walkCommands(subName, subCmd, cliPath));
    }
    return results;
  }

  // Leaf node - extract command metadata
  const toolName = cliPath.replace(/-/g, '_').replace(/ /g, '_');
  const args = cmd.args ? Object.entries(cmd.args).map(([argName, argDef]) => ({
    argName,
    argType: argDef.type ?? 'string',
    required: argDef.required ?? false,
    argDescription: argDef.description ?? '',
    defaultValue: argDef.default != null ? String(argDef.default) : null,
  })) : [];

  return [{
    toolName,
    cliPath,
    description: cmd.meta?.description ?? '',
    args,
  }];
}

/**
 * Read version from package.json
 * @returns {string} Version string
 */
function readVersion() {
  const pkgPath = resolve(__dirname, '../../package.json');
  const pkg = JSON.parse(readFileSync(pkgPath, 'utf-8'));
  return pkg.version;
}

/**
 * Main generation flow
 */
async function generate() {
  try {
    console.log('🔍 Discovering CLI commands...');
    const main = await loadCliCommands();

    console.log('📊 Walking command tree...');
    const commands = [];
    for (const [name, cmd] of Object.entries(main.subCommands ?? {})) {
      commands.push(...walkCommands(name, cmd));
    }

    console.log(`✅ Discovered ${commands.length} tools`);

    // Read version
    const version = readVersion();
    console.log(`📦 Version: ${version}`);

    // Setup Nunjucks
    const templateDir = resolve(__dirname, 'templates');
    nunjucks.configure(templateDir, { autoescape: false });

    // Render template
    console.log('🎨 Rendering template...');
    const templateContent = readFileSync(resolve(templateDir, 'cli-commands.ttl.njk'), 'utf-8');
    const rendered = nunjucks.renderString(templateContent, { commands });

    // Write output
    const outputPath = resolve(__dirname, 'cli-commands.ttl');
    writeFileSync(outputPath, rendered, 'utf-8');
    console.log(`✅ Generated: ${outputPath}`);

    // Run sync to regenerate all MCP files
    console.log('🔄 Running sync to regenerate MCP files...');
    const { spawn } = await import('child_process');
    const configPath = resolve(__dirname, '.unrdf.toml');

    const sync = spawn('node', [
      resolve(__dirname, '../../../cli/src/cli/main.mjs'),
      'sync',
      '--config', configPath,
    ], {
      cwd: resolve(__dirname, '../../../..'),
      stdio: 'inherit',
    });

    const exitCode = await new Promise(resolve => sync.on('close', resolve));

    if (exitCode === 0) {
      console.log('✅ MCP files regenerated successfully');
    } else {
      console.error(`❌ Sync failed with exit code ${exitCode}`);
      process.exit(1);
    }
  } catch (error) {
    console.error('❌ Generation failed:', error.message);
    process.exit(1);
  }
}

generate();
