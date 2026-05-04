/**
 * AtomVM Command Suite - Erlang/BEAM WASM runtime tools
 *
 * Commands for managing AtomVM environments, compiling Erlang, and checking system health.
 *
 * @module cli/commands/atomvm
 */

import { defineCommand } from 'citty';
import { execSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import { join, resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const c = {
  reset: '\x1b[0m',
  bold: '\x1b[1m',
  dim: '\x1b[2m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  red: '\x1b[31m',
  cyan: '\x1b[36m',
};

const __dirname = dirname(fileURLToPath(import.meta.url));
// Find packages/atomvm directory relative to this file
// cli/src/cli/commands/atomvm.mjs -> up 4 levels -> packages/atomvm
const ATOMVM_PKG_DIR = resolve(__dirname, '../../../../atomvm');

/**
 * Diagnostic tool for AtomVM dependencies
 */
const doctorCommand = defineCommand({
  meta: {
    name: 'doctor',
    description: 'Check AtomVM development dependencies (erlc, packbeam)',
  },
  async run() {
    console.log(`${c.bold}${c.cyan}AtomVM Doctor${c.reset} - Checking development environment\n`);

    let allPassed = true;

    // Check 1: erlc
    try {
      const erlcVersion = execSync('erlc -version 2>&1', { stdio: 'pipe' }).toString().trim();
      console.log(`${c.green}✅ erlc found:${c.reset} ${erlcVersion}`);
    } catch (e) {
      console.log(`${c.red}❌ erlc not found${c.reset}`);
      console.log(`   ${c.dim}Required to compile Erlang (.erl) to BEAM (.beam)${c.reset}`);
      console.log(`   ${c.yellow}Fix: brew install erlang${c.reset} or apt-get install erlang\n`);
      allPassed = false;
    }

    // Check 2: packbeam
    try {
      execSync('packbeam -h', { stdio: 'pipe' });
      console.log(`${c.green}✅ packbeam found${c.reset}`);
    } catch (e) {
      if (e.message && e.message.includes('ENOENT')) {
        console.log(`${c.red}❌ packbeam not found${c.reset}`);
        console.log(`   ${c.dim}Required to package BEAM files to AVM (.avm)${c.reset}`);
        console.log(`   ${c.yellow}Fix: brew tap atomvm/atomvm && brew install atomvm${c.reset}\n`);
        allPassed = false;
      } else {
        console.log(`${c.green}✅ packbeam found${c.reset}`);
      }
    }

    // Check 3: node
    try {
      const nodeVersion = execSync('node -v', { stdio: 'pipe' }).toString().trim();
      console.log(`${c.green}✅ node found:${c.reset} ${nodeVersion}`);
    } catch (e) {
      console.log(`${c.red}❌ node not found${c.reset}`);
      allPassed = false;
    }

    console.log('\n----------------------------------------');
    if (allPassed) {
      console.log(`${c.bold}${c.green}System is ready for AtomVM development!${c.reset}`);
    } else {
      console.log(`${c.bold}${c.red}Missing dependencies detected. Please install them.${c.reset}`);
      process.exitCode = 1;
    }
  },
});

/**
 * Build Erlang module to .avm
 */
const buildCommand = defineCommand({
  meta: {
    name: 'build',
    description: 'Compile Erlang module and package to .avm bytecode',
  },
  args: {
    module: {
      type: 'positional',
      description: 'Module name (e.g. hello_world)',
      required: true,
    },
  },
  async run({ args }) {
    const { module: moduleName } = args;
    console.log(`${c.bold}${c.cyan}AtomVM Build${c.reset} - ${moduleName}\n`);

    if (!existsSync(ATOMVM_PKG_DIR)) {
      throw new Error(`AtomVM package not found at: ${ATOMVM_PKG_DIR}`);
    }

    try {
      // Import build logic from atomvm package
      const { buildModule } = await import(join(ATOMVM_PKG_DIR, 'scripts/build.mjs'));
      await buildModule(moduleName);
    } catch (error) {
      console.error(`${c.red}Build failed:${c.reset} ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Execute .avm file using WASM runtime
 */
const executeCommand = defineCommand({
  meta: {
    name: 'execute',
    description: 'Execute .avm bytecode using WASM runtime',
  },
  args: {
    file: {
      type: 'positional',
      description: 'Path to .avm file',
      required: true,
    },
    verbose: {
      type: 'boolean',
      description: 'Show detailed runtime logs',
      default: false,
    },
  },
  async run({ args }) {
    const { file: avmPath, verbose } = args;
    console.log(`${c.bold}${c.cyan}AtomVM Execute${c.reset} - ${avmPath}\n`);

    const absoluteAvmPath = resolve(process.cwd(), avmPath);
    if (!existsSync(absoluteAvmPath)) {
      throw new Error(`AVM file not found: ${absoluteAvmPath}`);
    }

    try {
      // Import runtime from atomvm package
      const { AtomVMNodeRuntime } = await import(join(ATOMVM_PKG_DIR, 'src/node-runtime.mjs'));
      
      const runtime = new AtomVMNodeRuntime({
        log: (msg) => {
          if (verbose || !msg.startsWith('[Runtime]')) {
            console.log(msg);
          }
        },
        errorLog: (msg) => console.error(`${c.red}[Error]${c.reset} ${msg}`),
      });

      await runtime.load();
      const result = await runtime.execute(absoluteAvmPath);
      
      console.log(`\n${c.green}✅ Execution complete (exit code: ${result.exitCode})${c.reset}`);
    } catch (error) {
      console.error(`${c.red}Execution failed:${c.reset} ${error.message}`);
      process.exit(1);
    }
  },
});

export const atomvmCommand = defineCommand({
  meta: {
    name: 'atomvm',
    description: 'AtomVM tools and environment management',
  },
  subCommands: {
    doctor: doctorCommand,
    build: buildCommand,
    execute: executeCommand,
  },
});
