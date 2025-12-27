#!/usr/bin/env node
/**
 * Claude Code CLI Modes Test Suite
 *
 * Tests all programmatic execution modes:
 * - Text output
 * - JSON output
 * - Stream-JSON output
 * - Tool allowlisting
 * - Session management
 */

import { spawn } from 'child_process';
import { promisify } from 'util';

const sleep = promisify(setTimeout);

/**
 * Execute Claude Code CLI with specified options
 * @param {string} prompt - The prompt to send
 * @param {object} options - CLI options
 * @returns {Promise<{stdout: string, stderr: string, exitCode: number}>}
 */
async function executeClaude(prompt, options = {}) {
  return new Promise((resolve, reject) => {
    const args = ['-p', prompt];

    if (options.outputFormat) {
      args.push('--output-format', options.outputFormat);
    }

    if (options.allowedTools) {
      args.push('--allowedTools', options.allowedTools);
    }

    if (options.model) {
      args.push('--model', options.model);
    }

    if (options.sessionId) {
      args.push('--session-id', options.sessionId);
    }

    if (options.continue) {
      args.push('--continue');
    }

    if (options.tools) {
      args.push('--tools', options.tools);
    }

    console.log(`Executing: claude ${args.join(' ')}`);

    const proc = spawn('claude', args, {
      timeout: options.timeout || 30000
    });

    let stdout = '';
    let stderr = '';

    proc.stdout.on('data', (data) => {
      stdout += data.toString();
    });

    proc.stderr.on('data', (data) => {
      stderr += data.toString();
    });

    proc.on('close', (code) => {
      resolve({ stdout, stderr, exitCode: code });
    });

    proc.on('error', (err) => {
      reject(err);
    });
  });
}

/**
 * Test text output mode
 */
async function testTextOutput() {
  console.log('\n=== Testing Text Output Mode ===');
  const result = await executeClaude('What is 2+2? Respond with just the number.', {
    outputFormat: 'text',
    timeout: 15000
  });

  console.log('Exit Code:', result.exitCode);
  console.log('Output:', result.stdout.substring(0, 200));
  console.log('Stderr:', result.stderr.substring(0, 200));

  return result;
}

/**
 * Test JSON output mode
 */
async function testJsonOutput() {
  console.log('\n=== Testing JSON Output Mode ===');
  const result = await executeClaude('What is 3+3? Respond with just the number.', {
    outputFormat: 'json',
    timeout: 15000
  });

  console.log('Exit Code:', result.exitCode);

  try {
    const parsed = JSON.parse(result.stdout);
    console.log('Parsed JSON keys:', Object.keys(parsed));
    console.log('Response excerpt:', JSON.stringify(parsed, null, 2).substring(0, 300));
  } catch (e) {
    console.log('Failed to parse JSON:', e.message);
    console.log('Raw output:', result.stdout.substring(0, 500));
  }

  return result;
}

/**
 * Test stream-JSON output mode
 */
async function testStreamJsonOutput() {
  console.log('\n=== Testing Stream-JSON Output Mode ===');
  const result = await executeClaude('What is 5+5? Respond with just the number.', {
    outputFormat: 'stream-json',
    timeout: 15000
  });

  console.log('Exit Code:', result.exitCode);

  const lines = result.stdout.split('\n').filter(l => l.trim());
  console.log(`Received ${lines.length} stream-json lines`);

  lines.slice(0, 5).forEach((line, idx) => {
    try {
      const parsed = JSON.parse(line);
      console.log(`Line ${idx}:`, JSON.stringify(parsed).substring(0, 100));
    } catch (e) {
      console.log(`Line ${idx} (unparseable):`, line.substring(0, 100));
    }
  });

  return result;
}

/**
 * Test tool allowlisting
 */
async function testToolAllowlist() {
  console.log('\n=== Testing Tool Allowlist ===');
  const result = await executeClaude('List 3 files in /tmp', {
    outputFormat: 'json',
    allowedTools: 'Bash(ls:*)',
    timeout: 20000
  });

  console.log('Exit Code:', result.exitCode);

  try {
    const parsed = JSON.parse(result.stdout);
    console.log('Tool uses:', parsed.tool_uses?.length || 0);
    if (parsed.tool_uses?.length > 0) {
      console.log('First tool:', parsed.tool_uses[0].tool);
    }
  } catch (e) {
    console.log('Output:', result.stdout.substring(0, 300));
  }

  return result;
}

/**
 * Test no-tools mode
 */
async function testNoTools() {
  console.log('\n=== Testing No-Tools Mode ===');
  const result = await executeClaude('Explain what 2+2 equals', {
    outputFormat: 'json',
    tools: '',
    timeout: 15000
  });

  console.log('Exit Code:', result.exitCode);
  console.log('Output:', result.stdout.substring(0, 200));

  return result;
}

// Main test runner
async function main() {
  console.log('Claude Code CLI Modes Test Suite');
  console.log('=================================\n');

  try {
    await testTextOutput();
    await sleep(1000);

    await testJsonOutput();
    await sleep(1000);

    await testStreamJsonOutput();
    await sleep(1000);

    await testToolAllowlist();
    await sleep(1000);

    await testNoTools();

    console.log('\n✅ All tests completed');
  } catch (error) {
    console.error('\n❌ Test failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

main();
