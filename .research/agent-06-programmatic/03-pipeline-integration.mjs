#!/usr/bin/env node
/**
 * Claude Code Pipeline Integration Patterns
 *
 * Examples of integrating Claude Code into automation pipelines
 */

import { spawn, execSync } from 'child_process';
import { promisify } from 'util';
import { randomUUID } from 'crypto';

const sleep = promisify(setTimeout);

/**
 * Promise-based Claude execution with structured output
 */
class ClaudeClient {
  constructor(options = {}) {
    this.defaultModel = options.model || 'sonnet';
    this.defaultTimeout = options.timeout || 30000;
    this.allowedTools = options.allowedTools;
  }

  /**
   * Execute a prompt and return JSON result
   * @param {string} prompt - The prompt
   * @param {object} options - Execution options
   * @returns {Promise<object>} - Parsed JSON response
   */
  async execute(prompt, options = {}) {
    return new Promise((resolve, reject) => {
      const args = ['-p', prompt, '--output-format', 'json'];

      if (options.model || this.defaultModel) {
        args.push('--model', options.model || this.defaultModel);
      }

      if (options.allowedTools || this.allowedTools) {
        args.push('--allowedTools', options.allowedTools || this.allowedTools);
      }

      if (options.sessionId) {
        args.push('--session-id', options.sessionId);
      }

      if (options.tools !== undefined) {
        args.push('--tools', options.tools);
      }

      const timeout = options.timeout || this.defaultTimeout;
      const proc = spawn('claude', args, { timeout });

      let stdout = '';
      let stderr = '';

      proc.stdout.on('data', (data) => {
        stdout += data.toString();
      });

      proc.stderr.on('data', (data) => {
        stderr += data.toString();
      });

      proc.on('close', (code) => {
        if (code === 0) {
          try {
            const result = JSON.parse(stdout);
            resolve(result);
          } catch (e) {
            reject(new Error(`Failed to parse JSON: ${e.message}\n${stdout}`));
          }
        } else {
          reject(new Error(`Claude exited with code ${code}\n${stderr}`));
        }
      });

      proc.on('error', (err) => {
        reject(err);
      });
    });
  }

  /**
   * Execute with streaming output
   * @param {string} prompt - The prompt
   * @param {function} onChunk - Callback for each chunk
   * @returns {Promise<string>} - Full response
   */
  async stream(prompt, onChunk) {
    return new Promise((resolve, reject) => {
      const proc = spawn('claude', [
        '-p', prompt,
        '--output-format', 'stream-json'
      ]);

      let buffer = '';

      proc.stdout.on('data', (data) => {
        const lines = data.toString().split('\n');
        lines.forEach(line => {
          if (!line.trim()) return;

          try {
            const event = JSON.parse(line);
            if (event.delta?.type === 'text_delta') {
              buffer += event.delta.text;
              onChunk(event.delta.text);
            }
          } catch (e) {
            // Ignore parse errors
          }
        });
      });

      proc.on('close', (code) => {
        if (code === 0) {
          resolve(buffer);
        } else {
          reject(new Error(`Stream failed with code ${code}`));
        }
      });
    });
  }

  /**
   * Create a persistent session
   * @param {string} sessionId - Optional session ID (generates UUID if not provided)
   * @returns {object} - Session object
   */
  createSession(sessionId = randomUUID()) {
    return {
      id: sessionId,
      execute: (prompt, options = {}) => {
        return this.execute(prompt, { ...options, sessionId });
      }
    };
  }
}

/**
 * Example: Multi-step workflow with session persistence
 */
async function exampleWorkflow() {
  console.log('=== Multi-Step Workflow Example ===\n');

  const client = new ClaudeClient({ model: 'haiku' });
  const session = client.createSession();

  console.log(`Session ID: ${session.id}\n`);

  // Step 1: Initialize
  console.log('Step 1: Planning...');
  const plan = await session.execute('Create a plan to sort an array in JavaScript. Respond in 1 sentence.');
  console.log(`Plan: ${plan.content?.substring(0, 100)}...\n`);

  await sleep(1000);

  // Step 2: Execute
  console.log('Step 2: Implementation...');
  const code = await session.execute('Write the sorting function code. Just code, no explanation.');
  console.log(`Code snippet: ${code.content?.substring(0, 150)}...\n`);

  console.log('✅ Workflow complete\n');
}

/**
 * Example: Pipeline with error handling
 */
async function examplePipeline() {
  console.log('=== Pipeline with Error Handling ===\n');

  const client = new ClaudeClient({
    model: 'haiku',
    timeout: 20000
  });

  const tasks = [
    'What is 10 + 5?',
    'What is the capital of France?',
    'Name one programming language'
  ];

  for (const [idx, task] of tasks.entries()) {
    try {
      console.log(`Task ${idx + 1}: ${task}`);
      const result = await client.execute(task, { tools: '' });
      console.log(`  ✓ ${result.content?.substring(0, 50)}\n`);
    } catch (error) {
      console.log(`  ✗ Failed: ${error.message}\n`);
    }
  }

  console.log('✅ Pipeline complete\n');
}

/**
 * Example: Parallel execution
 */
async function exampleParallel() {
  console.log('=== Parallel Execution Example ===\n');

  const client = new ClaudeClient({ model: 'haiku' });

  const prompts = [
    'What is 2+2?',
    'What is 3+3?',
    'What is 5+5?'
  ];

  console.log('Executing 3 prompts in parallel...\n');

  const startTime = Date.now();

  const results = await Promise.all(
    prompts.map(p => client.execute(p, { tools: '' }))
  );

  const duration = Date.now() - startTime;

  results.forEach((result, idx) => {
    console.log(`${idx + 1}. ${prompts[idx]}`);
    console.log(`   → ${result.content?.substring(0, 50)}`);
  });

  console.log(`\n⏱️  Completed in ${duration}ms\n`);
}

// Main
async function main() {
  try {
    await exampleWorkflow();
    await examplePipeline();
    await exampleParallel();
  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { ClaudeClient };
