/**
 * Headless Runner - Programmatic Claude Code Execution
 *
 * Provides non-interactive execution capabilities for automation pipelines.
 * Supports structured output formats, tool permissions, and session management.
 *
 * @module @unrdf/kgc-claude/capabilities/headless-runner
 */

import { spawn } from 'child_process';
import { z } from 'zod';
import { createHash } from 'crypto';

/**
 * Output format enumeration
 */
export const OutputFormat = {
  TEXT: 'text',
  JSON: 'json',
  STREAM_JSON: 'stream-json',
};

/**
 * Permission mode enumeration
 */
export const PermissionMode = {
  DEFAULT: 'default',
  ACCEPT_EDITS: 'acceptEdits',
  BYPASS: 'bypassPermissions',
  DONT_ASK: 'dontAsk',
  PLAN: 'plan',
};

/**
 * Headless execution options schema
 */
export const HeadlessOptionsSchema = z.object({
  prompt: z.string().min(1),
  outputFormat: z.enum(['text', 'json', 'stream-json']).default('text'),
  model: z.string().optional(),
  allowedTools: z.array(z.string()).optional(),
  disallowedTools: z.array(z.string()).optional(),
  tools: z.array(z.string()).optional(),
  sessionId: z.string().uuid().optional(),
  continueSession: z.boolean().default(false),
  resumeSession: z.string().optional(),
  forkSession: z.boolean().default(false),
  systemPrompt: z.string().optional(),
  appendSystemPrompt: z.string().optional(),
  permissionMode: z.enum(['default', 'acceptEdits', 'bypassPermissions', 'dontAsk', 'plan']).optional(),
  dangerouslySkipPermissions: z.boolean().default(false),
  jsonSchema: z.any().optional(),
  includePartialMessages: z.boolean().default(false),
  fallbackModel: z.string().optional(),
  timeout: z.number().positive().default(120000), // 2 minutes default
  cwd: z.string().optional(),
  env: z.record(z.string()).optional(),
});

/**
 * Execution result schema
 */
export const ExecutionResultSchema = z.object({
  success: z.boolean(),
  output: z.any(),
  exitCode: z.number(),
  stdout: z.string(),
  stderr: z.string(),
  duration: z.number(),
  promptHash: z.string(),
  sessionId: z.string().optional(),
  error: z.string().optional(),
});

/**
 * Stream event schema
 */
export const StreamEventSchema = z.object({
  type: z.enum(['delta', 'tool_use', 'complete', 'error']),
  content: z.any(),
  timestamp: z.number(),
});

/**
 * Headless Runner - Execute Claude Code programmatically
 */
export class HeadlessRunner {
  /**
   * @param {object} config - Runner configuration
   * @param {string} [config.claudePath='claude'] - Path to claude CLI
   * @param {boolean} [config.debug=false] - Enable debug output
   */
  constructor({ claudePath = 'claude', debug = false } = {}) {
    this.claudePath = claudePath;
    this.debug = debug;
    this.activeProcesses = new Map();
  }

  /**
   * Execute a prompt and return structured result
   *
   * @param {object} options - Execution options
   * @returns {Promise<object>} Execution result
   */
  async execute(options) {
    const validated = HeadlessOptionsSchema.parse(options);
    const startTime = Date.now();
    const promptHash = createHash('sha256').update(validated.prompt).digest('hex');

    if (this.debug) {
      console.log('[HeadlessRunner] Executing:', {
        prompt: validated.prompt.slice(0, 50) + '...',
        outputFormat: validated.outputFormat,
        timeout: validated.timeout,
      });
    }

    const args = this._buildArgs(validated);
    const result = await this._spawn(args, validated);

    const duration = Date.now() - startTime;

    return ExecutionResultSchema.parse({
      success: result.exitCode === 0,
      output: this._parseOutput(result.stdout, validated.outputFormat),
      exitCode: result.exitCode,
      stdout: result.stdout,
      stderr: result.stderr,
      duration,
      promptHash,
      sessionId: this._extractSessionId(result.stdout, validated.outputFormat),
      error: result.exitCode !== 0 ? result.stderr || result.stdout : undefined,
    });
  }

  /**
   * Execute with streaming output
   *
   * @param {object} options - Execution options
   * @param {function} onEvent - Event handler (event) => void
   * @returns {Promise<object>} Final result
   */
  async executeStream(options, onEvent) {
    const validated = HeadlessOptionsSchema.parse({
      ...options,
      outputFormat: 'stream-json',
      includePartialMessages: true,
    });

    const startTime = Date.now();
    const promptHash = createHash('sha256').update(validated.prompt).digest('hex');
    const args = this._buildArgs(validated);

    const events = [];
    const result = await this._spawn(args, validated, (chunk) => {
      try {
        const lines = chunk.split('\n').filter(Boolean);
        for (const line of lines) {
          try {
            const event = StreamEventSchema.parse({
              ...JSON.parse(line),
              timestamp: Date.now(),
            });
            events.push(event);
            onEvent(event);
          } catch {
            // Skip invalid JSON lines
          }
        }
      } catch (error) {
        if (this.debug) {
          console.error('[HeadlessRunner] Stream parse error:', error.message);
        }
      }
    });

    const duration = Date.now() - startTime;

    return ExecutionResultSchema.parse({
      success: result.exitCode === 0,
      output: events,
      exitCode: result.exitCode,
      stdout: result.stdout,
      stderr: result.stderr,
      duration,
      promptHash,
      error: result.exitCode !== 0 ? result.stderr : undefined,
    });
  }

  /**
   * Execute multiple prompts in parallel
   *
   * @param {Array<object>} prompts - Array of execution options
   * @param {object} options - Parallel execution options
   * @param {number} [options.concurrency=5] - Max parallel executions
   * @returns {Promise<Array<object>>} Array of results
   */
  async executeParallel(prompts, { concurrency = 5 } = {}) {
    const results = [];
    const queue = [...prompts];

    const executeNext = async () => {
      if (queue.length === 0) return;
      const options = queue.shift();
      const result = await this.execute(options);
      results.push(result);
      return executeNext();
    };

    const workers = Array(Math.min(concurrency, prompts.length))
      .fill(null)
      .map(() => executeNext());

    await Promise.all(workers);
    return results;
  }

  /**
   * Cancel all active processes
   */
  cancelAll() {
    for (const [pid, process] of this.activeProcesses.entries()) {
      process.kill('SIGTERM');
      this.activeProcesses.delete(pid);
    }
  }

  /**
   * Build CLI arguments from options
   *
   * @private
   */
  _buildArgs(options) {
    const args = ['-p', options.prompt];

    if (options.outputFormat !== 'text') {
      args.push('--output-format', options.outputFormat);
    }

    if (options.model) {
      args.push('--model', options.model);
    }

    if (options.allowedTools && options.allowedTools.length > 0) {
      args.push('--allowedTools', ...options.allowedTools);
    }

    if (options.disallowedTools && options.disallowedTools.length > 0) {
      args.push('--disallowedTools', ...options.disallowedTools);
    }

    if (options.tools && options.tools.length > 0) {
      args.push('--tools', ...options.tools);
    }

    if (options.sessionId) {
      args.push('--session-id', options.sessionId);
    }

    if (options.continueSession) {
      args.push('--continue');
    }

    if (options.resumeSession) {
      args.push('--resume', options.resumeSession);
    }

    if (options.forkSession) {
      args.push('--fork-session');
    }

    if (options.systemPrompt) {
      args.push('--system-prompt', options.systemPrompt);
    }

    if (options.appendSystemPrompt) {
      args.push('--append-system-prompt', options.appendSystemPrompt);
    }

    if (options.permissionMode) {
      args.push('--permission-mode', options.permissionMode);
    }

    if (options.dangerouslySkipPermissions) {
      args.push('--dangerously-skip-permissions');
    }

    if (options.jsonSchema) {
      args.push('--json-schema', JSON.stringify(options.jsonSchema));
    }

    if (options.includePartialMessages) {
      args.push('--include-partial-messages');
    }

    if (options.fallbackModel) {
      args.push('--fallback-model', options.fallbackModel);
    }

    return args;
  }

  /**
   * Spawn child process and collect output
   *
   * @private
   */
  _spawn(args, options, onStdoutChunk = null) {
    return new Promise((resolve, reject) => {
      const child = spawn(this.claudePath, args, {
        cwd: options.cwd,
        env: { ...process.env, ...options.env },
        stdio: ['ignore', 'pipe', 'pipe'],
      });

      this.activeProcesses.set(child.pid, child);

      let stdout = '';
      let stderr = '';

      child.stdout.on('data', (data) => {
        const chunk = data.toString();
        stdout += chunk;
        if (onStdoutChunk) {
          onStdoutChunk(chunk);
        }
      });

      child.stderr.on('data', (data) => {
        stderr += data.toString();
      });

      const timeout = setTimeout(() => {
        child.kill('SIGTERM');
        this.activeProcesses.delete(child.pid);
        reject(new Error(`Execution timeout after ${options.timeout}ms`));
      }, options.timeout);

      child.on('close', (exitCode) => {
        clearTimeout(timeout);
        this.activeProcesses.delete(child.pid);
        resolve({ exitCode, stdout, stderr });
      });

      child.on('error', (error) => {
        clearTimeout(timeout);
        this.activeProcesses.delete(child.pid);
        reject(error);
      });
    });
  }

  /**
   * Parse output based on format
   *
   * @private
   */
  _parseOutput(stdout, format) {
    if (format === 'json') {
      try {
        return JSON.parse(stdout);
      } catch {
        return { raw: stdout };
      }
    }

    if (format === 'stream-json') {
      const lines = stdout.split('\n').filter(Boolean);
      const events = [];
      for (const line of lines) {
        try {
          events.push(JSON.parse(line));
        } catch {
          // Skip invalid lines
        }
      }
      return events;
    }

    return stdout;
  }

  /**
   * Extract session ID from output
   *
   * @private
   */
  _extractSessionId(stdout, format) {
    if (format === 'json') {
      try {
        const parsed = JSON.parse(stdout);
        return parsed.session_id || parsed.sessionId;
      } catch {
        return undefined;
      }
    }
    return undefined;
  }
}

/**
 * Create headless runner instance
 *
 * @param {object} config - Runner configuration
 * @returns {HeadlessRunner} Runner instance
 */
export function createHeadlessRunner(config = {}) {
  return new HeadlessRunner(config);
}

/**
 * Quick execution helper - one-shot execution
 *
 * @param {string} prompt - Prompt to execute
 * @param {object} options - Execution options
 * @returns {Promise<object>} Execution result
 */
export async function execute(prompt, options = {}) {
  const runner = createHeadlessRunner();
  return runner.execute({ ...options, prompt });
}

/**
 * Quick streaming execution helper
 *
 * @param {string} prompt - Prompt to execute
 * @param {function} onEvent - Event handler
 * @param {object} options - Execution options
 * @returns {Promise<object>} Execution result
 */
export async function executeStream(prompt, onEvent, options = {}) {
  const runner = createHeadlessRunner();
  return runner.executeStream({ ...options, prompt }, onEvent);
}
