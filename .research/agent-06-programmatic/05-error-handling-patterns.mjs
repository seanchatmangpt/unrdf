#!/usr/bin/env node
/**
 * Claude Code Error Handling Patterns
 *
 * Demonstrates robust error handling for production use
 */

import { spawn } from 'child_process';
import { promisify } from 'util';

const sleep = promisify(setTimeout);

/**
 * Enhanced Claude client with error handling
 */
class RobustClaudeClient {
  constructor(options = {}) {
    this.maxRetries = options.maxRetries || 3;
    this.retryDelay = options.retryDelay || 2000;
    this.timeout = options.timeout || 30000;
    this.model = options.model || 'sonnet';
  }

  /**
   * Execute with automatic retry on transient errors
   */
  async executeWithRetry(prompt, options = {}, attempt = 1) {
    try {
      return await this.execute(prompt, options);
    } catch (error) {
      // Check if error is retryable
      const isRetryable = this.isRetryableError(error);

      if (isRetryable && attempt < this.maxRetries) {
        console.log(`Attempt ${attempt} failed: ${error.message}`);
        console.log(`Retrying in ${this.retryDelay}ms... (${attempt}/${this.maxRetries})`);

        await sleep(this.retryDelay);
        return this.executeWithRetry(prompt, options, attempt + 1);
      }

      throw error;
    }
  }

  /**
   * Check if error should trigger retry
   */
  isRetryableError(error) {
    const message = error.message.toLowerCase();

    // Retryable conditions
    const retryable = [
      'overloaded',
      'timeout',
      'rate limit',
      'network',
      'econnrefused',
      'enotfound'
    ];

    return retryable.some(pattern => message.includes(pattern));
  }

  /**
   * Basic execute method
   */
  async execute(prompt, options = {}) {
    return new Promise((resolve, reject) => {
      const args = ['-p', prompt, '--output-format', 'json'];

      if (options.model || this.model) {
        args.push('--model', options.model || this.model);
      }

      if (options.allowedTools) {
        args.push('--allowedTools', options.allowedTools);
      }

      if (options.tools !== undefined) {
        args.push('--tools', options.tools);
      }

      const timeout = options.timeout || this.timeout;
      const proc = spawn('claude', args, { timeout });

      let stdout = '';
      let stderr = '';

      const timeoutId = setTimeout(() => {
        proc.kill('SIGTERM');
        reject(new Error(`Execution timeout after ${timeout}ms`));
      }, timeout);

      proc.stdout.on('data', d => stdout += d);
      proc.stderr.on('data', d => stderr += d);

      proc.on('close', (code) => {
        clearTimeout(timeoutId);

        if (code === 0) {
          try {
            const result = JSON.parse(stdout);
            resolve(result);
          } catch (e) {
            reject(new Error(`JSON parse error: ${e.message}\nOutput: ${stdout.substring(0, 200)}`));
          }
        } else {
          // Categorize error based on exit code and stderr
          const error = this.categorizeError(code, stderr, stdout);
          reject(error);
        }
      });

      proc.on('error', (err) => {
        clearTimeout(timeoutId);
        reject(err);
      });
    });
  }

  /**
   * Categorize error for better handling
   */
  categorizeError(code, stderr, stdout) {
    const error = new Error(`Claude execution failed (exit code ${code})`);

    // Add error category for better handling
    if (stderr.includes('overloaded')) {
      error.category = 'OVERLOADED';
      error.retryable = true;
    } else if (stderr.includes('rate limit')) {
      error.category = 'RATE_LIMIT';
      error.retryable = true;
    } else if (stderr.includes('authentication')) {
      error.category = 'AUTH_ERROR';
      error.retryable = false;
    } else if (stderr.includes('network')) {
      error.category = 'NETWORK_ERROR';
      error.retryable = true;
    } else {
      error.category = 'UNKNOWN';
      error.retryable = false;
    }

    error.stderr = stderr;
    error.stdout = stdout;
    error.exitCode = code;

    return error;
  }

  /**
   * Execute with timeout and fallback model
   */
  async executeWithFallback(prompt, options = {}) {
    try {
      return await this.execute(prompt, { ...options, model: this.model });
    } catch (error) {
      if (error.category === 'OVERLOADED' && this.model !== 'haiku') {
        console.log(`${this.model} overloaded, falling back to haiku...`);
        return await this.execute(prompt, { ...options, model: 'haiku' });
      }
      throw error;
    }
  }

  /**
   * Execute with validation
   */
  async executeWithValidation(prompt, validator, options = {}) {
    const result = await this.execute(prompt, options);

    if (!validator(result)) {
      throw new Error('Response validation failed');
    }

    return result;
  }
}

/**
 * Example: Retry on transient errors
 */
async function exampleRetry() {
  console.log('=== Retry Pattern Example ===\n');

  const client = new RobustClaudeClient({
    maxRetries: 3,
    retryDelay: 1000,
    model: 'haiku'
  });

  try {
    const result = await client.executeWithRetry('What is 2+2?', {
      tools: '',
      timeout: 15000
    });

    console.log(`✅ Success: ${result.content?.substring(0, 50)}\n`);
  } catch (error) {
    console.log(`❌ Failed after retries: ${error.message}\n`);
  }
}

/**
 * Example: Fallback to different model
 */
async function exampleFallback() {
  console.log('=== Model Fallback Example ===\n');

  const client = new RobustClaudeClient({ model: 'sonnet' });

  try {
    const result = await client.executeWithFallback('Explain sorting algorithms in 1 sentence.', {
      tools: '',
      timeout: 15000
    });

    console.log(`✅ Success: ${result.content?.substring(0, 100)}\n`);
  } catch (error) {
    console.log(`❌ All models failed: ${error.message}\n`);
  }
}

/**
 * Example: Validation
 */
async function exampleValidation() {
  console.log('=== Response Validation Example ===\n');

  const client = new RobustClaudeClient({ model: 'haiku' });

  const validator = (result) => {
    // Ensure response is not empty and has content
    return result.content && result.content.length > 0;
  };

  try {
    const result = await client.executeWithValidation(
      'What is the capital of France?',
      validator,
      { tools: '', timeout: 15000 }
    );

    console.log(`✅ Valid response: ${result.content?.substring(0, 50)}\n`);
  } catch (error) {
    console.log(`❌ Validation failed: ${error.message}\n`);
  }
}

/**
 * Example: Circuit breaker pattern
 */
class CircuitBreaker {
  constructor(client, options = {}) {
    this.client = client;
    this.failureThreshold = options.failureThreshold || 5;
    this.resetTimeout = options.resetTimeout || 60000;
    this.failures = 0;
    this.state = 'CLOSED'; // CLOSED, OPEN, HALF_OPEN
    this.nextAttempt = null;
  }

  async execute(prompt, options) {
    // Check circuit state
    if (this.state === 'OPEN') {
      if (Date.now() < this.nextAttempt) {
        throw new Error('Circuit breaker is OPEN');
      }
      this.state = 'HALF_OPEN';
    }

    try {
      const result = await this.client.execute(prompt, options);

      // Success - reset circuit
      if (this.state === 'HALF_OPEN') {
        this.state = 'CLOSED';
        this.failures = 0;
      }

      return result;
    } catch (error) {
      this.failures++;

      if (this.failures >= this.failureThreshold) {
        this.state = 'OPEN';
        this.nextAttempt = Date.now() + this.resetTimeout;
        console.log(`⚠️  Circuit breaker opened (${this.failures} failures)`);
      }

      throw error;
    }
  }
}

async function exampleCircuitBreaker() {
  console.log('=== Circuit Breaker Example ===\n');

  const client = new RobustClaudeClient({ model: 'haiku' });
  const breaker = new CircuitBreaker(client, {
    failureThreshold: 3,
    resetTimeout: 5000
  });

  // Simulate requests
  for (let i = 0; i < 5; i++) {
    try {
      console.log(`Request ${i + 1}...`);
      const result = await breaker.execute(`What is ${i}+${i}?`, {
        tools: '',
        timeout: 10000
      });
      console.log(`  ✓ ${result.content?.substring(0, 30)}`);
    } catch (error) {
      console.log(`  ✗ ${error.message}`);
    }

    await sleep(500);
  }

  console.log(`\nCircuit state: ${breaker.state}\n`);
}

// Main
async function main() {
  console.log('Claude Code Error Handling Patterns');
  console.log('===================================\n');

  try {
    await exampleRetry();
    await exampleFallback();
    await exampleValidation();
    await exampleCircuitBreaker();

    console.log('✅ All error handling examples completed\n');
  } catch (error) {
    console.error('❌ Example failed:', error.message);
    process.exit(1);
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { RobustClaudeClient, CircuitBreaker };
