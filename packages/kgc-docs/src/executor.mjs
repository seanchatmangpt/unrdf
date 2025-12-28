/**
 * @file Code Block Executor
 * @module kgc-docs/executor
 *
 * Execute code blocks marked as kgc:executable and capture output
 */

import { createContext, runInContext } from 'node:vm';

/**
 * Execute JavaScript code and capture output
 * @param {string} code - JavaScript code to execute
 * @param {number} timeout - Execution timeout in milliseconds (default: 1000)
 * @returns {object} Execution result with output and status
 */
export function executeCode(code, timeout = 1000) {
  const result = {
    success: false,
    output: [],
    error: null,
    returnValue: null,
  };

  // Create sandbox context
  const sandbox = {
    console: {
      log: (...args) => {
        result.output.push(args.map(String).join(' '));
      },
      error: (...args) => {
        result.output.push(`ERROR: ${args.map(String).join(' ')}`);
      },
    },
    setTimeout: undefined,
    setInterval: undefined,
    setImmediate: undefined,
    // Safe globals
    Math,
    Date,
    JSON,
    Object,
    Array,
    String,
    Number,
    Boolean,
  };

  const context = createContext(sandbox);

  try {
    // Execute code in sandbox
    const returnValue = runInContext(code, context, {
      timeout,
      displayErrors: true,
    });

    result.success = true;
    result.returnValue = returnValue;
  } catch (err) {
    result.success = false;
    result.error = err.message;
    result.output.push(`Execution error: ${err.message}`);
  }

  return result;
}

/**
 * Execute code block and format output for embedding
 * @param {object} block - Code block object with content
 * @param {number} timeout - Execution timeout in milliseconds
 * @returns {object} Formatted execution result
 */
export function executeBlock(block, timeout = 1000) {
  if (!block.executable) {
    return {
      executed: false,
      reason: 'Block not marked as executable',
    };
  }

  const execution = executeCode(block.content, timeout);

  return {
    executed: true,
    success: execution.success,
    code: block.content,
    output: execution.output,
    returnValue: execution.returnValue,
    error: execution.error,
  };
}

/**
 * Format execution result as markdown
 * @param {object} executionResult - Result from executeBlock
 * @returns {string} Markdown formatted output
 */
export function formatExecutionOutput(executionResult) {
  if (!executionResult.executed) {
    return '';
  }

  let output = '**Execution Output:**\n\n';

  if (executionResult.output.length > 0) {
    output += '```\n';
    output += executionResult.output.join('\n');
    output += '\n```\n\n';
  }

  if (
    executionResult.returnValue !== null &&
    executionResult.returnValue !== undefined
  ) {
    output += `**Return Value:** \`${JSON.stringify(executionResult.returnValue)}\`\n\n`;
  }

  if (executionResult.error) {
    output += `**Error:** ${executionResult.error}\n\n`;
  }

  return output;
}
