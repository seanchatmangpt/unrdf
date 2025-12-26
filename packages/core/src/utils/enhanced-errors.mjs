/**
 * @file Enhanced Error Messages and Debugging Utilities
 * @module @unrdf/core/utils/enhanced-errors
 *
 * @description
 * 80/20 Error UX Enhancement:
 * - Zod validation errors with field context and fixes
 * - Workflow state errors with recovery suggestions
 * - Import errors with actionable solutions
 * - Debug mode with execution tracing
 *
 * Target: 90% errors have actionable fixes, 50-70% debug time reduction
 */

import { z } from 'zod';

// =============================================================================
// Documentation Links Registry
// =============================================================================

/** @type {Record<string, string>} */
const DOCS_LINKS = {
  workflow: 'https://github.com/unrdf/docs/workflow-patterns.md',
  validation: 'https://github.com/unrdf/docs/validation.md',
  imports: 'https://github.com/unrdf/docs/package-setup.md',
  yawl: 'https://github.com/unrdf/docs/yawl-quickstart.md',
  hooks: 'https://github.com/unrdf/docs/hooks-guide.md',
  sparql: 'https://www.w3.org/TR/sparql11-query/',
  rdf: 'https://www.w3.org/TR/rdf11-primer/',
  debugging: 'https://github.com/unrdf/docs/debugging.md',
};

// =============================================================================
// Zod Error Enhancement
// =============================================================================

/**
 * Extract received type from Zod error message
 * Zod 4.x doesn't include received field, so we parse it from message
 * @private
 */
function extractReceivedType(issue) {
  if (issue.received) {
    return issue.received;
  }

  // Parse from message: "Invalid input: expected array, received string"
  const match = issue.message.match(/received (\w+)/i);
  return match ? match[1] : undefined;
}

/**
 * Generate actionable fix suggestion based on Zod issue
 * @param {import('zod').ZodIssue} issue - Zod validation issue
 * @returns {string} Suggested fix
 */
function suggestFixForZodIssue(issue) {
  const path = issue.path.join('.') || 'value';

  // Type mismatch errors
  if (issue.code === 'invalid_type') {
    const expected = issue.expected;
    const received = extractReceivedType(issue);

    // Array/String confusion
    if (expected === 'array' && received === 'string') {
      return `Change ${path} from a string to an array: ["${path}"] or split the string`;
    }
    if (expected === 'string' && received === 'array') {
      return `Change ${path} from an array to a string: ${path}.join(',') or ${path}[0]`;
    }

    // Object/String confusion
    if (expected === 'object' && received === 'string') {
      return `Change ${path} from a string to an object: { value: "${path}" } or JSON.parse()`;
    }

    // Number/String confusion
    if (expected === 'number' && received === 'string') {
      return `Convert ${path} to a number: parseInt(${path}) or parseFloat(${path})`;
    }

    // Boolean confusion
    if (expected === 'boolean') {
      return `Change ${path} to a boolean: true/false (not "${received}")`;
    }

    return `Change ${path} from ${received} to ${expected}`;
  }

  // Missing required field
  if (issue.code === 'invalid_literal' || issue.message.includes('Required')) {
    return `Add required field: ${path}`;
  }

  // Invalid enum value
  if (issue.code === 'invalid_enum_value') {
    const options = issue.options?.join(', ') || 'check schema';
    return `Use one of these values for ${path}: ${options}`;
  }

  // String validation (min/max length)
  if (issue.code === 'too_small' && issue.type === 'string') {
    return `${path} must be at least ${issue.minimum} characters`;
  }
  if (issue.code === 'too_big' && issue.type === 'string') {
    return `${path} must be at most ${issue.maximum} characters`;
  }

  // Number validation (min/max)
  if (issue.code === 'too_small' && issue.type === 'number') {
    return `${path} must be >= ${issue.minimum}`;
  }
  if (issue.code === 'too_big' && issue.type === 'number') {
    return `${path} must be <= ${issue.maximum}`;
  }

  // Array validation
  if (issue.code === 'too_small' && issue.type === 'array') {
    return `${path} must have at least ${issue.minimum} items`;
  }
  if (issue.code === 'too_big' && issue.type === 'array') {
    return `${path} must have at most ${issue.maximum} items`;
  }

  // Custom validation (refine)
  if (issue.code === 'custom') {
    return `${path}: ${issue.message}`;
  }

  // Default
  return issue.message;
}

/**
 * Get documentation link for error context
 * @param {string} fieldPath - Field path from Zod error
 * @returns {string} Documentation URL
 */
function getDocsLinkForField(fieldPath) {
  if (fieldPath.includes('workflow') || fieldPath.includes('case')) {
    return DOCS_LINKS.workflow;
  }
  if (fieldPath.includes('hook')) {
    return DOCS_LINKS.hooks;
  }
  if (fieldPath.includes('query') || fieldPath.includes('sparql')) {
    return DOCS_LINKS.sparql;
  }
  if (fieldPath.includes('import') || fieldPath.includes('package')) {
    return DOCS_LINKS.imports;
  }
  return DOCS_LINKS.validation;
}

/**
 * Enhanced Zod error formatter with actionable fixes
 * @param {import('zod').ZodError} error - Zod validation error
 * @param {Object} options - Formatting options
 * @param {string} [options.operation] - Operation context (e.g., "workflow creation")
 * @param {boolean} [options.includeRawError] - Include original Zod error
 * @returns {Error} Enhanced error with context
 */
export function enhanceZodError(error, { operation = 'validation', includeRawError = false } = {}) {
  if (!(error instanceof z.ZodError)) {
    return error;
  }

  const issues = error.issues.map(issue => {
    const receivedType = issue.code === 'invalid_type' ? extractReceivedType(issue) : undefined;

    return {
      field: issue.path.join('.') || 'root',
      expected: issue.code === 'invalid_type' ? issue.expected : issue.message,
      received: receivedType,
      message: issue.message,
      fix: suggestFixForZodIssue(issue),
      docs: getDocsLinkForField(issue.path.join('.')),
    };
  });

  // Format first issue (most common case is single issue)
  const primary = issues[0];

  let message = `
❌ Validation Error in ${operation}

Field: ${primary.field}
Expected: ${primary.expected}
${primary.received ? `Received: ${primary.received}\n` : ''}
✅ Suggested Fix:
${primary.fix}

📖 Documentation: ${primary.docs}
`;

  // Add additional issues if present
  if (issues.length > 1) {
    message += `\n⚠️  Additional validation errors (${issues.length - 1}):\n`;
    issues.slice(1).forEach((issue, idx) => {
      message += `${idx + 2}. ${issue.field}: ${issue.fix}\n`;
    });
  }

  if (includeRawError) {
    message += `\nRaw Error:\n${JSON.stringify(error.issues, null, 2)}`;
  }

  const enhanced = new Error(message.trim());
  enhanced.name = 'ValidationError';
  enhanced.issues = issues;
  enhanced.originalError = error;

  return enhanced;
}

// =============================================================================
// Workflow Error Enhancement
// =============================================================================

/**
 * Enhanced workflow error with context and recovery
 */
export class WorkflowError extends Error {
  /**
   * @param {string} message - Error message
   * @param {Object} context - Error context
   * @param {string} [context.workflowId] - Workflow ID
   * @param {string} [context.caseId] - Case ID
   * @param {string} [context.taskId] - Task ID
   * @param {string} [context.state] - Current workflow state
   * @param {string} [context.reason] - Detailed failure reason
   * @param {Array<string>} [context.upstreamTasks] - Required upstream tasks
   * @param {Array<string>} [context.completedTasks] - Completed tasks
   * @param {string} [context.debugCommand] - Command to debug this error
   */
  constructor(message, context = {}) {
    const formatted = WorkflowError.formatMessage(message, context);
    super(formatted);
    this.name = 'WorkflowError';
    this.context = context;
    this.recovery = WorkflowError.suggestRecovery(context);
  }

  /**
   * Format workflow error message with context
   * @private
   */
  static formatMessage(message, context) {
    let formatted = `❌ Workflow Error: ${message}\n`;

    if (context.workflowId) {
      formatted += `Workflow: ${context.workflowId}\n`;
    }
    if (context.caseId) {
      formatted += `Case: ${context.caseId}\n`;
    }
    if (context.taskId) {
      formatted += `Task: ${context.taskId}\n`;
    }
    if (context.state) {
      formatted += `Current State: ${context.state}\n`;
    }

    if (context.reason) {
      formatted += `\n🔍 Reason: ${context.reason}\n`;
    }

    // Upstream task dependencies
    if (context.upstreamTasks?.length > 0) {
      formatted += `\n⏳ Required Upstream Tasks:\n`;
      context.upstreamTasks.forEach(task => {
        const completed = context.completedTasks?.includes(task);
        formatted += `  ${completed ? '✅' : '❌'} ${task}\n`;
      });
    }

    return formatted.trim();
  }

  /**
   * Suggest recovery actions based on error context
   * @private
   */
  static suggestRecovery(context) {
    const suggestions = [];

    // State-based recovery
    if (context.state === 'INVALID') {
      suggestions.push('🔄 Recreate workflow from checkpoint or restart case');
      suggestions.push('📊 Check workflow definition for invalid transitions');
    }

    if (context.state === 'DEADLOCK') {
      suggestions.push('🔓 Review task dependencies for circular references');
      suggestions.push('🧹 Clear stuck tasks: workflow.clearDeadlock(caseId)');
    }

    if (context.state === 'CANCELLED' || context.state === 'FAILED') {
      suggestions.push('🔍 Check logs for root cause of failure');
      suggestions.push('🔄 Restart case from last checkpoint');
    }

    // Upstream task issues
    if (context.upstreamTasks?.length > 0 && context.completedTasks) {
      const incomplete = context.upstreamTasks.filter(
        task => !context.completedTasks.includes(task)
      );
      if (incomplete.length > 0) {
        suggestions.push(`⏳ Wait for upstream tasks to complete: ${incomplete.join(', ')}`);
        suggestions.push('📋 Check task execution order in workflow definition');
      }
    }

    // Debug command
    if (context.debugCommand) {
      suggestions.push(`🐛 Debug: ${context.debugCommand}`);
    } else if (context.workflowId) {
      suggestions.push(`🐛 Debug: DEBUG=unrdf:workflow node your-script.mjs`);
    }

    // Documentation
    suggestions.push(`📖 Docs: ${DOCS_LINKS.workflow}`);

    return suggestions;
  }
}

// =============================================================================
// Import Error Enhancement
// =============================================================================

/**
 * Enhanced import error with dependency troubleshooting
 */
export class ImportError extends Error {
  /**
   * @param {string} packageName - Package that failed to import
   * @param {Error} originalError - Original import error
   * @param {Object} [context] - Additional context
   */
  constructor(packageName, originalError, context = {}) {
    const message = ImportError.formatMessage(packageName, originalError, context);
    super(message);
    this.name = 'ImportError';
    this.packageName = packageName;
    this.originalError = originalError;
    this.context = context;
  }

  /**
   * Format import error with actionable fixes
   * @private
   */
  static formatMessage(packageName, error, context) {
    let message = `❌ Cannot import package '${packageName}'\n`;

    if (error.code === 'ERR_MODULE_NOT_FOUND') {
      message += `\n🔍 Package not found in node_modules\n`;
      message += `\n✅ Possible fixes:\n`;
      message += `1. Install dependencies: pnpm install\n`;
      message += `2. Add to package.json: pnpm add ${packageName}\n`;
      message += `3. Check spelling: '${packageName}' (case-sensitive)\n`;
      message += `4. Verify package.json has "${packageName}" in dependencies\n`;

      message += `\n🔍 Verification steps:\n`;
      message += `- Check node_modules/${packageName} exists\n`;
      message += `- Check pnpm-lock.yaml is up to date\n`;
      message += `- Remove conflicting package-lock.json or yarn.lock files\n`;
      message += `- Try: rm -rf node_modules && pnpm install\n`;
    } else if (error.code === 'ERR_PACKAGE_PATH_NOT_EXPORTED') {
      message += `\n🔍 Package exists but path is not exported\n`;
      message += `\n✅ Possible fixes:\n`;
      message += `1. Check package.json "exports" field of ${packageName}\n`;
      message += `2. Import from correct subpath (e.g., '${packageName}/submodule')\n`;
      message += `3. Update ${packageName} to latest version: pnpm update ${packageName}\n`;
    } else if (error instanceof SyntaxError || error.name === 'SyntaxError' || error.message.includes('SyntaxError')) {
      message += `\n🔍 Syntax error in imported module\n`;
      message += `\n✅ Possible fixes:\n`;
      message += `1. Check ${packageName} is compatible with your Node.js version\n`;
      message += `2. Ensure using .mjs extension for ESM modules\n`;
      message += `3. Check package.json has "type": "module"\n`;
    } else {
      message += `\nOriginal error: ${error.message}\n`;
    }

    // Add context
    if (context.suggestion) {
      message += `\n💡 Suggestion: ${context.suggestion}\n`;
    }

    message += `\n📖 Docs: ${DOCS_LINKS.imports}`;

    return message.trim();
  }
}

/**
 * Wrap dynamic import with enhanced error handling
 * @param {string} packageName - Package to import
 * @param {Object} [options] - Import options
 * @returns {Promise<any>} Imported module
 * @throws {ImportError} Enhanced import error
 */
export async function safeImport(packageName, options = {}) {
  try {
    return await import(packageName);
  } catch (error) {
    throw new ImportError(packageName, error, options);
  }
}

// =============================================================================
// Debug Mode Support
// =============================================================================

/** @type {Set<string>} */
const DEBUG_NAMESPACES = new Set();

/**
 * Reset debug mode (primarily for testing)
 * @private
 */
export function resetDebugMode() {
  DEBUG_NAMESPACES.clear();
}

/**
 * Initialize debug mode from environment variable
 * Supports patterns like: DEBUG=unrdf:*, DEBUG=unrdf:workflow, DEBUG=*
 */
export function initializeDebugMode() {
  // Clear existing patterns first
  DEBUG_NAMESPACES.clear();

  const debugEnv = process.env.DEBUG;
  if (!debugEnv) {
    return;
  }

  // Parse DEBUG patterns
  const patterns = debugEnv.split(',').map(p => p.trim());
  patterns.forEach(pattern => {
    if (pattern === '*') {
      DEBUG_NAMESPACES.add('*');
    } else if (pattern.endsWith(':*')) {
      DEBUG_NAMESPACES.add(pattern);
    } else {
      DEBUG_NAMESPACES.add(pattern);
    }
  });
}

/**
 * Check if debug is enabled for namespace
 * @param {string} namespace - Debug namespace (e.g., "unrdf:workflow")
 * @returns {boolean} True if debugging enabled
 */
export function isDebugEnabled(namespace) {
  if (DEBUG_NAMESPACES.size === 0) {
    return false;
  }

  // Check for exact match
  if (DEBUG_NAMESPACES.has(namespace)) {
    return true;
  }

  // Check for wildcard match
  if (DEBUG_NAMESPACES.has('*')) {
    return true;
  }

  // Check for prefix match (e.g., "unrdf:*" matches "unrdf:workflow")
  for (const pattern of DEBUG_NAMESPACES) {
    if (pattern.endsWith(':*')) {
      const prefix = pattern.slice(0, -2);
      if (namespace.startsWith(prefix)) {
        return true;
      }
    }
  }

  return false;
}

/**
 * Create debug logger for namespace
 * @param {string} namespace - Debug namespace
 * @returns {Function} Debug logging function
 */
export function createDebugger(namespace) {
  return (message, data) => {
    if (!isDebugEnabled(namespace)) {
      return;
    }

    const timestamp = new Date().toISOString();
    console.error(`[${timestamp}] ${namespace}: ${message}`);

    if (data !== undefined) {
      console.error(JSON.stringify(data, null, 2));
    }
  };
}

/**
 * Trace workflow execution step
 * @param {string} step - Execution step name
 * @param {Object} data - Step data
 */
export function traceWorkflowStep(step, data) {
  const debug = createDebugger('unrdf:workflow');
  debug(`Execution trace: ${step}`, {
    step,
    timestamp: Date.now(),
    ...data,
  });
}

// =============================================================================
// Error Recovery Documentation
// =============================================================================

/**
 * Get error recovery guide for error type
 * @param {Error} error - Error to get recovery guide for
 * @returns {string} Recovery guide markdown
 */
export function getErrorRecoveryGuide(error) {
  if (error instanceof WorkflowError) {
    return `
# Workflow Error Recovery Guide

## Error Context
${error.message}

## Recovery Steps
${error.recovery.map((step, i) => `${i + 1}. ${step}`).join('\n')}

## Prevention
- Validate workflow definition before deployment
- Use workflow.validate() to check for issues
- Test workflows in development environment
- Monitor workflow health: workflow.getHealth()

## Resources
- Workflow Patterns: ${DOCS_LINKS.workflow}
- YAWL Quickstart: ${DOCS_LINKS.yawl}
- Debugging Guide: ${DOCS_LINKS.debugging}
`;
  }

  if (error instanceof ImportError) {
    return `
# Import Error Recovery Guide

## Error Context
Package: ${error.packageName}
Error: ${error.originalError.message}

## Quick Fixes
1. \`pnpm install\` - Install all dependencies
2. \`pnpm add ${error.packageName}\` - Add missing package
3. \`rm -rf node_modules && pnpm install\` - Clean reinstall

## Verification
- [ ] package.json lists "${error.packageName}"
- [ ] node_modules/${error.packageName} exists
- [ ] pnpm-lock.yaml is up to date
- [ ] No package-lock.json or yarn.lock conflicts

## Resources
- Package Setup: ${DOCS_LINKS.imports}
- Debugging Guide: ${DOCS_LINKS.debugging}
`;
  }

  if (error.name === 'ValidationError') {
    return `
# Validation Error Recovery Guide

## Error Context
${error.message}

## Common Causes
- Type mismatch (string vs array, object vs primitive)
- Missing required fields
- Invalid enum values
- Out of range values

## Quick Fixes
1. Check the expected type vs what you provided
2. Consult schema documentation for field requirements
3. Use TypeScript or JSDoc for type checking
4. Validate input data before processing

## Resources
- Validation Guide: ${DOCS_LINKS.validation}
- Debugging Guide: ${DOCS_LINKS.debugging}
`;
  }

  return `
# Error Recovery Guide

## Error
${error.message}

## General Troubleshooting
1. Check error message for specific field/value issues
2. Enable debug mode: DEBUG=unrdf:* node script.mjs
3. Review recent code changes
4. Check documentation for correct API usage
5. Search issues: https://github.com/unrdf/issues

## Resources
- Debugging Guide: ${DOCS_LINKS.debugging}
`;
}

// =============================================================================
// Initialization
// =============================================================================

// Auto-initialize debug mode on module load
initializeDebugMode();

// =============================================================================
// Exports
// =============================================================================

export default {
  enhanceZodError,
  WorkflowError,
  ImportError,
  safeImport,
  createDebugger,
  traceWorkflowStep,
  getErrorRecoveryGuide,
  isDebugEnabled,
  DOCS_LINKS,
};
