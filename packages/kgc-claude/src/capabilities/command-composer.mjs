/**
 * Command Composer - Command chaining, piping, and macro composition
 *
 * Hyper-advanced capability for composing Claude Code slash commands
 * into workflows with conditional execution, error handling, and state passing.
 *
 * @module @unrdf/kgc-claude/capabilities/command-composer
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

/**
 * Command invocation schema
 */
const CommandInvocationSchema = z.object({
  command: z.string().min(1), // Command name (e.g., '/research:explore')
  args: z.record(z.string(), z.any()).default({}),
  timeout: z.number().int().positive().default(120000), // 2 minutes default
  retries: z.number().int().min(0).max(3).default(0),
  condition: z.string().optional(), // JavaScript expression for conditional execution
});

/**
 * Composition step schema
 */
const CompositionStepSchema = z.object({
  id: z.string().min(1),
  type: z.enum(['command', 'parallel', 'conditional', 'loop', 'error-boundary']),
  invocation: CommandInvocationSchema.optional(),
  steps: z.lazy(() => z.array(CompositionStepSchema)).optional(), // Recursive for nested steps
  condition: z.string().optional(),
  errorHandler: z.string().optional(), // Command to run on error
  maxIterations: z.number().int().positive().optional(), // For loops
});

/**
 * Command macro schema - reusable composition
 */
export const CommandMacroSchema = z.object({
  name: z.string().min(1).regex(/^[a-z][a-z0-9-]*$/i),
  description: z.string().min(1),
  version: z.string().regex(/^\d+\.\d+\.\d+$/).default('1.0.0'),
  parameters: z.array(z.string()).default([]),
  steps: z.array(CompositionStepSchema),
  onSuccess: z.string().optional(), // Command to run on success
  onError: z.string().optional(), // Command to run on error
  timeout: z.number().int().positive().default(300000), // 5 minutes default
});

/**
 * @typedef {z.infer<typeof CommandMacroSchema>} CommandMacro
 * @typedef {z.infer<typeof CompositionStepSchema>} CompositionStep
 * @typedef {z.infer<typeof CommandInvocationSchema>} CommandInvocation
 */

/**
 * Command Composer - Fluent API for building command compositions
 *
 * @example
 * const workflow = new CommandComposer('research-workflow')
 *   .step('scan', { command: '/kgc:scan', args: { scope: 'docs' } })
 *   .step('prove', { command: '/kgc:prove', args: { doc: '$scan.output' } })
 *   .onError('/notify-failure')
 *   .build();
 */
export class CommandComposer {
  /**
   * @param {string} name - Macro name
   */
  constructor(name) {
    /** @type {Partial<CommandMacro>} */
    this.macro = {
      name,
      description: '',
      version: '1.0.0',
      parameters: [],
      steps: [],
      timeout: 300000,
    };
  }

  /**
   * Set macro description
   * @param {string} description
   * @returns {CommandComposer}
   */
  description(description) {
    this.macro.description = description;
    return this;
  }

  /**
   * Set macro version
   * @param {string} version
   * @returns {CommandComposer}
   */
  setVersion(version) {
    this.macro.version = version;
    return this;
  }

  /**
   * Add a parameter that can be passed to this macro
   * @param {string} param - Parameter name
   * @returns {CommandComposer}
   */
  addParameter(param) {
    this.macro.parameters = this.macro.parameters || [];
    this.macro.parameters.push(param);
    return this;
  }

  /**
   * Add a sequential command step
   * @param {string} id - Step identifier
   * @param {Partial<CommandInvocation>} invocation - Command invocation
   * @returns {CommandComposer}
   */
  step(id, invocation) {
    this.macro.steps = this.macro.steps || [];
    this.macro.steps.push({
      id,
      type: 'command',
      invocation: {
        command: invocation.command || '',
        args: invocation.args || {},
        timeout: invocation.timeout || 120000,
        retries: invocation.retries || 0,
        condition: invocation.condition,
      },
    });
    return this;
  }

  /**
   * Add parallel command execution
   * @param {string} id - Step identifier
   * @param {Array<{ id: string, invocation: Partial<CommandInvocation> }>} parallelSteps
   * @returns {CommandComposer}
   */
  parallel(id, parallelSteps) {
    this.macro.steps = this.macro.steps || [];
    const steps = parallelSteps.map((ps) => ({
      id: ps.id,
      type: 'command',
      invocation: {
        command: ps.invocation.command || '',
        args: ps.invocation.args || {},
        timeout: ps.invocation.timeout || 120000,
        retries: ps.invocation.retries || 0,
      },
    }));

    this.macro.steps.push({
      id,
      type: 'parallel',
      steps,
    });
    return this;
  }

  /**
   * Add conditional execution
   * @param {string} id - Step identifier
   * @param {string} condition - JavaScript expression evaluating to boolean
   * @param {CompositionStep[]} thenSteps - Steps to execute if condition is true
   * @param {CompositionStep[]} [elseSteps] - Steps to execute if condition is false
   * @returns {CommandComposer}
   */
  conditional(id, condition, thenSteps, elseSteps = []) {
    this.macro.steps = this.macro.steps || [];
    this.macro.steps.push({
      id,
      type: 'conditional',
      condition,
      steps: thenSteps,
      // Store else steps in a sub-step with inverted condition
      ...(elseSteps.length > 0 && {
        errorHandler: JSON.stringify(elseSteps),
      }),
    });
    return this;
  }

  /**
   * Add loop execution
   * @param {string} id - Step identifier
   * @param {CompositionStep[]} steps - Steps to repeat
   * @param {number} maxIterations - Maximum iterations
   * @returns {CommandComposer}
   */
  loop(id, steps, maxIterations) {
    this.macro.steps = this.macro.steps || [];
    this.macro.steps.push({
      id,
      type: 'loop',
      steps,
      maxIterations,
    });
    return this;
  }

  /**
   * Add error boundary with fallback
   * @param {string} id - Step identifier
   * @param {CompositionStep[]} steps - Steps to try
   * @param {string} errorHandler - Command to run on error
   * @returns {CommandComposer}
   */
  errorBoundary(id, steps, errorHandler) {
    this.macro.steps = this.macro.steps || [];
    this.macro.steps.push({
      id,
      type: 'error-boundary',
      steps,
      errorHandler,
    });
    return this;
  }

  /**
   * Set success handler
   * @param {string} command - Command to run on success
   * @returns {CommandComposer}
   */
  onSuccess(command) {
    this.macro.onSuccess = command;
    return this;
  }

  /**
   * Set error handler
   * @param {string} command - Command to run on error
   * @returns {CommandComposer}
   */
  onError(command) {
    this.macro.onError = command;
    return this;
  }

  /**
   * Set overall timeout
   * @param {number} timeout - Timeout in milliseconds
   * @returns {CommandComposer}
   */
  setTimeout(timeout) {
    this.macro.timeout = timeout;
    return this;
  }

  /**
   * Build and validate the macro
   * @returns {CommandMacro}
   * @throws {z.ZodError} If validation fails
   */
  build() {
    return CommandMacroSchema.parse(this.macro);
  }
}

/**
 * Pipe operator - chain commands with output passing
 *
 * @param {...CommandInvocation} commands - Commands to pipe
 * @returns {CommandMacro} Macro with piped commands
 *
 * @example
 * const pipeline = pipe(
 *   { command: '/kgc:scan', args: { scope: 'docs' } },
 *   { command: '/kgc:frontier', args: {} },
 *   { command: '/kgc:prove', args: { doc: '$frontier.output' } }
 * );
 */
export function pipe(...commands) {
  const composer = new CommandComposer(`pipe-${Date.now()}`);
  composer.description('Auto-generated pipeline');

  commands.forEach((cmd, i) => {
    composer.step(`step-${i}`, cmd);
  });

  return composer.build();
}

/**
 * Render macro to executable markdown format
 *
 * @param {CommandMacro} macro - Macro definition
 * @returns {string} Markdown representation
 */
export function renderMacroMarkdown(macro) {
  const lines = [];

  lines.push('---');
  lines.push(`name: ${macro.name}`);
  lines.push(`description: ${macro.description}`);
  lines.push(`version: ${macro.version}`);
  lines.push(`type: macro`);
  if (macro.parameters && macro.parameters.length > 0) {
    lines.push('parameters:');
    macro.parameters.forEach((p) => lines.push(`  - ${p}`));
  }
  lines.push('---');
  lines.push('');

  lines.push(`# ${macro.name}`);
  lines.push('');
  lines.push(macro.description);
  lines.push('');

  lines.push('## Execution Steps');
  lines.push('');

  const renderStep = (step, indent = 0) => {
    const prefix = '  '.repeat(indent);
    lines.push(`${prefix}- **${step.id}** (${step.type})`);

    if (step.invocation) {
      lines.push(`${prefix}  - Command: \`${step.invocation.command}\``);
      if (Object.keys(step.invocation.args || {}).length > 0) {
        lines.push(`${prefix}  - Args: \`${JSON.stringify(step.invocation.args)}\``);
      }
      if (step.invocation.timeout) {
        lines.push(`${prefix}  - Timeout: ${step.invocation.timeout}ms`);
      }
      if (step.invocation.condition) {
        lines.push(`${prefix}  - Condition: \`${step.invocation.condition}\``);
      }
    }

    if (step.condition) {
      lines.push(`${prefix}  - Condition: \`${step.condition}\``);
    }

    if (step.errorHandler) {
      lines.push(`${prefix}  - Error Handler: \`${step.errorHandler}\``);
    }

    if (step.maxIterations) {
      lines.push(`${prefix}  - Max Iterations: ${step.maxIterations}`);
    }

    if (step.steps && step.steps.length > 0) {
      lines.push(`${prefix}  - Sub-steps:`);
      step.steps.forEach((s) => renderStep(s, indent + 2));
    }
  };

  macro.steps.forEach((step) => renderStep(step));

  lines.push('');

  if (macro.onSuccess) {
    lines.push(`## On Success: \`${macro.onSuccess}\``);
    lines.push('');
  }

  if (macro.onError) {
    lines.push(`## On Error: \`${macro.onError}\``);
    lines.push('');
  }

  lines.push(`## Total Timeout: ${macro.timeout}ms`);
  lines.push('');

  return lines.join('\n');
}

/**
 * Compute execution graph from macro
 *
 * @param {CommandMacro} macro
 * @returns {{ nodes: Array<{ id: string, command: string }>, edges: Array<{ from: string, to: string }> }}
 */
export function computeExecutionGraph(macro) {
  const nodes = [];
  const edges = [];

  const traverse = (steps, parentId = null) => {
    steps.forEach((step, i) => {
      nodes.push({
        id: step.id,
        command: step.invocation?.command || step.type,
        type: step.type,
      });

      if (parentId) {
        edges.push({ from: parentId, to: step.id });
      }

      if (step.steps && step.steps.length > 0) {
        traverse(step.steps, step.id);
      }

      // Link sequential steps
      if (i > 0 && step.type !== 'parallel') {
        edges.push({ from: steps[i - 1].id, to: step.id });
      }
    });
  };

  traverse(macro.steps);

  return { nodes, edges };
}

/**
 * Validate macro for common issues
 *
 * @param {CommandMacro} macro
 * @returns {{ valid: boolean, issues: string[] }}
 */
export function validateMacro(macro) {
  const issues = [];

  // Check for circular dependencies
  const graph = computeExecutionGraph(macro);
  const visited = new Set();
  const recursionStack = new Set();

  const hasCycle = (nodeId) => {
    if (!visited.has(nodeId)) {
      visited.add(nodeId);
      recursionStack.add(nodeId);

      const outEdges = graph.edges.filter((e) => e.from === nodeId);
      for (const edge of outEdges) {
        if (!visited.has(edge.to) && hasCycle(edge.to)) {
          return true;
        } else if (recursionStack.has(edge.to)) {
          return true;
        }
      }
    }

    recursionStack.delete(nodeId);
    return false;
  };

  graph.nodes.forEach((node) => {
    if (hasCycle(node.id)) {
      issues.push(`Circular dependency detected involving step: ${node.id}`);
    }
  });

  // Check for unreachable steps
  const reachable = new Set();
  const markReachable = (nodeId) => {
    if (!reachable.has(nodeId)) {
      reachable.add(nodeId);
      const outEdges = graph.edges.filter((e) => e.from === nodeId);
      outEdges.forEach((e) => markReachable(e.to));
    }
  };

  if (graph.nodes.length > 0) {
    markReachable(graph.nodes[0].id);
  }

  graph.nodes.forEach((node) => {
    if (!reachable.has(node.id)) {
      issues.push(`Unreachable step: ${node.id}`);
    }
  });

  // Check for excessive timeout
  if (macro.timeout > 600000) {
    // 10 minutes
    issues.push(`Timeout exceeds recommended maximum: ${macro.timeout}ms (max: 600000ms)`);
  }

  // Check for missing error handlers on long-running steps
  macro.steps.forEach((step) => {
    if (step.invocation && step.invocation.timeout > 60000 && !step.errorHandler) {
      issues.push(`Long-running step '${step.id}' lacks error handler (timeout: ${step.invocation.timeout}ms)`);
    }
  });

  return {
    valid: issues.length === 0,
    issues,
  };
}

/**
 * Simulate macro execution (dry run)
 *
 * @param {CommandMacro} macro
 * @param {Record<string, any>} context - Execution context (parameters, state)
 * @returns {Promise<{ executedSteps: string[], estimatedDuration: number, warnings: string[] }>}
 */
export async function simulateMacroExecution(macro, context = {}) {
  const executedSteps = [];
  const warnings = [];
  let estimatedDuration = 0;

  const simulateStep = async (step) => {
    // Check condition if present
    if (step.condition) {
      try {
        // Simple condition evaluation (would need safe eval in production)
        const conditionMet = true; // Placeholder
        if (!conditionMet) {
          warnings.push(`Step '${step.id}' skipped due to condition: ${step.condition}`);
          return;
        }
      } catch (err) {
        warnings.push(`Invalid condition in step '${step.id}': ${err.message}`);
      }
    }

    executedSteps.push(step.id);

    if (step.invocation) {
      estimatedDuration += step.invocation.timeout || 120000;
    }

    if (step.steps && step.steps.length > 0) {
      if (step.type === 'parallel') {
        // Parallel steps - max of individual durations
        const durations = await Promise.all(step.steps.map(simulateStep));
        const maxDuration = Math.max(...durations.map((d) => d || 0));
        estimatedDuration += maxDuration;
      } else {
        // Sequential steps - sum of durations
        for (const subStep of step.steps) {
          await simulateStep(subStep);
        }
      }
    }
  };

  for (const step of macro.steps) {
    await simulateStep(step);
  }

  return {
    executedSteps,
    estimatedDuration,
    warnings,
  };
}

/**
 * Compute macro hash for versioning
 *
 * @param {CommandMacro} macro
 * @returns {Promise<string>}
 */
export async function computeMacroHash(macro) {
  const normalized = {
    name: macro.name,
    version: macro.version,
    steps: JSON.stringify(macro.steps),
  };
  const serialized = JSON.stringify(normalized, Object.keys(normalized).sort());
  return blake3(serialized);
}

/**
 * Quick helper to create a composer
 *
 * @param {string} name - Macro name
 * @returns {CommandComposer}
 */
export function compose(name) {
  return new CommandComposer(name);
}
