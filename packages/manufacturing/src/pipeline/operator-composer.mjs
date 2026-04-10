/**
 * @file Operator Composer
 * @module manufacturing/pipeline/operator-composer
 * @description DAG-based pipeline composition with parallel execution support
 */

/**
 *
 */
export class OperatorComposer {
  /**
   *
   */
  constructor() {
    this._steps = [];
  }

  /**
   * Add a step to the pipeline.
   * @param {string} operatorName
   * @param {object} input
   * @param {{ id?: string, dependsOn?: string[], timeoutMs?: number, parallel?: boolean }} opts
   * @returns {OperatorComposer}
   */
  add(operatorName, input, opts = {}) {
    this._steps.push({
      id: opts.id || `step-${this._steps.length}`,
      operatorName,
      input,
      dependsOn: opts.dependsOn || [],
      timeoutMs: opts.timeoutMs,
      parallel: opts.parallel || false,
      status: 'pending',
      result: undefined,
      error: undefined,
    });
    return this;
  }

  /**
   * Get the execution order respecting dependencies.
   * @returns {string[][]} Array of parallel groups
   */
  getExecutionOrder() {
    const executed = new Set();
    const groups = [];
    let remaining = [...this._steps];

    while (remaining.length > 0) {
      const ready = remaining.filter(
        step => !executed.has(step.id) && step.dependsOn.every(dep => executed.has(dep)),
      );
      const parallel = ready.filter(step => step.parallel);
      const sequential = ready.filter(step => !step.parallel);

      // Execute sequential items first (preserves order)
      for (const step of sequential) {
        groups.push([step.id]);
        executed.add(step.id);
      }

      // Then execute parallel items as a group
      if (parallel.length > 0) {
        groups.push(parallel.map(s => s.id));
        for (const step of parallel) executed.add(step.id);
      }

      if (ready.length === 0 && remaining.length > 0) {
        throw new Error(`Circular dependency detected among steps: ${remaining.map(s => s.id).join(', ')}`);
      }

      remaining = remaining.filter(step => !executed.has(step.id));
    }

    return groups;
  }

  /**
   * Get all steps.
   * @returns {Array}
   */
  get steps() {
    return [...this._steps];
  }

  /**
   * Get step by ID.
   * @param {string} id
   * @returns {object|undefined}
   */
  getStep(id) {
    return this._steps.find(s => s.id === id);
  }

  /**
   * Get execution plan summary.
   * @returns {{ groups: string[][], totalSteps: number, hasParallel: boolean }}
   */
  get plan() {
    const groups = this.getExecutionOrder();
    return {
      groups,
      totalSteps: this._steps.length,
      hasParallel: groups.some(g => g.length > 1),
    };
  }

  /**
   * Reset all step statuses (for re-execution).
   */
  reset() {
    for (const step of this._steps) {
      step.status = 'pending';
      step.result = undefined;
      step.error = undefined;
    }
  }
}
