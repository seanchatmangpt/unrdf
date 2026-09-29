/** Dependency-closed deterministic execution plans. */
export class ExecutionPlan {
  #steps = new Map();

  /**
   * Add a step.
   * @param {Object} step - Step definition.
   * @param {string} step.id - Unique step id.
   * @param {Function} step.run - Async function `({context, inputs})` producing the output.
   * @param {string[]} [step.dependsOn] - Ids of prerequisite steps.
   * @param {Function|null} [step.verify] - Function `(output, {context, inputs})` that must return true.
   * @param {Function|null} [step.compensate] - Function `(output, {context})` to undo the step on later failure.
   * @returns {ExecutionPlan} This plan, for chaining.
   * @throws {TypeError} If id or run is missing.
   * @throws {Error} If the id is duplicated.
   */
  add(step) {
    const { id, run, dependsOn = [], verify = null, compensate = null } = step ?? {};
    if (!id || typeof run !== 'function') throw new TypeError('step.id and step.run are required');
    if (this.#steps.has(id)) throw new Error(`STEP_DUPLICATE:${id}`);
    this.#steps.set(id, { id, run, dependsOn: [...new Set(dependsOn)].sort(), verify, compensate });
    return this;
  }

  /**
   * Topologically sort steps (dependencies first, ties by id).
   * @returns {string[]} Step ids in execution order.
   * @throws {Error} On a cycle (PLAN_CYCLE) or unknown dependency (STEP_NOT_FOUND).
   */
  order() {
    const permanent = new Set();
    const temporary = new Set();
    const ordered = [];
    const visit = id => {
      if (permanent.has(id)) return;
      if (temporary.has(id)) throw new Error(`PLAN_CYCLE:${id}`);
      const step = this.#steps.get(id);
      if (!step) throw new Error(`STEP_NOT_FOUND:${id}`);
      temporary.add(id);
      for (const dependency of step.dependsOn) visit(dependency);
      temporary.delete(id);
      permanent.add(id);
      ordered.push(id);
    };
    for (const id of [...this.#steps.keys()].sort()) visit(id);
    return ordered;
  }

  /**
   * Run the steps in order, verifying each and compensating completed steps in reverse on failure.
   * @param {Object} [context] - Context passed to every step.
   * @param {Object} [options] - Execution options.
   * @param {Object|null} [options.receiptChain] - Chain to append a receipt per step.
   * @param {boolean} [options.stopOnFailure] - Rethrow the first error; otherwise record it and continue.
   * @returns {Promise<Object>} Map of step id to output (or `{error}` for failures when not stopping).
   */
  async execute(context = {}, { receiptChain = null, stopOnFailure = true } = {}) {
    const results = new Map();
    const completed = [];
    for (const id of this.order()) {
      const step = this.#steps.get(id);
      const inputs = Object.fromEntries(step.dependsOn.map(dependency => [dependency, results.get(dependency)]));
      try {
        const output = await step.run({ context, inputs });
        if (step.verify) {
          const verified = await step.verify(output, { context, inputs });
          if (verified !== true) throw new Error(`STEP_VERIFICATION_FAILED:${id}`);
        }
        results.set(id, output);
        completed.push(id);
        receiptChain?.append({ action: id, inputs, outputs: output, result: 'success', verifier: step.verify?.name || null });
      } catch (error) {
        receiptChain?.append({ action: id, inputs, outputs: {}, result: 'error', verifier: step.verify?.name || null, exclusions: [error.message] });
        for (const completedId of completed.reverse()) {
          const completedStep = this.#steps.get(completedId);
          if (completedStep.compensate) await completedStep.compensate(results.get(completedId), { context });
        }
        if (stopOnFailure) throw error;
        results.set(id, { error: error.message });
      }
    }
    return Object.fromEntries(results);
  }
}

/**
 * Create an empty execution plan.
 * @returns {ExecutionPlan} A new plan.
 */
export function createExecutionPlan() { return new ExecutionPlan(); }
