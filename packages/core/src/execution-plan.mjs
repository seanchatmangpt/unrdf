/** Dependency-closed deterministic execution plans. */
export class ExecutionPlan {
  #steps = new Map();

  add(step) {
    const { id, run, dependsOn = [], verify = null, compensate = null } = step ?? {};
    if (!id || typeof run !== 'function') throw new TypeError('step.id and step.run are required');
    if (this.#steps.has(id)) throw new Error(`STEP_DUPLICATE:${id}`);
    this.#steps.set(id, { id, run, dependsOn: [...new Set(dependsOn)].sort(), verify, compensate });
    return this;
  }

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

export function createExecutionPlan() { return new ExecutionPlan(); }
