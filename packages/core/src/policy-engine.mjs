/** Deterministic policy evaluation; decisions never actuate. */
export class PolicyEngine {
  #policies = [];
  /**
   * Add a policy; policies run by descending priority, then id.
   * @param {Object} policy - Policy definition.
   * @param {string} policy.id - Unique policy id.
   * @param {number} [policy.priority] - Higher runs first.
   * @param {Function} [policy.when] - Applicability predicate `(subject, context)`; defaults to always.
   * @param {Function} policy.decide - Returns `{effect: 'PERMIT'|'REFUSE'|'ABSTAIN', ...}`.
   * @returns {PolicyEngine} This engine, for chaining.
   * @throws {TypeError} If id, when or decide is invalid.
   * @throws {Error} If the id is duplicated.
   */
  add({ id, priority = 0, when = () => true, decide }) {
    if (!id || typeof when !== 'function' || typeof decide !== 'function')
      throw new TypeError('policy id, when, and decide are required');
    if (this.#policies.some(policy => policy.id === id)) throw new Error(`POLICY_DUPLICATE:${id}`);
    this.#policies.push({ id, priority, when, decide });
    this.#policies.sort((a, b) => b.priority - a.priority || a.id.localeCompare(b.id));
    return this;
  }
  /**
   * Evaluate applicable policies until one does not abstain.
   * @param {*} subject - Subject under evaluation.
   * @param {Object} [context] - Context passed to policies.
   * @returns {Promise<Object>} The first non-ABSTAIN decision with `policy` and `trace`, or a REFUSE (NO_POLICY_PERMITTED) if none decides.
   * @throws {Error} If a policy returns an invalid decision.
   */
  async evaluate(subject, context = {}) {
    const trace = [];
    for (const policy of this.#policies) {
      const applicable = await policy.when(subject, context);
      if (!applicable) {
        trace.push({ id: policy.id, applicable: false });
        continue;
      }
      const decision = await policy.decide(subject, context);
      if (!decision || !['PERMIT', 'REFUSE', 'ABSTAIN'].includes(decision.effect))
        throw new Error(`POLICY_INVALID_DECISION:${policy.id}`);
      trace.push({ id: policy.id, applicable: true, decision });
      if (decision.effect !== 'ABSTAIN') return { ...decision, policy: policy.id, trace };
    }
    return { effect: 'REFUSE', code: 'NO_POLICY_PERMITTED', policy: null, trace };
  }
}
/**
 * Create an empty policy engine.
 * @returns {PolicyEngine} A new engine.
 */
export function createPolicyEngine() {
  return new PolicyEngine();
}
