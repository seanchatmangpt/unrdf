/** Deterministic policy evaluation; decisions never actuate. */
export class PolicyEngine {
  #policies = [];
  add({ id, priority = 0, when = () => true, decide }) {
    if (!id || typeof when !== 'function' || typeof decide !== 'function') throw new TypeError('policy id, when, and decide are required');
    if (this.#policies.some(policy => policy.id === id)) throw new Error(`POLICY_DUPLICATE:${id}`);
    this.#policies.push({ id, priority, when, decide });
    this.#policies.sort((a, b) => b.priority - a.priority || a.id.localeCompare(b.id));
    return this;
  }
  async evaluate(subject, context = {}) {
    const trace = [];
    for (const policy of this.#policies) {
      const applicable = await policy.when(subject, context);
      if (!applicable) { trace.push({ id: policy.id, applicable: false }); continue; }
      const decision = await policy.decide(subject, context);
      if (!decision || !['PERMIT', 'REFUSE', 'ABSTAIN'].includes(decision.effect)) throw new Error(`POLICY_INVALID_DECISION:${policy.id}`);
      trace.push({ id: policy.id, applicable: true, decision });
      if (decision.effect !== 'ABSTAIN') return { ...decision, policy: policy.id, trace };
    }
    return { effect: 'REFUSE', code: 'NO_POLICY_PERMITTED', policy: null, trace };
  }
}
export function createPolicyEngine() { return new PolicyEngine(); }
