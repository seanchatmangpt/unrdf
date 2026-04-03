// self-observing-system.mjs
// A system that detects its own contradictions during execution

export class SelfObservingSystem {
  constructor(name) {
    this.name = name;
    this.executions = [];
    this.claims = [];
    this.failures = [];
    this.state = new Map();
    this.observers = [];
  }

  claim(statement) {
    this.claims.push({
      time: Date.now(),
      statement,
      verified: null
    });
  }

  execute(computation, inputs) {
    const executionId = this.executions.length;
    const startState = new Map(this.state);

    const execution = {
      id: executionId,
      computation: computation.name,
      inputs,
      startState: Object.fromEntries(startState),
      startTime: Date.now(),
      result: null,
      endTime: null,
      endState: null,
      contradictions: []
    };

    try {
      const result = computation.fn(inputs, this.state);
      execution.result = result;
      execution.endState = Object.fromEntries(this.state);
      execution.endTime = Date.now();

      // Check claims against result
      const violations = this._checkClaims(execution);
      if (violations.length > 0) {
        execution.contradictions = violations;
        this.failures.push(execution);
      }

      this._notifyObservers(execution);
      this.executions.push(execution);
      return result;
    } catch (e) {
      execution.result = null;
      execution.endTime = Date.now();
      execution.contradictions.push({
        type: 'runtime_error',
        message: e.message
      });
      this.failures.push(execution);
      this._notifyObservers(execution);
      throw e;
    }
  }

  _checkClaims(execution) {
    const violations = [];

    for (const claim of this.claims) {
      if (claim.verified !== null) continue; // Already checked

      // Try to verify claim against execution
      const verified = this._verifyClaim(claim, execution);

      if (!verified) {
        violations.push({
          claim: claim.statement,
          execution: execution.computation,
          contradiction: `claim made at ${claim.time} not satisfied by execution`
        });
      }
    }

    return violations;
  }

  _verifyClaim(claim, execution) {
    // Simple pattern matching
    if (claim.statement.includes('produces')) {
      return execution.result !== undefined && execution.result !== null;
    }
    if (claim.statement.includes('idempotent')) {
      // Would need repeated execution to verify
      return true; // Defer
    }
    if (claim.statement.includes('no side effects')) {
      return JSON.stringify(execution.startState) ===
             JSON.stringify(execution.endState);
    }
    return true; // Unknown claims are deferred
  }

  observe(observer) {
    this.observers.push(observer);
  }

  _notifyObservers(execution) {
    for (const observer of this.observers) {
      observer(this.name, execution);
    }
  }

  getFailures() {
    return this.failures;
  }

  getState() {
    return Object.fromEntries(this.state);
  }

  setState(key, value) {
    this.state.set(key, value);
  }
}
