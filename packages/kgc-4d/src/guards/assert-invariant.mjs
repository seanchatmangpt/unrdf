export function assertInvariant(state, invariant, contextFn) {
  if (typeof invariant !== 'function') {
    throw new TypeError('assertInvariant: invariant must be a function');
  }
  if (typeof contextFn !== 'function') {
    throw new TypeError('assertInvariant: contextFn must be a function');
  }
  if (!invariant(state)) {
    const context = contextFn(state);
    const error = new Error(
      'Invariant violation: ' + context.violations + '\n' +
      'Fix: ' + context.fix + '\n' +
      'Context: ' + JSON.stringify(context, null, 2)
    );
    error.invariantContext = context;
    throw error;
  }
  return true;
}

export function assertInvariants(state, checks) {
  if (!Array.isArray(checks)) {
    throw new TypeError('assertInvariants: checks must be an array');
  }
  const violations = [];
  for (const check of checks) {
    if (!check.invariant || !check.context) {
      throw new TypeError('assertInvariants: Each check must have invariant and context functions');
    }
    if (!check.invariant(state)) {
      violations.push(check.context(state));
    }
  }
  return { valid: violations.length === 0, violations };
}

export function createInvariantChecker(invariant, contextFn) {
  return (state) => assertInvariant(state, invariant, contextFn);
}
