export function composeGuards(...guards) {
  if (guards.length === 0) {
    throw new Error('composeGuards: At least one guard required');
  }
  return function composedGuard(...args) {
    for (const guard of guards) {
      if (typeof guard !== 'function') {
        throw new TypeError('composeGuards: All guards must be functions');
      }
      guard(...args);
    }
    return true;
  };
}

export function composeGuardsAccumulate(...guards) {
  if (guards.length === 0) {
    throw new Error('composeGuardsAccumulate: At least one guard required');
  }
  return function composedGuard(...args) {
    const errors = [];
    for (const guard of guards) {
      if (typeof guard !== 'function') {
        errors.push('Invalid guard: not a function');
        continue;
      }
      try {
        guard(...args);
      } catch (error) {
        errors.push(error.message);
      }
    }
    return { passed: errors.length === 0, errors };
  };
}
