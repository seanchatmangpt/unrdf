/**
 * @file Routing decision logic for partial serve phase
 * @description Define routes and make routing decisions between legacy and facade
 */

/**
 * @typedef {Object} Route
 * @property {Function} predicate - Function that returns true if route matches request
 * @property {'legacy'|'facade'} target - Target system for this route
 * @property {number} [weight=100] - Percentage (0-100) of traffic to send to target
 * @property {string} [name] - Human-readable route name
 * @property {number} [priority=0] - Priority (higher = evaluated first)
 */

/**
 * Define a route configuration
 *
 * @param {Function} predicate - Function (request) => boolean
 * @param {'legacy'|'facade'} target - Target system
 * @param {Object} [options] - Additional options
 * @param {number} [options.weight=100] - Traffic percentage (0-100)
 * @param {string} [options.name] - Route name
 * @param {number} [options.priority=0] - Evaluation priority
 * @returns {Route} Route configuration
 */
export function defineRoute(predicate, target, options = {}) {
  const { weight = 100, name, priority = 0 } = options;

  if (typeof predicate !== 'function') {
    throw new Error('Route predicate must be a function');
  }

  if (target !== 'legacy' && target !== 'facade') {
    throw new Error('Route target must be "legacy" or "facade"');
  }

  if (typeof weight !== 'number' || weight < 0 || weight > 100) {
    throw new Error('Route weight must be a number between 0 and 100');
  }

  return {
    predicate,
    target,
    weight,
    ...(name && { name }),
    priority
  };
}

/**
 * Make routing decision for a request
 * Evaluates routes in priority order, applies weight for canary rollout
 *
 * @param {Route[]} routes - Array of route configurations
 * @param {*} request - Request object to evaluate
 * @returns {'legacy'|'facade'} Target system
 */
export function routingDecision(routes, request) {
  if (!Array.isArray(routes) || routes.length === 0) {
    // Default to legacy if no routes defined
    return 'legacy';
  }

  // Sort routes by priority (descending)
  const sortedRoutes = [...routes].sort((a, b) => (b.priority || 0) - (a.priority || 0));

  // Evaluate predicates in order
  for (const route of sortedRoutes) {
    try {
      if (route.predicate(request)) {
        // Route matches, apply weight
        const weight = route.weight ?? 100;

        // If weight is 100, always use target
        if (weight >= 100) {
          return route.target;
        }

        // If weight is 0, skip to next route
        if (weight <= 0) {
          continue;
        }

        // Canary rollout: random sample based on weight
        // Use deterministic randomness based on request if available
        const random = getRequestRandom(request);

        if (random < weight) {
          return route.target;
        } else {
          // Weight didn't select this route, continue to next
          continue;
        }
      }
    } catch (err) {
      // If predicate throws, log and continue to next route
      console.error(`Route predicate error: ${err.message}`, route.name || 'unnamed');
      continue;
    }
  }

  // No route matched, default to legacy
  return 'legacy';
}

/**
 * Get deterministic random value for request (0-100)
 * Uses request ID or path for consistency (same request → same routing)
 *
 * @param {*} request - Request object
 * @returns {number} Random value 0-100
 */
function getRequestRandom(request) {
  // Try to extract deterministic seed from request
  let seed = 0;

  if (request?.id) {
    // Use request ID if available
    seed = hashString(String(request.id));
  } else if (request?.path) {
    // Use path as fallback
    seed = hashString(String(request.path));
  } else if (request?.url) {
    // Use URL as fallback
    seed = hashString(String(request.url));
  } else {
    // Truly random if no deterministic seed available
    return Math.random() * 100;
  }

  // Convert hash to 0-100 range
  return (seed % 100);
}

/**
 * Simple string hash function for deterministic randomness
 *
 * @param {string} str - String to hash
 * @returns {number} Hash value
 */
function hashString(str) {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    const char = str.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash; // Convert to 32-bit integer
  }
  return Math.abs(hash);
}

/**
 * Create a catch-all route (matches everything)
 *
 * @param {'legacy'|'facade'} target - Target system
 * @param {Object} [options] - Route options
 * @returns {Route} Catch-all route
 */
export function catchAllRoute(target, options = {}) {
  return defineRoute(() => true, target, {
    name: 'Catch-all',
    priority: -1000, // Lowest priority
    ...options
  });
}

/**
 * Create a path-based route
 *
 * @param {string|RegExp} pattern - Path pattern (string prefix or regex)
 * @param {'legacy'|'facade'} target - Target system
 * @param {Object} [options] - Route options
 * @returns {Route} Path-based route
 */
export function pathRoute(pattern, target, options = {}) {
  const predicate = typeof pattern === 'string'
    ? (req) => req?.path?.startsWith(pattern)
    : (req) => pattern.test(req?.path || '');

  return defineRoute(predicate, target, {
    name: `Path: ${pattern}`,
    ...options
  });
}

/**
 * Create a method-based route
 *
 * @param {string} method - HTTP method (GET, POST, etc.)
 * @param {'legacy'|'facade'} target - Target system
 * @param {Object} [options] - Route options
 * @returns {Route} Method-based route
 */
export function methodRoute(method, target, options = {}) {
  const upperMethod = method.toUpperCase();

  return defineRoute(
    (req) => req?.method?.toUpperCase() === upperMethod,
    target,
    {
      name: `Method: ${upperMethod}`,
      ...options
    }
  );
}

/**
 * Create a header-based route
 *
 * @param {string} headerName - Header name
 * @param {string|RegExp} headerValue - Header value (string or regex)
 * @param {'legacy'|'facade'} target - Target system
 * @param {Object} [options] - Route options
 * @returns {Route} Header-based route
 */
export function headerRoute(headerName, headerValue, target, options = {}) {
  const lowerHeaderName = headerName.toLowerCase();

  const predicate = typeof headerValue === 'string'
    ? (req) => req?.headers?.[lowerHeaderName] === headerValue
    : (req) => headerValue.test(req?.headers?.[lowerHeaderName] || '');

  return defineRoute(predicate, target, {
    name: `Header: ${headerName}`,
    ...options
  });
}

/**
 * Combine multiple routes with AND logic
 *
 * @param {Route[]} routes - Routes to combine
 * @param {'legacy'|'facade'} target - Target system
 * @param {Object} [options] - Route options
 * @returns {Route} Combined route
 */
export function andRoute(routes, target, options = {}) {
  return defineRoute(
    (req) => routes.every(route => route.predicate(req)),
    target,
    {
      name: 'AND combination',
      ...options
    }
  );
}

/**
 * Combine multiple routes with OR logic
 *
 * @param {Route[]} routes - Routes to combine
 * @param {'legacy'|'facade'} target - Target system
 * @param {Object} [options] - Route options
 * @returns {Route} Combined route
 */
export function orRoute(routes, target, options = {}) {
  return defineRoute(
    (req) => routes.some(route => route.predicate(req)),
    target,
    {
      name: 'OR combination',
      ...options
    }
  );
}
