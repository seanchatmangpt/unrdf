/**
 * E2E Scenario Suite - Deterministic test scenarios for shadow verification
 * @module agent-10/scenario-suite
 */

/**
 * @typedef {Object} Scenario
 * @property {string} name - Scenario name
 * @property {string} description - Scenario description
 * @property {Function} legacyHandler - Legacy system handler
 * @property {Function} migratedHandler - Migrated system handler
 * @property {*} input - Scenario input data
 * @property {Function} assertions - Assertion function (result) => void
 * @property {Object} [options] - Optional configuration
 * @property {number} [options.tolerance] - Tolerance for numeric comparisons (default: 0)
 * @property {boolean} [options.allowTimestampDrift] - Allow timestamp differences (default: false)
 */

/**
 * Legacy user retrieval handler (mock)
 * @param {Object} input - Input data
 * @returns {Object} User object
 */
function legacyGetUser(input) {
  if (!input.userId) {
    throw new Error('userId is required');
  }

  return {
    id: input.userId,
    name: 'John Doe',
    email: 'john@example.com',
    status: 'active',
    createdAt: 1703587200000,
  };
}

/**
 * Migrated user retrieval handler (mock)
 * @param {Object} input - Input data
 * @returns {Object} User object
 */
function migratedGetUser(input) {
  if (!input.userId) {
    throw new Error('userId is required');
  }

  return {
    id: input.userId,
    name: 'John Doe',
    email: 'john@example.com',
    status: 'active',
    createdAt: 1703587200000,
  };
}

/**
 * Legacy order creation handler (mock)
 * @param {Object} input - Input data
 * @returns {Object} Order object
 */
function legacyCreateOrder(input) {
  if (!input.items || input.items.length === 0) {
    throw new Error('items are required');
  }

  const total = input.items.reduce((sum, item) => sum + item.price * item.quantity, 0);

  return {
    orderId: 'ORD-12345',
    items: input.items,
    total,
    tax: total * 0.1,
    grandTotal: total * 1.1,
    status: 'pending',
  };
}

/**
 * Migrated order creation handler (mock)
 * @param {Object} input - Input data
 * @returns {Object} Order object
 */
function migratedCreateOrder(input) {
  if (!input.items || input.items.length === 0) {
    throw new Error('items are required');
  }

  const total = input.items.reduce((sum, item) => sum + item.price * item.quantity, 0);

  return {
    orderId: 'ORD-12345',
    items: input.items,
    total,
    tax: total * 0.1,
    grandTotal: total * 1.1,
    status: 'pending',
  };
}

/**
 * Legacy list filtering handler (mock)
 * @param {Object} input - Input data
 * @returns {Object} Filtered list
 */
function legacyListUsers(input) {
  const allUsers = [
    { id: 1, name: 'Alice', role: 'admin' },
    { id: 2, name: 'Bob', role: 'user' },
    { id: 3, name: 'Charlie', role: 'admin' },
  ];

  let filtered = allUsers;

  if (input.role) {
    filtered = filtered.filter(u => u.role === input.role);
  }

  return {
    users: filtered,
    count: filtered.length,
    total: allUsers.length,
  };
}

/**
 * Migrated list filtering handler (mock)
 * @param {Object} input - Input data
 * @returns {Object} Filtered list
 */
function migratedListUsers(input) {
  const allUsers = [
    { id: 1, name: 'Alice', role: 'admin' },
    { id: 2, name: 'Bob', role: 'user' },
    { id: 3, name: 'Charlie', role: 'admin' },
  ];

  let filtered = allUsers;

  if (input.role) {
    filtered = filtered.filter(u => u.role === input.role);
  }

  return {
    users: filtered,
    count: filtered.length,
    total: allUsers.length,
  };
}

/**
 * Legacy batch update handler (mock)
 * @param {Object} input - Input data
 * @returns {Object} Update result
 */
function legacyBatchUpdate(input) {
  if (!input.updates || input.updates.length === 0) {
    throw new Error('updates are required');
  }

  const results = input.updates.map(update => ({
    id: update.id,
    status: 'updated',
    changes: update.changes,
  }));

  return {
    updated: results.length,
    failed: 0,
    results,
  };
}

/**
 * Migrated batch update handler (mock)
 * @param {Object} input - Input data
 * @returns {Object} Update result
 */
function migratedBatchUpdate(input) {
  if (!input.updates || input.updates.length === 0) {
    throw new Error('updates are required');
  }

  const results = input.updates.map(update => ({
    id: update.id,
    status: 'updated',
    changes: update.changes,
  }));

  return {
    updated: results.length,
    failed: 0,
    results,
  };
}

/**
 * Legacy error handling scenario (mock)
 * @param {Object} input - Input data
 * @returns {Object} Result
 */
function legacyErrorCase(input) {
  if (input.shouldFail) {
    throw new Error('Intentional failure for testing');
  }

  return { success: true };
}

/**
 * Migrated error handling scenario (mock)
 * @param {Object} input - Input data
 * @returns {Object} Result
 */
function migratedErrorCase(input) {
  if (input.shouldFail) {
    throw new Error('Intentional failure for testing');
  }

  return { success: true };
}

/**
 * Define E2E scenarios
 * Each scenario must be deterministic and produce identical outputs
 *
 * @returns {Scenario[]} Array of test scenarios
 */
export function defineScenarios() {
  return [
    {
      name: 'GET_USER',
      description: 'Retrieve user by ID',
      legacyHandler: legacyGetUser,
      migratedHandler: migratedGetUser,
      input: { userId: 'user-123' },
      assertions: (result) => {
        if (!result.id) throw new Error('Missing user id');
        if (!result.name) throw new Error('Missing user name');
        if (!result.email) throw new Error('Missing user email');
        if (result.status !== 'active') throw new Error('Unexpected user status');
      },
    },

    {
      name: 'CREATE_ORDER',
      description: 'Create order with items',
      legacyHandler: legacyCreateOrder,
      migratedHandler: migratedCreateOrder,
      input: {
        items: [
          { id: 'item-1', name: 'Widget', price: 10.0, quantity: 2 },
          { id: 'item-2', name: 'Gadget', price: 15.0, quantity: 1 },
        ],
      },
      assertions: (result) => {
        if (!result.orderId) throw new Error('Missing order id');
        if (result.total !== 35.0) throw new Error(`Expected total 35.0, got ${result.total}`);
        if (result.tax !== 3.5) throw new Error(`Expected tax 3.5, got ${result.tax}`);
        if (result.grandTotal !== 38.5) throw new Error(`Expected grandTotal 38.5, got ${result.grandTotal}`);
      },
      options: {
        tolerance: 0.01, // Allow small floating-point differences
      },
    },

    {
      name: 'LIST_USERS_FILTERED',
      description: 'List users with role filter',
      legacyHandler: legacyListUsers,
      migratedHandler: migratedListUsers,
      input: { role: 'admin' },
      assertions: (result) => {
        if (!result.users) throw new Error('Missing users array');
        if (result.count !== 2) throw new Error(`Expected count 2, got ${result.count}`);
        if (result.users.length !== 2) throw new Error('Incorrect filtered count');
        const allAdmins = result.users.every(u => u.role === 'admin');
        if (!allAdmins) throw new Error('Not all users are admins');
      },
    },

    {
      name: 'BATCH_UPDATE',
      description: 'Batch update multiple records',
      legacyHandler: legacyBatchUpdate,
      migratedHandler: migratedBatchUpdate,
      input: {
        updates: [
          { id: 1, changes: { status: 'active' } },
          { id: 2, changes: { status: 'inactive' } },
          { id: 3, changes: { status: 'pending' } },
        ],
      },
      assertions: (result) => {
        if (result.updated !== 3) throw new Error(`Expected 3 updates, got ${result.updated}`);
        if (result.failed !== 0) throw new Error(`Expected 0 failures, got ${result.failed}`);
        if (result.results.length !== 3) throw new Error('Incorrect results count');
      },
    },

    {
      name: 'ERROR_HANDLING',
      description: 'Test error cases for equivalence',
      legacyHandler: legacyErrorCase,
      migratedHandler: migratedErrorCase,
      input: { shouldFail: false },
      assertions: (result) => {
        if (!result.success) throw new Error('Expected success: true');
      },
    },

    {
      name: 'ERROR_HANDLING_FAILURE',
      description: 'Test error handling when both should fail',
      legacyHandler: legacyErrorCase,
      migratedHandler: migratedErrorCase,
      input: { shouldFail: true },
      assertions: (result) => {
        // Both should throw, so this will be handled specially in runner
        throw new Error('Should not reach assertions for error case');
      },
    },
  ];
}

/**
 * Get scenario by name
 * @param {string} name - Scenario name
 * @returns {Scenario|null} Scenario or null if not found
 */
export function getScenario(name) {
  const scenarios = defineScenarios();
  return scenarios.find(s => s.name === name) || null;
}

/**
 * Validate scenario structure
 * @param {Scenario} scenario - Scenario to validate
 * @returns {boolean} True if valid
 * @throws {Error} If scenario is invalid
 */
export function validateScenario(scenario) {
  if (!scenario || typeof scenario !== 'object') {
    throw new Error('Scenario must be an object');
  }

  if (!scenario.name || typeof scenario.name !== 'string') {
    throw new Error('Scenario must have a string name');
  }

  if (!scenario.legacyHandler || typeof scenario.legacyHandler !== 'function') {
    throw new Error('Scenario must have a legacyHandler function');
  }

  if (!scenario.migratedHandler || typeof scenario.migratedHandler !== 'function') {
    throw new Error('Scenario must have a migratedHandler function');
  }

  if (!scenario.assertions || typeof scenario.assertions !== 'function') {
    throw new Error('Scenario must have an assertions function');
  }

  return true;
}
