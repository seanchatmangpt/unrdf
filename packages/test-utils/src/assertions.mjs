/**
 * @file Custom assertion helpers for RDF and store tests.
 *
 * Reduces repetitive try/catch blocks and transaction rollback verification.
 *
 * @module @unrdf/test-utils/assertions
 */

// ============================================================================
// Error assertions
// ============================================================================

/**
 * Assert that a synchronous function throws an error with a specific code.
 *
 * @param {Function} fn - Function expected to throw
 * @param {string} expectedCode - Expected `error.code` value
 * @param {Function} [ErrorClass=Error] - Expected error constructor
 * @returns {Error} The caught error (for further assertions)
 *
 * @example
 * const err = assertThrowsCode(() => store.query('bad sparql'), 'ERR_QUERY');
 * expect(err.message).toContain('syntax error');
 */
export function assertThrowsCode(fn, expectedCode, ErrorClass = Error) {
  let caught = null;
  try {
    fn();
  } catch (e) {
    caught = e;
  }
  if (!caught) {
    throw new Error(`Expected error with code "${expectedCode}" but nothing was thrown`);
  }
  if (!(caught instanceof ErrorClass)) {
    throw new Error(`Expected ${ErrorClass.name}, got ${caught.constructor.name}: ${caught.message}`);
  }
  if (caught.code !== undefined && caught.code !== expectedCode) {
    throw new Error(`Expected error.code "${expectedCode}", got "${caught.code}"`);
  }
  return caught;
}

/**
 * Assert that an async function throws an error (optionally checking message/code).
 *
 * @param {Function} asyncFn - Async function expected to reject
 * @param {string|RegExp} [match] - String or regex to match against error message
 * @returns {Promise<Error>} The caught error
 *
 * @example
 * const err = await assertRejects(() => fetchData('invalid-url'), /network/i);
 */
export async function assertRejects(asyncFn, match) {
  let caught = null;
  try {
    await asyncFn();
  } catch (e) {
    caught = e;
  }
  if (!caught) {
    throw new Error('Expected async function to reject, but it resolved');
  }
  if (match) {
    const msg = caught.message ?? String(caught);
    const ok = match instanceof RegExp ? match.test(msg) : msg.includes(match);
    if (!ok) {
      throw new Error(`Expected error matching ${match}, got: ${msg}`);
    }
  }
  return caught;
}

// ============================================================================
// RDF store assertions
// ============================================================================

/**
 * Assert a store contains exactly N quads.
 *
 * @param {object} store - RDF store
 * @param {number} expected - Expected quad count
 *
 * @example
 * assertStoreSize(store, 3);
 */
export function assertStoreSize(store, expected) {
  const actual = [...store.match()].length;
  if (actual !== expected) {
    throw new Error(`Expected store to contain ${expected} quads, but has ${actual}`);
  }
}

/**
 * Assert a store contains a specific quad (by subject+predicate+object IRIs/values).
 *
 * @param {object} store - RDF store
 * @param {object} quad - Quad to search for
 *
 * @example
 * assertStoreContains(store, testQuad);
 */
export function assertStoreContains(store, quad) {
  const matches = [...store.match(quad.subject, quad.predicate, quad.object, quad.graph)];
  if (matches.length === 0) {
    throw new Error(
      `Expected store to contain quad: <${quad.subject?.value}> <${quad.predicate?.value}> "${quad.object?.value}"`
    );
  }
}

/**
 * Assert that a transaction rolls back on error — store size is unchanged.
 *
 * @param {object} store - RDF store with .transaction() method
 * @param {Function} txFn - Transaction function that is expected to throw
 * @returns {Error} The error thrown by txFn
 *
 * @example
 * const err = assertRollback(store, txStore => {
 *   txStore.add(testQuad);
 *   throw new Error('deliberate rollback');
 * });
 * expect(err.message).toBe('deliberate rollback');
 */
export function assertRollback(store, txFn) {
  const before = [...store.match()].length;
  let caught = null;
  try {
    store.transaction(txFn);
  } catch (e) {
    caught = e;
  }
  if (!caught) {
    throw new Error('Expected transaction to throw, but it succeeded');
  }
  const after = [...store.match()].length;
  if (after !== before) {
    throw new Error(
      `Transaction rollback failed: store had ${before} quads before, ${after} after`
    );
  }
  return caught;
}

/**
 * Assert that SPARQL SELECT results contain a specific binding value.
 *
 * @param {object[]} results - SPARQL result rows
 * @param {string} variable - Variable name (without '?')
 * @param {string} expectedValue - Value to find
 *
 * @example
 * const results = store.query('SELECT * WHERE { ?s ?p ?o }');
 * assertBindingContains(results, 's', 'http://example.org/alice');
 */
export function assertBindingContains(results, variable, expectedValue) {
  const values = results.map(row => row[variable]?.value ?? row.get?.(variable)?.value);
  if (!values.includes(expectedValue)) {
    throw new Error(
      `Expected ?${variable} to contain "${expectedValue}", got: [${values.slice(0, 5).join(', ')}]`
    );
  }
}
