/**
 * @file arbitraries.mjs
 * @description Fast-check arbitraries for generative testing
 *
 * Provides random generators for:
 * - Observables
 * - Receipts
 * - Receipt chains
 * - Operations (for guard testing)
 * - Contexts
 *
 * @module @unrdf/kgc-swarm/test/arbitraries
 */

import fc from 'fast-check';

// ============================================================================
// Hash Arbitraries
// ============================================================================

/**
 * Generate a valid SHA-256 hash (64 hex characters)
 */
export const arbHash = () =>
  fc.hexaString({ minLength: 64, maxLength: 64 });

/**
 * Generate a possibly invalid hash (for fuzzing)
 */
export const arbInvalidHash = () =>
  fc.oneof(
    fc.constant(''),
    fc.constant('invalid'),
    fc.hexaString({ minLength: 1, maxLength: 63 }),
    fc.hexaString({ minLength: 65, maxLength: 100 }),
    fc.string({ minLength: 64, maxLength: 64 }), // Non-hex characters
  );

// ============================================================================
// Observable Arbitraries
// ============================================================================

/**
 * Generate a valid observable
 */
export const arbObservable = () =>
  fc.record({
    id: fc.uuid(),
    timestamp: fc.integer({ min: 1000000000000, max: 9999999999999 }), // Unix ms
    data: fc.oneof(
      fc.object(),
      fc.record({
        x: fc.integer(),
        y: fc.integer(),
        z: fc.string(),
      }),
      fc.record({
        value: fc.double(),
        unit: fc.constantFrom('m', 'kg', 's', 'A', 'K'),
      }),
    ),
    metadata: fc.option(
      fc.record({
        source: fc.string(),
        version: fc.constantFrom('1.0', '2.0', '3.0'),
        tags: fc.array(fc.string()),
      }),
      { nil: undefined },
    ),
  });

/**
 * Generate array of observables with configurable size
 */
export const arbObservables = ({ minLength = 0, maxLength = 10 } = {}) =>
  fc.array(arbObservable(), { minLength, maxLength });

/**
 * Generate observables with duplicate detection
 */
export const arbObservablesWithDuplicates = () =>
  fc.array(arbObservable(), { minLength: 5, maxLength: 20 }).chain(obs => {
    // Randomly duplicate some observables
    return fc.record({
      original: fc.constant(obs),
      duplicates: fc.array(fc.integer({ min: 0, max: obs.length - 1 })),
    }).map(({ original, duplicates }) => {
      const result = [...original];
      duplicates.forEach(idx => {
        if (original[idx]) {
          result.push(original[idx]);
        }
      });
      return result;
    });
  });

// ============================================================================
// Receipt Arbitraries
// ============================================================================

/**
 * Generate a valid receipt
 */
export const arbReceipt = () =>
  fc.record({
    before: arbHash(),
    after: arbHash(),
    delta: arbHash(),
    timestamp: fc.integer({ min: 1000000000000, max: 9999999999999 }),
    id: fc.option(fc.uuid(), { nil: undefined }),
  });

/**
 * Generate a receipt with invalid data (for fuzzing)
 */
export const arbInvalidReceipt = () =>
  fc.oneof(
    // Invalid hash lengths
    fc.record({
      before: arbInvalidHash(),
      after: arbHash(),
      delta: arbHash(),
      timestamp: fc.integer({ min: 0, max: Date.now() }),
    }),
    // Negative timestamp
    fc.record({
      before: arbHash(),
      after: arbHash(),
      delta: arbHash(),
      timestamp: fc.integer({ min: -1000000, max: -1 }),
    }),
    // Non-integer timestamp
    fc.record({
      before: arbHash(),
      after: arbHash(),
      delta: arbHash(),
      timestamp: fc.double(),
    }),
    // Missing fields
    fc.record({
      before: arbHash(),
      after: arbHash(),
      // delta missing
      timestamp: fc.integer({ min: 0, max: Date.now() }),
    }),
  );

/**
 * Generate a valid receipt chain (linked receipts)
 */
export const arbReceiptChain = ({ minLength = 1, maxLength = 10 } = {}) =>
  fc
    .integer({ min: minLength, max: maxLength })
    .chain(length => {
      if (length === 0) {
        return fc.constant([]);
      }

      // Generate first receipt
      return arbHash().chain(initialHash => {
        const receipts = [];
        let currentBefore = initialHash;

        return fc.array(
          fc.tuple(arbHash(), arbHash()).map(([afterHash, deltaHash]) => {
            const receipt = {
              before: currentBefore,
              after: afterHash,
              delta: deltaHash,
              timestamp: Date.now() + receipts.length * 1000,
            };
            currentBefore = afterHash; // Chain linking
            return receipt;
          }),
          { minLength: length, maxLength: length },
        );
      });
    });

/**
 * Generate a receipt chain with broken integrity (for fuzzing)
 */
export const arbBrokenReceiptChain = () =>
  arbReceiptChain({ minLength: 3, maxLength: 10 }).chain(chain => {
    // Pick a random index to break the chain
    return fc.integer({ min: 1, max: chain.length - 1 }).map(breakIndex => {
      const broken = [...chain];
      broken[breakIndex] = {
        ...broken[breakIndex],
        before: 'broken' + broken[breakIndex].before.slice(6), // Corrupt the link
      };
      return broken;
    });
  });

// ============================================================================
// Operation Arbitraries (for Guard Testing)
// ============================================================================

/**
 * Generate a valid file operation
 */
export const arbFileOperation = () =>
  fc.record({
    type: fc.constantFrom('file:read', 'file:write', 'file:delete'),
    target: fc.oneof(
      fc.constant('/tmp/test.txt'),
      fc.constant('/home/user/data.json'),
      fc.constant('./local/file.mjs'),
    ),
    data: fc.option(fc.string(), { nil: undefined }),
    metadata: fc.option(fc.object(), { nil: undefined }),
  });

/**
 * Generate a network operation
 */
export const arbNetworkOperation = () =>
  fc.record({
    type: fc.constant('network:request'),
    target: fc.webUrl(),
    data: fc.option(fc.object(), { nil: undefined }),
    metadata: fc.option(fc.object(), { nil: undefined }),
  });

/**
 * Generate a process operation
 */
export const arbProcessOperation = () =>
  fc.record({
    type: fc.constant('process:spawn'),
    target: fc.constantFrom('node', 'npm', 'git', 'echo'),
    data: fc.option(
      fc.record({
        args: fc.array(fc.string()),
        cwd: fc.option(fc.string(), { nil: undefined }),
      }),
      { nil: undefined },
    ),
    metadata: fc.option(fc.object(), { nil: undefined }),
  });

/**
 * Generate any valid operation
 */
export const arbOperation = () =>
  fc.oneof(
    arbFileOperation(),
    arbNetworkOperation(),
    arbProcessOperation(),
    fc.record({
      type: fc.constantFrom('env:access', 'secret:access'),
      target: fc.string(),
      data: fc.option(fc.anything(), { nil: undefined }),
      metadata: fc.option(fc.object(), { nil: undefined }),
    }),
  );

/**
 * Generate operations with potential secrets
 */
export const arbSecretOperation = () =>
  fc.oneof(
    // File operations with secret-like names
    fc.record({
      type: fc.constantFrom('file:read', 'file:write'),
      target: fc.constantFrom(
        '/etc/passwd',
        './config/secrets.json',
        '.env',
        './api_key.txt',
        './private_key.pem',
      ),
      data: fc.option(fc.string(), { nil: undefined }),
    }),
    // Operations with secret data
    fc.record({
      type: fc.constantFrom('network:request', 'env:access'),
      target: fc.string(),
      data: fc.constantFrom(
        'sk-1234567890abcdef1234567890abcdef',
        'ghp_1234567890abcdef1234567890abcdef1234',
        'Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...',
        'AKIA1234567890ABCDEF',
      ),
    }),
  );

/**
 * Generate operations with path traversal attempts
 */
export const arbPathTraversalOperation = () =>
  fc.record({
    type: fc.constantFrom('file:read', 'file:write', 'file:delete'),
    target: fc.constantFrom(
      '../../../etc/passwd',
      '../../secrets.json',
      './data/../../../root/.ssh/id_rsa',
      '/tmp/../../etc/shadow',
    ),
    data: fc.option(fc.string(), { nil: undefined }),
  });

/**
 * Generate privilege escalation operations
 */
export const arbPrivilegeEscalationOperation = () =>
  fc.oneof(
    fc.record({
      type: fc.constant('process:spawn'),
      target: fc.constantFrom('sudo', 'su', 'chmod', 'chown'),
      data: fc.option(
        fc.record({
          args: fc.array(fc.string()),
        }),
        { nil: undefined },
      ),
    }),
    fc.record({
      type: fc.constantFrom('file:read', 'file:write'),
      target: fc.constantFrom(
        '/etc/sudoers',
        '/etc/shadow',
        '/root/.ssh/authorized_keys',
      ),
      data: fc.option(fc.string(), { nil: undefined }),
    }),
  );

/**
 * Generate malicious operations (combined)
 */
export const arbMaliciousOperation = () =>
  fc.oneof(
    arbSecretOperation(),
    arbPathTraversalOperation(),
    arbPrivilegeEscalationOperation(),
  );

// ============================================================================
// Edge Case Arbitraries
// ============================================================================

/**
 * Generate empty/minimal data
 */
export const arbEmpty = () =>
  fc.oneof(
    fc.constant([]),
    fc.constant({}),
    fc.constant(''),
    fc.constant(null),
    fc.constant(undefined),
  );

/**
 * Generate huge data structures
 */
export const arbHuge = () =>
  fc.oneof(
    fc.array(arbObservable(), { minLength: 1000, maxLength: 5000 }),
    fc.string({ minLength: 10000, maxLength: 100000 }),
    fc.object({ maxDepth: 10 }),
  );

/**
 * Generate malformed observables
 */
export const arbMalformedObservable = () =>
  fc.oneof(
    fc.constant({ id: 123 }), // Wrong type
    fc.constant({ timestamp: 'not-a-number' }),
    fc.constant({ data: null }),
    fc.constant({}), // Missing fields
    fc.constant(null),
    fc.constant('not-an-object'),
  );

// ============================================================================
// Archive Arbitraries (for Compression Testing)
// ============================================================================

/**
 * Generate a valid archive structure
 */
export const arbArchive = () =>
  arbObservables({ minLength: 1, maxLength: 10 }).chain(observables => {
    // Compute cover (unique keys)
    const keys = new Set();
    observables.forEach(obs => {
      Object.keys(obs.data || {}).forEach(k => keys.add(k));
    });
    const cover = Array.from(keys).sort();

    return fc.record({
      observables: fc.constant(observables),
      cover: fc.constant(cover),
      glue: fc.object(),
      compressed: fc.constant(true),
      hash: arbHash(),
    });
  });

/**
 * Generate a corrupted archive (for fuzzing)
 */
export const arbCorruptedArchive = () =>
  arbArchive().chain(archive =>
    fc.oneof(
      // Corrupt hash
      fc.record({
        ...archive,
        hash: arbInvalidHash(),
      }),
      // Corrupt compressed flag
      fc.constant({
        ...archive,
        compressed: false,
      }),
      // Corrupt observables
      fc.constant({
        ...archive,
        observables: [{ invalid: 'data' }],
      }),
    ),
  );

// ============================================================================
// Exports
// ============================================================================

export default {
  arbHash,
  arbInvalidHash,
  arbObservable,
  arbObservables,
  arbObservablesWithDuplicates,
  arbReceipt,
  arbInvalidReceipt,
  arbReceiptChain,
  arbBrokenReceiptChain,
  arbOperation,
  arbFileOperation,
  arbNetworkOperation,
  arbProcessOperation,
  arbSecretOperation,
  arbPathTraversalOperation,
  arbPrivilegeEscalationOperation,
  arbMaliciousOperation,
  arbEmpty,
  arbHuge,
  arbMalformedObservable,
  arbArchive,
  arbCorruptedArchive,
};
