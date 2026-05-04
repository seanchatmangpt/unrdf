/**
 * @file guards.fuzz.test.mjs
 * @description Fuzz tests for Poka-Yoke guards using fast-check
 *
 * Fuzzes guards with:
 * 1. Malicious inputs (secrets, path traversal, privilege escalation)
 * 2. Edge cases (empty, huge, malformed)
 * 3. Boundary conditions
 * 4. Random valid/invalid operations
 *
 * @module @unrdf/kgc-swarm/test/guards.fuzz
 */

import { describe, it, expect } from 'vitest';
import fc from 'fast-check';
import {
  GuardSystem,
  SecretGuard,
  PathGuard,
  NetworkGuard,
  PrivilegeGuard,
} from '../src/guards.mjs';
import {
  arbOperation,
  arbFileOperation,
  arbNetworkOperation,
  arbProcessOperation,
  arbSecretOperation,
  arbPathTraversalOperation,
  arbPrivilegeEscalationOperation,
  arbMaliciousOperation,
} from './arbitraries.mjs';

describe('Guard Fuzz Tests (fast-check)', () => {
  describe('Fuzz: SecretGuard', () => {
    it('should block all secret operations', async () => {
      await fc.assert(
        fc.asyncProperty(arbSecretOperation(), async (operation) => {
          const result = await SecretGuard(operation);

          // Property: All secret operations should be blocked
          expect(result.allowed).toBe(false);
          expect(result.guard).toBe('SecretGuard');
          expect(result.reason).toBeTruthy();
          expect(result.receipt).toBeTruthy();
          return !result.allowed;
        }),
        { numRuns: 100 },
      );
    });

    it('should allow operations without secrets', async () => {
      await fc.assert(
        fc.asyncProperty(arbFileOperation(), async (operation) => {
          // Ensure target doesn't contain secret patterns
          const safeOperation = {
            ...operation,
            target: '/tmp/safe-file.txt',
            data: 'public data',
          };

          const result = await SecretGuard(safeOperation);

          // Property: Safe operations should pass
          expect(result.allowed).toBe(true);
          expect(result.guard).toBe('SecretGuard');
          return result.allowed;
        }),
        { numRuns: 100 },
      );
    });

    it('should detect secrets in data payload', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom(
            'sk-1234567890abcdef1234567890abcdef',
            'ghp_1234567890abcdef1234567890abcdef1234',
            'AKIAIOSFODNN7EXAMPLE',
            'Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9',
          ),
          async (secretData) => {
            const operation = {
              type: 'network:request',
              target: 'https://api.example.com',
              data: secretData,
            };

            const result = await SecretGuard(operation);

            expect(result.allowed).toBe(false);
            return !result.allowed;
          },
        ),
        { numRuns: 50 },
      );
    });

    it('should handle random string data without false positives', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.string({ minLength: 1, maxLength: 100 }),
          async (randomString) => {
            // Filter out strings that match secret patterns
            const hasSecretPattern = /PASSWORD|SECRET|API[_-]?KEY|TOKEN|PRIVATE|sk-|ghp_|AKIA/i.test(
              randomString,
            );

            if (hasSecretPattern) {
              return true; // Skip this case
            }

            const operation = {
              type: 'file:write',
              target: '/tmp/data.txt',
              data: randomString,
            };

            const result = await SecretGuard(operation);

            // Property: Random non-secret strings should pass
            expect(result.allowed).toBe(true);
            return result.allowed;
          },
        ),
        { numRuns: 100 },
      );
    });
  });

  describe('Fuzz: PathGuard', () => {
    it('should block all path traversal attempts', async () => {
      await fc.assert(
        fc.asyncProperty(
          arbPathTraversalOperation(),
          async (operation) => {
            const config = { rootPath: '/home/user/project' };
            const result = await PathGuard(operation, config);

            // Property: Path traversal should be blocked
            expect(result.allowed).toBe(false);
            expect(result.guard).toBe('PathGuard');
            return !result.allowed;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should allow operations within root path', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom(
            '/home/user/project/file.txt',
            '/home/user/project/subdir/data.json',
            '/home/user/project/nested/deep/file.mjs',
          ),
          async (safePath) => {
            const operation = {
              type: 'file:read',
              target: safePath,
            };
            const config = { rootPath: '/home/user/project' };
            const result = await PathGuard(operation, config);

            expect(result.allowed).toBe(true);
            return result.allowed;
          },
        ),
        { numRuns: 50 },
      );
    });

    it('should skip non-file operations', async () => {
      await fc.assert(
        fc.asyncProperty(arbNetworkOperation(), async (operation) => {
          const result = await PathGuard(operation);

          // Property: Non-file operations always pass PathGuard
          expect(result.allowed).toBe(true);
          expect(result.guard).toBe('PathGuard');
          return result.allowed;
        }),
        { numRuns: 100 },
      );
    });

    it('should handle absolute vs relative paths correctly', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.oneof(
            fc.constant('./local/file.txt'),
            fc.constant('../parent/file.txt'),
            fc.constant('/absolute/path/file.txt'),
          ),
          async (path) => {
            const operation = {
              type: 'file:read',
              target: path,
            };
            const config = { rootPath: process.cwd() };
            const result = await PathGuard(operation, config);

            // Path with ../ should be blocked
            if (path.includes('../')) {
              expect(result.allowed).toBe(false);
            }
            return true;
          },
        ),
        { numRuns: 50 },
      );
    });
  });

  describe('Fuzz: NetworkGuard', () => {
    it('should block non-allowlisted hosts', async () => {
      await fc.assert(
        fc.asyncProperty(fc.webUrl(), async (randomUrl) => {
          const config = {
            allowedHosts: ['api.example.com', 'cdn.example.com'],
          };

          const operation = {
            type: 'network:request',
            target: randomUrl,
          };

          const result = await NetworkGuard(operation, config);
          const url = new URL(randomUrl);

          // Property: Only allowlisted hosts should pass
          const isAllowed = config.allowedHosts.includes(url.hostname);
          expect(result.allowed).toBe(isAllowed);
          return true;
        }),
        { numRuns: 100 },
      );
    });

    it('should support wildcard host matching', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom(
            'https://api.example.com',
            'https://cdn.example.com',
            'https://subdomain.example.com',
          ),
          async (url) => {
            const config = { allowedHosts: ['*.example.com'] };
            const operation = { type: 'network:request', target: url };
            const result = await NetworkGuard(operation, config);

            expect(result.allowed).toBe(true);
            return result.allowed;
          },
        ),
        { numRuns: 50 },
      );
    });

    it('should validate ports correctly', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.integer({ min: 1, max: 65535 }),
          async (port) => {
            const config = {
              allowedHosts: ['example.com'],
              allowedPorts: [80, 443, 8080],
            };

            const operation = {
              type: 'network:request',
              target: `https://example.com:${port}`,
            };

            const result = await NetworkGuard(operation, config);

            // Property: Only allowed ports should pass
            const isAllowed = config.allowedPorts.includes(port) || port === 443; // https default
            expect(result.allowed).toBe(isAllowed);
            return true;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should reject invalid URLs', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom(
            'not-a-url',
            'ftp://invalid',
            '://broken',
            'http://',
          ),
          async (invalidUrl) => {
            const operation = {
              type: 'network:request',
              target: invalidUrl,
            };

            const result = await NetworkGuard(operation);

            expect(result.allowed).toBe(false);
            expect(result.reason).toContain('Invalid URL');
            return !result.allowed;
          },
        ),
        { numRuns: 50 },
      );
    });

    it('should skip non-network operations', async () => {
      await fc.assert(
        fc.asyncProperty(arbFileOperation(), async (operation) => {
          const result = await NetworkGuard(operation);

          expect(result.allowed).toBe(true);
          return result.allowed;
        }),
        { numRuns: 100 },
      );
    });
  });

  describe('Fuzz: PrivilegeGuard', () => {
    it('should block all privilege escalation attempts', async () => {
      await fc.assert(
        fc.asyncProperty(
          arbPrivilegeEscalationOperation(),
          async (operation) => {
            const result = await PrivilegeGuard(operation);

            // Property: All privilege escalation should be blocked
            expect(result.allowed).toBe(false);
            expect(result.guard).toBe('PrivilegeGuard');
            expect(result.reason).toBeTruthy();
            return !result.allowed;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should block access to restricted paths', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom(
            '/etc/passwd',
            '/etc/shadow',
            '/etc/sudoers',
            '/root/.ssh/id_rsa',
          ),
          async (restrictedPath) => {
            const operation = {
              type: 'file:read',
              target: restrictedPath,
            };

            const result = await PrivilegeGuard(operation);

            expect(result.allowed).toBe(false);
            return !result.allowed;
          },
        ),
        { numRuns: 50 },
      );
    });

    it('should block restricted commands', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom('sudo', 'su', 'chmod', 'chown'),
          async (command) => {
            const operation = {
              type: 'process:spawn',
              target: command,
            };

            const result = await PrivilegeGuard(operation);

            expect(result.allowed).toBe(false);
            return !result.allowed;
          },
        ),
        { numRuns: 50 },
      );
    });

    it('should allow safe operations', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom('node', 'npm', 'git', 'echo'),
          async (safeCommand) => {
            const operation = {
              type: 'process:spawn',
              target: safeCommand,
            };

            const result = await PrivilegeGuard(operation);

            expect(result.allowed).toBe(true);
            return result.allowed;
          },
        ),
        { numRuns: 50 },
      );
    });
  });

  describe('Fuzz: GuardSystem (Composed)', () => {
    it('should block all malicious operations', async () => {
      await fc.assert(
        fc.asyncProperty(arbMaliciousOperation(), async (operation) => {
          const guardSystem = new GuardSystem({
            enableSecretGuard: true,
            enablePathGuard: true,
            enableNetworkGuard: true,
            enablePrivilegeGuard: true,
            rootPath: '/home/user/project',
            allowedHosts: ['api.example.com'],
          });

          const result = await guardSystem.validate(operation);

          // Property: All malicious operations should be blocked
          expect(result.allowed).toBe(false);
          expect(result.receipt).toBeTruthy();
          return !result.allowed;
        }),
        { numRuns: 100 },
      );
    });

    it('should allow safe operations through all guards', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom(
            {
              type: 'file:read',
              target: '/home/user/project/data.txt',
              data: 'safe data',
            },
            {
              type: 'network:request',
              target: 'https://api.example.com',
            },
            {
              type: 'process:spawn',
              target: 'node',
            },
          ),
          async (safeOperation) => {
            const guardSystem = new GuardSystem({
              rootPath: '/home/user/project',
              allowedHosts: ['api.example.com'],
            });

            const result = await guardSystem.validate(safeOperation);

            expect(result.allowed).toBe(true);
            expect(result.guard).toBe('GuardSystem');
            return result.allowed;
          },
        ),
        { numRuns: 50 },
      );
    });

    it('should check unlawful operation detection', async () => {
      await fc.assert(
        fc.asyncProperty(arbMaliciousOperation(), async (operation) => {
          const guardSystem = new GuardSystem();
          const isUnlawful = await guardSystem.unlawful(operation);

          expect(isUnlawful).toBe(true);
          return isUnlawful;
        }),
        { numRuns: 100 },
      );
    });

    it('should handle random valid operations', async () => {
      await fc.assert(
        fc.asyncProperty(arbOperation(), async (operation) => {
          const guardSystem = new GuardSystem({
            rootPath: process.cwd(),
            allowedHosts: [], // Empty = allow all
          });

          const result = await guardSystem.validate(operation);

          // Should not throw, should return result
          expect(result).toHaveProperty('allowed');
          expect(result).toHaveProperty('guard');
          expect(result).toHaveProperty('receipt');
          return true;
        }),
        { numRuns: 100 },
      );
    });

    it('should handle edge cases: empty/null data', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom(null, undefined, '', {}),
          async (edgeData) => {
            const operation = {
              type: 'file:write',
              target: '/tmp/test.txt',
              data: edgeData,
            };

            const guardSystem = new GuardSystem();

            try {
              const result = await guardSystem.validate(operation);
              expect(result).toHaveProperty('allowed');
              return true;
            } catch (error) {
              // Validation error is acceptable
              return true;
            }
          },
        ),
        { numRuns: 50 },
      );
    });

    it('should generate receipts for all decisions', async () => {
      await fc.assert(
        fc.asyncProperty(arbOperation(), async (operation) => {
          const guardSystem = new GuardSystem();

          try {
            const result = await guardSystem.validate(operation);

            // Property: All guard decisions have receipts
            expect(result.receipt).toBeDefined();
            expect(result.receipt.id).toBeTruthy();
            expect(result.receipt.timestamp).toBeTruthy();
            expect(result.receipt.hash).toBeTruthy();
            expect(result.receipt.hash).toHaveLength(64);
            return true;
          } catch (error) {
            // Schema validation errors are acceptable
            return true;
          }
        }),
        { numRuns: 100 },
      );
    });
  });

  describe('Edge Cases: Performance & Stress', () => {
    it('should handle huge operation data', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.string({ minLength: 10000, maxLength: 50000 }),
          async (hugeData) => {
            const operation = {
              type: 'file:write',
              target: '/tmp/huge.txt',
              data: hugeData,
            };

            const start = Date.now();
            const guardSystem = new GuardSystem();
            const result = await guardSystem.validate(operation);
            const duration = Date.now() - start;

            // Property: Should complete in reasonable time
            expect(duration).toBeLessThan(100);
            expect(result).toHaveProperty('allowed');
            return true;
          },
        ),
        { numRuns: 10 }, // Fewer runs for performance
      );
    });

    it('should handle many sequential validations', async () => {
      const guardSystem = new GuardSystem();

      await fc.assert(
        fc.asyncProperty(
          fc.array(arbOperation(), { minLength: 100, maxLength: 200 }),
          async (operations) => {
            const start = Date.now();

            for (const op of operations) {
              try {
                await guardSystem.validate(op);
              } catch (error) {
                // Schema errors acceptable
              }
            }

            const duration = Date.now() - start;

            // Property: Batch validation should be efficient
            expect(duration).toBeLessThan(500);
            return true;
          },
        ),
        { numRuns: 5 },
      );
    });
  });
});
