/**
 * Comprehensive Hook Types Test Suite
 *
 * Tests all 33 hook trigger types with nontrivial scenarios:
 * - Core CRUD (6)
 * - Transaction (4)
 * - Error/Event (5)
 * - Async/IO (6)
 * - Cron/Time (4)
 * - Quality/Lean Six Sigma (8)
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  defineHook,
  executeHook,
  executeHookChain,
  executeHooksByTrigger,
  registerHook,
  unregisterHook,
  getHooksByTrigger,
  clearHooks,
  createHookRegistry,
} from '../src/index.mjs';

describe('Comprehensive Hook Types - Core CRUD (6 types)', () => {
  let registry;
  let testQuad;

  beforeEach(() => {
    registry = createHookRegistry();
    clearHooks(registry);
    testQuad = {
      subject: { value: 'http://example.org/subject' },
      predicate: { value: 'http://example.org/predicate' },
      object: { value: 'test value', language: 'en' },
      graph: { value: 'http://example.org/graph' },
    };
  });

  it('before-add: validates data before insertion', async () => {
    const validationLog = [];

    const hook = defineHook({
      name: 'validate-before-add',
      trigger: 'before-add',
      validate: quad => {
        validationLog.push(`Validating: ${quad.subject.value}`);
        // Reject quads without proper IRI format
        return quad.subject.value.startsWith('http://');
      },
    });

    // Valid quad should pass
    const result1 = await executeHook(hook, testQuad);
    expect(result1.valid).toBe(true);
    expect(validationLog).toContain('Validating: http://example.org/subject');

    // Invalid quad should fail
    const invalidQuad = { ...testQuad, subject: { value: 'invalid-iri' } };
    const result2 = await executeHook(hook, invalidQuad);
    expect(result2.valid).toBe(false);
  });

  it('after-add: audits successful insertions', async () => {
    const auditLog = [];

    const hook = defineHook({
      name: 'audit-after-add',
      trigger: 'after-add',
      transform: quad => {
        auditLog.push({
          timestamp: Date.now(),
          subject: quad.subject.value,
          action: 'added',
        });
        return quad;
      },
    });

    await executeHook(hook, testQuad);

    expect(auditLog).toHaveLength(1);
    expect(auditLog[0].action).toBe('added');
    expect(auditLog[0].subject).toBe('http://example.org/subject');
  });

  it('before-query: transforms query patterns', async () => {
    const hook = defineHook({
      name: 'expand-query-pattern',
      trigger: 'before-query',
      transform: quad => {
        // Expand wildcard predicates
        if (quad.predicate.value === '*') {
          return {
            ...quad,
            predicate: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
          };
        }
        return quad;
      },
    });

    const queryQuad = {
      subject: { value: 'http://example.org/entity' },
      predicate: { value: '*' },
      object: { value: null },
    };

    const result = await executeHook(hook, queryQuad);
    expect(result.quad.predicate.value).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
  });

  it('after-query: filters query results', async () => {
    const hook = defineHook({
      name: 'filter-sensitive-data',
      trigger: 'after-query',
      validate: quad => {
        // Filter out sensitive predicates
        return !quad.predicate.value.includes('password');
      },
    });

    const sensitiveQuad = {
      ...testQuad,
      predicate: { value: 'http://example.org/password' },
    };

    const result = await executeHook(hook, sensitiveQuad);
    expect(result.valid).toBe(false);
  });

  it('before-remove: prevents deletion of protected data', async () => {
    const hook = defineHook({
      name: 'protect-system-data',
      trigger: 'before-remove',
      validate: quad => {
        // Prevent deletion of system namespace data
        return !quad.subject.value.startsWith('http://system.internal/');
      },
    });

    const systemQuad = {
      subject: { value: 'http://system.internal/config' },
      predicate: { value: 'http://example.org/setting' },
      object: { value: 'important' },
    };

    const result = await executeHook(hook, systemQuad);
    expect(result.valid).toBe(false);

    const userQuad = {
      subject: { value: 'http://example.org/user/123' },
      predicate: { value: 'http://example.org/name' },
      object: { value: 'John' },
    };

    const result2 = await executeHook(hook, userQuad);
    expect(result2.valid).toBe(true);
  });

  it('after-remove: cascades deletions', async () => {
    const cascadeLog = [];

    const hook = defineHook({
      name: 'cascade-delete',
      trigger: 'after-remove',
      transform: quad => {
        // Log entities that need cascade deletion
        cascadeLog.push({
          entity: quad.subject.value,
          needsCascade: true,
        });
        return quad;
      },
    });

    await executeHook(hook, testQuad);

    expect(cascadeLog).toHaveLength(1);
    expect(cascadeLog[0].needsCascade).toBe(true);
  });
});

describe('Comprehensive Hook Types - Transaction (4 types)', () => {
  let registry;

  beforeEach(() => {
    registry = createHookRegistry();
    clearHooks(registry);
  });

  it('before-commit: validates transaction consistency', async () => {
    const hook = defineHook({
      name: 'validate-transaction',
      trigger: 'before-commit',
      validate: quad => {
        // Ensure transaction is not empty
        return quad !== null && quad.subject !== undefined;
      },
    });

    const transactionQuad = {
      subject: { value: 'transaction://12345' },
      predicate: { value: 'http://example.org/operation' },
      object: { value: 'batch-insert' },
    };

    const result = await executeHook(hook, transactionQuad);
    expect(result.valid).toBe(true);
  });

  it('after-commit: logs successful transactions', async () => {
    const commitLog = [];

    const hook = defineHook({
      name: 'log-commit',
      trigger: 'after-commit',
      transform: quad => {
        commitLog.push({
          txId: quad.subject.value,
          timestamp: Date.now(),
          status: 'committed',
        });
        return quad;
      },
    });

    const txQuad = {
      subject: { value: 'transaction://67890' },
      predicate: { value: 'http://example.org/status' },
      object: { value: 'success' },
    };

    await executeHook(hook, txQuad);
    expect(commitLog).toHaveLength(1);
    expect(commitLog[0].status).toBe('committed');
  });

  it('before-rollback: captures rollback reasons', async () => {
    const rollbackReasons = [];

    const hook = defineHook({
      name: 'capture-rollback-reason',
      trigger: 'before-rollback',
      transform: quad => {
        rollbackReasons.push({
          txId: quad.subject.value,
          reason: quad.object.value,
        });
        return quad;
      },
    });

    const rollbackQuad = {
      subject: { value: 'transaction://error-123' },
      predicate: { value: 'http://example.org/error' },
      object: { value: 'constraint violation' },
    };

    await executeHook(hook, rollbackQuad);
    expect(rollbackReasons).toHaveLength(1);
    expect(rollbackReasons[0].reason).toBe('constraint violation');
  });

  it('after-rollback: cleans up transaction state', async () => {
    const cleanupLog = [];

    const hook = defineHook({
      name: 'cleanup-after-rollback',
      trigger: 'after-rollback',
      transform: quad => {
        cleanupLog.push({
          txId: quad.subject.value,
          cleaned: true,
        });
        return quad;
      },
    });

    const txQuad = {
      subject: { value: 'transaction://failed-456' },
      predicate: { value: 'http://example.org/status' },
      object: { value: 'rolled-back' },
    };

    await executeHook(hook, txQuad);
    expect(cleanupLog).toHaveLength(1);
    expect(cleanupLog[0].cleaned).toBe(true);
  });
});

describe('Comprehensive Hook Types - Error/Event (5 types)', () => {
  let registry;

  beforeEach(() => {
    registry = createHookRegistry();
    clearHooks(registry);
  });

  it('on-error: handles and logs errors', async () => {
    const errorLog = [];

    const hook = defineHook({
      name: 'error-handler',
      trigger: 'on-error',
      transform: quad => {
        errorLog.push({
          error: quad.object.value,
          severity: quad.predicate.value.includes('critical') ? 'critical' : 'warning',
          timestamp: Date.now(),
        });
        return quad;
      },
    });

    const errorQuad = {
      subject: { value: 'error://001' },
      predicate: { value: 'http://example.org/error/critical' },
      object: { value: 'Database connection failed' },
    };

    await executeHook(hook, errorQuad);
    expect(errorLog).toHaveLength(1);
    expect(errorLog[0].severity).toBe('critical');
  });

  it('on-validation-fail: provides detailed validation errors', async () => {
    const validationErrors = [];

    const hook = defineHook({
      name: 'validation-error-reporter',
      trigger: 'on-validation-fail',
      transform: quad => {
        validationErrors.push({
          field: quad.predicate.value,
          value: quad.object.value,
          reason: 'Invalid format',
        });
        return quad;
      },
    });

    const failedQuad = {
      subject: { value: 'validation://fail-001' },
      predicate: { value: 'http://example.org/email' },
      object: { value: 'not-an-email' },
    };

    await executeHook(hook, failedQuad);
    expect(validationErrors).toHaveLength(1);
    expect(validationErrors[0].reason).toBe('Invalid format');
  });

  it('on-transform: tracks transformation operations', async () => {
    const transformLog = [];

    const hook = defineHook({
      name: 'track-transformations',
      trigger: 'on-transform',
      transform: quad => {
        transformLog.push({
          original: quad.object.value,
          transformed: quad.object.value.toUpperCase(),
        });
        return {
          ...quad,
          object: { value: quad.object.value.toUpperCase() },
        };
      },
    });

    const transformQuad = {
      subject: { value: 'http://example.org/text' },
      predicate: { value: 'http://example.org/content' },
      object: { value: 'hello world' },
    };

    const result = await executeHook(hook, transformQuad);
    expect(transformLog).toHaveLength(1);
    expect(transformLog[0].transformed).toBe('HELLO WORLD');
    expect(result.quad.object.value).toBe('HELLO WORLD');
  });

  it('on-timeout: handles operation timeouts', async () => {
    const timeoutLog = [];

    const hook = defineHook({
      name: 'timeout-handler',
      trigger: 'on-timeout',
      transform: quad => {
        timeoutLog.push({
          operation: quad.predicate.value,
          duration: parseInt(quad.object.value),
        });
        return quad;
      },
    });

    const timeoutQuad = {
      subject: { value: 'operation://slow-query' },
      predicate: { value: 'http://example.org/query/complex' },
      object: { value: '5000' }, // 5 second timeout
    };

    await executeHook(hook, timeoutQuad);
    expect(timeoutLog).toHaveLength(1);
    expect(timeoutLog[0].duration).toBe(5000);
  });

  it('on-circuit-open: activates circuit breaker', async () => {
    const circuitLog = [];

    const hook = defineHook({
      name: 'circuit-breaker',
      trigger: 'on-circuit-open',
      transform: quad => {
        circuitLog.push({
          service: quad.subject.value,
          failureCount: parseInt(quad.object.value),
          state: 'OPEN',
        });
        return quad;
      },
    });

    const circuitQuad = {
      subject: { value: 'service://external-api' },
      predicate: { value: 'http://example.org/failures' },
      object: { value: '5' },
    };

    await executeHook(hook, circuitQuad);
    expect(circuitLog).toHaveLength(1);
    expect(circuitLog[0].state).toBe('OPEN');
  });
});

describe('Comprehensive Hook Types - Async/IO (6 types)', () => {
  let registry;

  beforeEach(() => {
    registry = createHookRegistry();
    clearHooks(registry);
  });

  it('before-fetch: adds authentication headers', async () => {
    const hook = defineHook({
      name: 'add-auth-header',
      trigger: 'before-fetch',
      transform: quad => {
        return {
          ...quad,
          object: {
            ...quad.object,
            auth: 'Bearer token123',
          },
        };
      },
    });

    const fetchQuad = {
      subject: { value: 'fetch://api.example.com/data' },
      predicate: { value: 'http://example.org/request' },
      object: { value: 'GET' },
    };

    const result = await executeHook(hook, fetchQuad);
    expect(result.quad.object.auth).toBe('Bearer token123');
  });

  it('after-fetch: validates response data', async () => {
    const hook = defineHook({
      name: 'validate-response',
      trigger: 'after-fetch',
      validate: quad => {
        // Validate response status is 2xx
        const status = parseInt(quad.object.value);
        return status >= 200 && status < 300;
      },
    });

    const successQuad = {
      subject: { value: 'response://req-001' },
      predicate: { value: 'http://example.org/status' },
      object: { value: '200' },
    };

    const result = await executeHook(hook, successQuad);
    expect(result.valid).toBe(true);

    const errorQuad = {
      ...successQuad,
      object: { value: '500' },
    };

    const result2 = await executeHook(hook, errorQuad);
    expect(result2.valid).toBe(false);
  });

  it('before-sync: prepares data for synchronization', async () => {
    const syncLog = [];

    const hook = defineHook({
      name: 'prepare-sync',
      trigger: 'before-sync',
      transform: quad => {
        syncLog.push({
          entity: quad.subject.value,
          prepared: true,
          timestamp: Date.now(),
        });
        return {
          ...quad,
          object: {
            ...quad.object,
            syncTimestamp: Date.now(),
          },
        };
      },
    });

    const syncQuad = {
      subject: { value: 'http://example.org/entity/123' },
      predicate: { value: 'http://example.org/sync' },
      object: { value: 'pending' },
    };

    const result = await executeHook(hook, syncQuad);
    expect(syncLog).toHaveLength(1);
    expect(result.quad.object.syncTimestamp).toBeDefined();
  });

  it('after-sync: confirms synchronization', async () => {
    const confirmLog = [];

    const hook = defineHook({
      name: 'confirm-sync',
      trigger: 'after-sync',
      transform: quad => {
        confirmLog.push({
          entity: quad.subject.value,
          status: quad.object.value,
          confirmed: true,
        });
        return quad;
      },
    });

    const syncQuad = {
      subject: { value: 'http://example.org/entity/456' },
      predicate: { value: 'http://example.org/sync/status' },
      object: { value: 'completed' },
    };

    await executeHook(hook, syncQuad);
    expect(confirmLog).toHaveLength(1);
    expect(confirmLog[0].confirmed).toBe(true);
  });

  it('before-import: validates import data format', async () => {
    const hook = defineHook({
      name: 'validate-import-format',
      trigger: 'before-import',
      validate: quad => {
        // Validate import data has required fields
        return quad.object.value && quad.object.value.trim().length > 0;
      },
    });

    const validImport = {
      subject: { value: 'import://batch-001' },
      predicate: { value: 'http://example.org/data' },
      object: { value: '@prefix ex: <http://example.org/> .' },
    };

    const result = await executeHook(hook, validImport);
    expect(result.valid).toBe(true);
  });

  it('after-import: logs import statistics', async () => {
    const importStats = [];

    const hook = defineHook({
      name: 'log-import-stats',
      trigger: 'after-import',
      transform: quad => {
        importStats.push({
          batchId: quad.subject.value,
          count: parseInt(quad.object.value),
          success: true,
        });
        return quad;
      },
    });

    const importQuad = {
      subject: { value: 'import://batch-002' },
      predicate: { value: 'http://example.org/count' },
      object: { value: '1500' },
    };

    await executeHook(hook, importQuad);
    expect(importStats).toHaveLength(1);
    expect(importStats[0].count).toBe(1500);
  });
});

describe('Comprehensive Hook Types - Cron/Time (4 types)', () => {
  let registry;

  beforeEach(() => {
    registry = createHookRegistry();
    clearHooks(registry);
  });

  it('on-schedule: executes scheduled maintenance', async () => {
    const maintenanceLog = [];

    const hook = defineHook({
      name: 'scheduled-cleanup',
      trigger: 'on-schedule',
      transform: quad => {
        maintenanceLog.push({
          task: quad.predicate.value,
          executed: Date.now(),
        });
        return quad;
      },
    });

    const scheduleQuad = {
      subject: { value: 'schedule://daily-cleanup' },
      predicate: { value: 'http://example.org/task/vacuum' },
      object: { value: '0 2 * * *' }, // 2 AM daily
    };

    await executeHook(hook, scheduleQuad);
    expect(maintenanceLog).toHaveLength(1);
  });

  it('on-interval: performs periodic health checks', async () => {
    const healthChecks = [];

    const hook = defineHook({
      name: 'health-check',
      trigger: 'on-interval',
      transform: quad => {
        healthChecks.push({
          service: quad.subject.value,
          interval: parseInt(quad.object.value),
          healthy: true,
        });
        return quad;
      },
    });

    const intervalQuad = {
      subject: { value: 'service://database' },
      predicate: { value: 'http://example.org/health' },
      object: { value: '60000' }, // 60 seconds
    };

    await executeHook(hook, intervalQuad);
    expect(healthChecks).toHaveLength(1);
    expect(healthChecks[0].interval).toBe(60000);
  });

  it('on-idle: triggers background optimization', async () => {
    const optimizationLog = [];

    const hook = defineHook({
      name: 'idle-optimization',
      trigger: 'on-idle',
      transform: quad => {
        optimizationLog.push({
          task: 'index-optimization',
          started: Date.now(),
        });
        return quad;
      },
    });

    const idleQuad = {
      subject: { value: 'system://idle' },
      predicate: { value: 'http://example.org/idle/duration' },
      object: { value: '300000' }, // 5 minutes idle
    };

    await executeHook(hook, idleQuad);
    expect(optimizationLog).toHaveLength(1);
    expect(optimizationLog[0].task).toBe('index-optimization');
  });

  it('on-startup: initializes system resources', async () => {
    const initLog = [];

    const hook = defineHook({
      name: 'startup-init',
      trigger: 'on-startup',
      transform: quad => {
        initLog.push({
          resource: quad.predicate.value,
          initialized: true,
        });
        return quad;
      },
    });

    const startupQuad = {
      subject: { value: 'system://startup' },
      predicate: { value: 'http://example.org/resource/cache' },
      object: { value: 'initializing' },
    };

    await executeHook(hook, startupQuad);
    expect(initLog).toHaveLength(1);
    expect(initLog[0].initialized).toBe(true);
  });
});

describe('Comprehensive Hook Types - Lean Six Sigma Quality (8 types)', () => {
  let registry;

  beforeEach(() => {
    registry = createHookRegistry();
    clearHooks(registry);
  });

  it('quality-gate: enforces quality thresholds', async () => {
    const hook = defineHook({
      name: 'quality-threshold',
      trigger: 'quality-gate',
      validate: quad => {
        // Ensure quality score is above 95%
        const score = parseFloat(quad.object.value);
        return score >= 0.95;
      },
    });

    const highQuality = {
      subject: { value: 'batch://data-001' },
      predicate: { value: 'http://example.org/quality/score' },
      object: { value: '0.98' },
    };

    const result = await executeHook(hook, highQuality);
    expect(result.valid).toBe(true);

    const lowQuality = {
      ...highQuality,
      object: { value: '0.85' },
    };

    const result2 = await executeHook(hook, lowQuality);
    expect(result2.valid).toBe(false);
  });

  it('defect-detection: identifies data quality issues', async () => {
    const defects = [];

    const hook = defineHook({
      name: 'detect-defects',
      trigger: 'defect-detection',
      transform: quad => {
        // Check for common defects
        if (!quad.object.value || quad.object.value.trim() === '') {
          defects.push({
            type: 'empty-value',
            subject: quad.subject.value,
          });
        }
        return quad;
      },
    });

    const defectiveQuad = {
      subject: { value: 'http://example.org/record/123' },
      predicate: { value: 'http://example.org/name' },
      object: { value: '' },
    };

    await executeHook(hook, defectiveQuad);
    expect(defects).toHaveLength(1);
    expect(defects[0].type).toBe('empty-value');
  });

  it('continuous-improvement: tracks improvement metrics', async () => {
    const improvements = [];

    const hook = defineHook({
      name: 'track-improvements',
      trigger: 'continuous-improvement',
      transform: quad => {
        improvements.push({
          metric: quad.predicate.value,
          before: parseFloat(quad.object.value),
          timestamp: Date.now(),
        });
        return quad;
      },
    });

    const metricQuad = {
      subject: { value: 'metric://query-time' },
      predicate: { value: 'http://example.org/metric/avg-query-time' },
      object: { value: '45' }, // ms
    };

    await executeHook(hook, metricQuad);
    expect(improvements).toHaveLength(1);
    expect(improvements[0].before).toBe(45);
  });

  it('spc-control: monitors statistical process control', async () => {
    const spcData = [];

    const hook = defineHook({
      name: 'spc-monitoring',
      trigger: 'spc-control',
      validate: quad => {
        const value = parseFloat(quad.object.value);
        const ucl = 100; // Upper Control Limit
        const lcl = 0; // Lower Control Limit

        spcData.push({
          value,
          inControl: value >= lcl && value <= ucl,
        });

        return value >= lcl && value <= ucl;
      },
    });

    const controlQuad = {
      subject: { value: 'process://manufacturing' },
      predicate: { value: 'http://example.org/spc/measurement' },
      object: { value: '50' },
    };

    const result = await executeHook(hook, controlQuad);
    expect(result.valid).toBe(true);
    expect(spcData[0].inControl).toBe(true);
  });

  it('capability-analysis: assesses process capability', async () => {
    const capabilityMetrics = [];

    const hook = defineHook({
      name: 'calculate-cpk',
      trigger: 'capability-analysis',
      transform: quad => {
        const cpk = parseFloat(quad.object.value);
        capabilityMetrics.push({
          process: quad.subject.value,
          cpk,
          capable: cpk >= 1.33, // Industry standard
        });
        return quad;
      },
    });

    const capabilityQuad = {
      subject: { value: 'process://data-quality' },
      predicate: { value: 'http://example.org/capability/cpk' },
      object: { value: '1.67' },
    };

    await executeHook(hook, capabilityQuad);
    expect(capabilityMetrics).toHaveLength(1);
    expect(capabilityMetrics[0].capable).toBe(true);
  });

  it('root-cause: performs root cause analysis', async () => {
    const rootCauses = [];

    const hook = defineHook({
      name: 'identify-root-cause',
      trigger: 'root-cause',
      transform: quad => {
        rootCauses.push({
          defect: quad.subject.value,
          cause: quad.object.value,
          identified: true,
        });
        return quad;
      },
    });

    const rcaQuad = {
      subject: { value: 'defect://data-corruption' },
      predicate: { value: 'http://example.org/rca/cause' },
      object: { value: 'Missing input validation' },
    };

    await executeHook(hook, rcaQuad);
    expect(rootCauses).toHaveLength(1);
    expect(rootCauses[0].identified).toBe(true);
  });

  it('kaizen-event: facilitates continuous improvement events', async () => {
    const kaizenEvents = [];

    const hook = defineHook({
      name: 'kaizen-tracker',
      trigger: 'kaizen-event',
      transform: quad => {
        kaizenEvents.push({
          event: quad.subject.value,
          improvement: quad.object.value,
          completed: true,
        });
        return quad;
      },
    });

    const kaizenQuad = {
      subject: { value: 'kaizen://2024-Q1-sprint' },
      predicate: { value: 'http://example.org/kaizen/improvement' },
      object: { value: 'Reduced query time by 40%' },
    };

    await executeHook(hook, kaizenQuad);
    expect(kaizenEvents).toHaveLength(1);
    expect(kaizenEvents[0].completed).toBe(true);
  });

  it('audit-trail: maintains compliance audit trail', async () => {
    const auditTrail = [];

    const hook = defineHook({
      name: 'compliance-audit',
      trigger: 'audit-trail',
      transform: quad => {
        auditTrail.push({
          timestamp: Date.now(),
          user: quad.subject.value,
          action: quad.predicate.value,
          resource: quad.object.value,
        });
        return quad;
      },
    });

    const auditQuad = {
      subject: { value: 'user://admin' },
      predicate: { value: 'http://example.org/action/delete' },
      object: { value: 'record://sensitive-123' },
    };

    await executeHook(hook, auditQuad);
    expect(auditTrail).toHaveLength(1);
    expect(auditTrail[0].action).toBe('http://example.org/action/delete');
  });
});

describe('Hook Chains - Complex Workflows', () => {
  it('chains validation and transformation hooks', async () => {
    const log = [];

    const validateHook = defineHook({
      name: 'validate-iri',
      trigger: 'before-add',
      validate: quad => {
        log.push('validation');
        return quad.subject.value.startsWith('http://');
      },
    });

    const transformHook = defineHook({
      name: 'normalize-language',
      trigger: 'before-add',
      transform: quad => {
        log.push('transform');
        return {
          ...quad,
          object: {
            ...quad.object,
            language: quad.object.language?.toLowerCase(),
          },
        };
      },
    });

    const auditHook = defineHook({
      name: 'audit-operation',
      trigger: 'before-add',
      transform: quad => {
        log.push('audit');
        return quad;
      },
    });

    const testQuad = {
      subject: { value: 'http://example.org/entity' },
      predicate: { value: 'http://example.org/label' },
      object: { value: 'Test', language: 'EN' },
    };

    const result = await executeHookChain([validateHook, transformHook, auditHook], testQuad);

    expect(result.valid).toBe(true);
    expect(result.quad.object.language).toBe('en');
    expect(log).toEqual(['validation', 'transform', 'audit']);
  });

  it('stops chain on validation failure', async () => {
    const executed = [];

    const failHook = defineHook({
      name: 'fail-validation',
      trigger: 'before-add',
      validate: () => {
        executed.push('fail');
        return false;
      },
    });

    const transformHook = defineHook({
      name: 'should-not-execute',
      trigger: 'before-add',
      transform: quad => {
        executed.push('transform');
        return quad;
      },
    });

    const testQuad = {
      subject: { value: 'http://example.org/test' },
      predicate: { value: 'http://example.org/p' },
      object: { value: 'test' },
    };

    const result = await executeHookChain([failHook, transformHook], testQuad);

    expect(result.valid).toBe(false);
    expect(executed).toEqual(['fail']); // Transform should not execute
  });
});
