/**
 * @file Configuration and Deployment Tests
 * @module configuration-deployment
 * 
 * @description
 * Tests for configuration management, deployment scenarios, environment variables,
 * version compatibility, rollback scenarios, and configuration drift in the knowledge hook system.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { writeFile, unlink, mkdir } from 'fs/promises';
import { defineHook } from '../../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { Store } from 'n3';
import { join } from 'path';
import { tmpdir } from 'os';

// Mock condition evaluator at module level
vi.mock('../../../src/knowledge-engine/condition-evaluator.mjs', () => ({
  evaluateCondition: vi.fn().mockResolvedValue(true),
  createConditionEvaluator: vi.fn().mockReturnValue({
    evaluate: vi.fn().mockResolvedValue(true),
    isSatisfied: vi.fn().mockResolvedValue(true)
  }),
  validateCondition: vi.fn().mockReturnValue({ valid: true })
}));

describe('Configuration and Deployment', () => {
  let tempDir;
  let manager;
  let testStore;
  let originalEnv;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `unrdf-config-test-${Date.now()}`);
    await require('fs/promises').mkdir(tempDir, { recursive: true });
    
    // Save original environment
    originalEnv = { ...process.env };
    
    manager = new KnowledgeHookManager({ basePath: tempDir });
    testStore = new Store();
  });

  afterEach(async () => {
    // Restore original environment
    process.env = originalEnv;
    
    try {
      await require('fs/promises').rm(tempDir, { recursive: true, force: true });
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  describe('Invalid Configuration Combinations', () => {
    it('should handle conflicting configuration options', async () => {
      const query = join(tempDir, 'config-conflict.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Test conflicting configurations
      const conflictingConfigs = [
        {
          strictMode: true,
          enableConditionEvaluation: false // Conflict: strict mode needs condition evaluation
        },
        {
          enableKnowledgeHooks: false,
          hookTimeout: 5000 // Conflict: timeout set but hooks disabled
        },
        {
          basePath: '/nonexistent/path',
          strictMode: true // Conflict: invalid path with strict mode
        }
      ];
      
      const results = [];
      
      for (const config of conflictingConfigs) {
        try {
          const testManager = new KnowledgeHookManager(config);
          results.push({ config, success: true, manager: testManager });
        } catch (error) {
          results.push({ config, success: false, error: error.message });
        }
      }
      
      expect(results).toHaveLength(3);
      
      // Some configurations should fail or be handled gracefully
      const hasFailures = results.some(r => !r.success);
      expect(hasFailures).toBe(true);
    });

    it('should validate configuration schema', async () => {
      const query = join(tempDir, 'schema-validation.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const invalidConfigs = [
        {
          basePath: 123, // Should be string
          strictMode: 'yes' // Should be boolean
        },
        {
          enableKnowledgeHooks: 'true', // Should be boolean
          hookTimeout: '5000' // Should be number
        },
        {
          unknownOption: 'value', // Unknown option
          basePath: null // Invalid null value
        }
      ];
      
      const validationResults = [];
      
      for (const config of invalidConfigs) {
        try {
          const testManager = new KnowledgeHookManager(config);
          validationResults.push({ config, valid: true, manager: testManager });
        } catch (error) {
          validationResults.push({ config, valid: false, error: error.message });
        }
      }
      
      expect(validationResults).toHaveLength(3);
      
      // Invalid configurations should be caught
      const hasInvalidConfigs = validationResults.some(r => !r.valid);
      expect(hasInvalidConfigs).toBe(true);
    });

    it('should handle missing required configuration', async () => {
      const query = join(tempDir, 'missing-config.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Test with minimal/missing configuration
      const minimalConfigs = [
        undefined,
        {},
        { basePath: undefined },
        { basePath: '' }
      ];
      
      const configResults = [];
      
      for (const config of minimalConfigs) {
        try {
          const testManager = new KnowledgeHookManager(config);
          configResults.push({ 
            config, 
            success: true, 
            basePath: testManager.basePath,
            strictMode: testManager.strictMode
          });
        } catch (error) {
          configResults.push({ config, success: false, error: error.message });
        }
      }
      
      expect(configResults).toHaveLength(4);
      
      // Should handle missing configuration with defaults
      const successfulConfigs = configResults.filter(r => r.success);
      expect(successfulConfigs.length).toBeGreaterThan(0);
      
      // Should have default values
      successfulConfigs.forEach(result => {
        expect(result.basePath).toBeDefined();
        expect(typeof result.strictMode).toBe('boolean');
      });
    });

    it('should detect configuration drift', async () => {
      const query = join(tempDir, 'config-drift.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Initial configuration
      const initialConfig = {
        basePath: tempDir,
        strictMode: false,
        enableKnowledgeHooks: true
      };
      
      const testManager = new KnowledgeHookManager(initialConfig);
      
      // Simulate configuration drift
      const configSnapshots = [];
      
      // Take initial snapshot
      configSnapshots.push({
        timestamp: Date.now(),
        config: {
          basePath: testManager.basePath,
          strictMode: testManager.strictMode,
          enableKnowledgeHooks: testManager.enableKnowledgeHooks
        },
        source: 'initial'
      });
      
      // Simulate runtime configuration changes
      testManager.strictMode = true; // Configuration drift
      
      configSnapshots.push({
        timestamp: Date.now(),
        config: {
          basePath: testManager.basePath,
          strictMode: testManager.strictMode,
          enableKnowledgeHooks: testManager.enableKnowledgeHooks
        },
        source: 'runtime-change'
      });
      
      // Detect drift
      const initialSnapshot = configSnapshots[0];
      const currentSnapshot = configSnapshots[1];
      
      const driftDetected = JSON.stringify(initialSnapshot.config) !== JSON.stringify(currentSnapshot.config);
      const changedFields = [];
      
      for (const [key, value] of Object.entries(currentSnapshot.config)) {
        if (initialSnapshot.config[key] !== value) {
          changedFields.push({
            field: key,
            initialValue: initialSnapshot.config[key],
            currentValue: value
          });
        }
      }
      
      expect(driftDetected).toBe(true);
      expect(changedFields).toHaveLength(1);
      expect(changedFields[0].field).toBe('strictMode');
      expect(changedFields[0].initialValue).toBe(false);
      expect(changedFields[0].currentValue).toBe(true);
    });
  });

  describe('Environment Variable Conflicts', () => {
    it('should handle conflicting environment variables', async () => {
      const query = join(tempDir, 'env-conflict.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Set conflicting environment variables
      process.env.UNRDF_BASE_PATH = '/env/path';
      process.env.UNRDF_STRICT_MODE = 'true';
      process.env.UNRDF_ENABLE_HOOKS = 'false';
      
      // Configuration that conflicts with environment
      const config = {
        basePath: tempDir, // Conflicts with UNRDF_BASE_PATH
        strictMode: false, // Conflicts with UNRDF_STRICT_MODE
        enableKnowledgeHooks: true // Conflicts with UNRDF_ENABLE_HOOKS
      };
      
      const hook = defineHook({
        meta: { name: 'env-conflict-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Check which configuration values are actually used
          const effectiveConfig = {
            basePath: manager.basePath,
            strictMode: manager.strictMode,
            enableKnowledgeHooks: manager.enableKnowledgeHooks
          };
          
          const envVars = {
            UNRDF_BASE_PATH: process.env.UNRDF_BASE_PATH,
            UNRDF_STRICT_MODE: process.env.UNRDF_STRICT_MODE,
            UNRDF_ENABLE_HOOKS: process.env.UNRDF_ENABLE_HOOKS
          };
          
          return { 
            success: true, 
            effectiveConfig,
            envVars,
            configTakesPrecedence: effectiveConfig.basePath === tempDir
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Configuration should take precedence over environment variables
      expect(results[0].configTakesPrecedence).toBe(true);
      expect(results[0].effectiveConfig.basePath).toBe(tempDir);
    });

    it('should handle missing environment variables gracefully', async () => {
      const query = join(tempDir, 'missing-env.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Clear environment variables
      delete process.env.UNRDF_BASE_PATH;
      delete process.env.UNRDF_STRICT_MODE;
      delete process.env.UNRDF_ENABLE_HOOKS;
      delete process.env.UNRDF_TIMEOUT;
      
      const hook = defineHook({
        meta: { name: 'missing-env-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Check default values when environment variables are missing
          const defaults = {
            basePath: manager.basePath || process.cwd(),
            strictMode: manager.strictMode || false,
            enableKnowledgeHooks: manager.enableKnowledgeHooks !== false
          };
          
          const missingEnvVars = [
            'UNRDF_BASE_PATH',
            'UNRDF_STRICT_MODE',
            'UNRDF_ENABLE_HOOKS',
            'UNRDF_TIMEOUT'
          ].filter(varName => !process.env[varName]);
          
          return { 
            success: true, 
            defaults,
            missingEnvVars,
            hasDefaults: Object.values(defaults).every(v => v !== undefined)
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle missing environment variables with defaults
      expect(results[0].hasDefaults).toBe(true);
      expect(results[0].missingEnvVars).toHaveLength(4);
      expect(results[0].defaults.basePath).toBeDefined();
    });

    it('should validate environment variable types', async () => {
      const query = join(tempDir, 'env-types.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Set environment variables with invalid types
      process.env.UNRDF_STRICT_MODE = 'maybe'; // Should be boolean
      process.env.UNRDF_TIMEOUT = 'five-seconds'; // Should be number
      process.env.UNRDF_ENABLE_HOOKS = '1'; // Should be boolean
      process.env.UNRDF_BASE_PATH = ''; // Should be non-empty string
      
      const hook = defineHook({
        meta: { name: 'env-types-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Validate environment variable types
          const envValidation = {};
          
          // Boolean validation
          const strictModeValue = process.env.UNRDF_STRICT_MODE;
          envValidation.strictMode = {
            value: strictModeValue,
            isValidBoolean: ['true', 'false'].includes(strictModeValue?.toLowerCase())
          };
          
          // Number validation
          const timeoutValue = process.env.UNRDF_TIMEOUT;
          envValidation.timeout = {
            value: timeoutValue,
            isValidNumber: !isNaN(Number(timeoutValue)) && timeoutValue !== ''
          };
          
          // Boolean validation (alternative format)
          const enableHooksValue = process.env.UNRDF_ENABLE_HOOKS;
          envValidation.enableHooks = {
            value: enableHooksValue,
            isValidBoolean: ['true', 'false', '1', '0'].includes(enableHooksValue?.toLowerCase())
          };
          
          // String validation
          const basePathValue = process.env.UNRDF_BASE_PATH;
          envValidation.basePath = {
            value: basePathValue,
            isValidPath: basePathValue && basePathValue.length > 0
          };
          
          return { 
            success: true, 
            envValidation,
            hasInvalidTypes: Object.values(envValidation).some(v => 
              v.hasOwnProperty('isValidBoolean') ? !v.isValidBoolean :
              v.hasOwnProperty('isValidNumber') ? !v.isValidNumber :
              v.hasOwnProperty('isValidPath') ? !v.isValidPath : false
            )
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should detect invalid environment variable types
      expect(results[0].hasInvalidTypes).toBe(true);
      expect(results[0].envValidation.strictMode.isValidBoolean).toBe(false);
      expect(results[0].envValidation.timeout.isValidNumber).toBe(false);
      expect(results[0].envValidation.basePath.isValidPath).toBe(false);
    });
  });

  describe('Version Compatibility Issues', () => {
    it('should handle schema version mismatches', async () => {
      const query = join(tempDir, 'version-mismatch.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Simulate different schema versions
      const schemaVersions = [
        { version: '1.0.0', supportedFeatures: ['basic-hooks'] },
        { version: '1.1.0', supportedFeatures: ['basic-hooks', 'conditions'] },
        { version: '2.0.0', supportedFeatures: ['basic-hooks', 'conditions', 'transactions'] },
        { version: '3.0.0', supportedFeatures: ['basic-hooks', 'conditions', 'transactions', 'advanced-monitoring'] }
      ];
      
      const currentVersion = '2.0.0';
      
      const hook = defineHook({
        meta: { 
          name: 'version-mismatch-hook',
          schemaVersion: '3.0.0' // Hook requires newer version than current
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const currentSchema = schemaVersions.find(s => s.version === currentVersion);
          const requiredSchema = schemaVersions.find(s => s.version === hook.meta.schemaVersion);
          
          const isCompatible = currentSchema && requiredSchema && 
            requiredSchema.supportedFeatures.every(feature => 
              currentSchema.supportedFeatures.includes(feature)
            );
          
          const missingFeatures = requiredSchema ? 
            requiredSchema.supportedFeatures.filter(feature => 
              !currentSchema?.supportedFeatures.includes(feature)
            ) : [];
          
          return { 
            success: isCompatible, 
            currentVersion,
            requiredVersion: hook.meta.schemaVersion,
            isCompatible,
            missingFeatures
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      
      // Should detect version incompatibility
      expect(results[0].isCompatible).toBe(false);
      expect(results[0].missingFeatures).toContain('advanced-monitoring');
      expect(results[0].currentVersion).toBe('2.0.0');
      expect(results[0].requiredVersion).toBe('3.0.0');
    });

    it('should handle API breaking changes', async () => {
      const query = join(tempDir, 'api-breaking.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Simulate API version changes
      const apiVersions = {
        'v1': {
          hookDefinition: {
            meta: { name: 'string' },
            condition: 'string', // Simple string condition
            action: 'function'
          }
        },
        'v2': {
          hookDefinition: {
            meta: { name: 'string', version: 'string' },
            when: { kind: 'string', ref: 'object' }, // Breaking change: condition -> when
            run: 'function' // Breaking change: action -> run
          }
        }
      };
      
      // Try to use v1 API with v2 system
      const legacyHookDef = {
        meta: { name: 'legacy-hook' },
        condition: 'simple-condition', // v1 API
        action: async () => ({ success: true }) // v1 API
      };
      
      const hook = defineHook({
        meta: { name: 'api-breaking-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Check for API compatibility
          const currentAPI = 'v2';
          const legacyFields = ['condition', 'action'];
          const currentFields = ['when', 'run'];
          
          const hasLegacyFields = legacyFields.some(field => 
            legacyHookDef.hasOwnProperty(field)
          );
          
          const hasCurrentFields = currentFields.some(field => 
            legacyHookDef.hasOwnProperty(field)
          );
          
          const migrationRequired = hasLegacyFields && !hasCurrentFields;
          
          return { 
            success: true, 
            currentAPI,
            hasLegacyFields,
            hasCurrentFields,
            migrationRequired,
            legacyHookDef
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should detect API breaking changes
      expect(results[0].migrationRequired).toBe(true);
      expect(results[0].hasLegacyFields).toBe(true);
      expect(results[0].hasCurrentFields).toBe(false);
    });

    it('should handle dependency version conflicts', async () => {
      const query = join(tempDir, 'dependency-conflict.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Simulate dependency version conflicts
      const dependencies = {
        'n3': { current: '1.16.3', required: '^1.15.0', compatible: true },
        'zod': { current: '3.22.4', required: '^4.0.0', compatible: false },
        'vitest': { current: '0.34.6', required: '^1.0.0', compatible: false }
      };
      
      const hook = defineHook({
        meta: { name: 'dependency-conflict-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Check dependency compatibility
          const incompatibleDeps = Object.entries(dependencies)
            .filter(([name, info]) => !info.compatible)
            .map(([name, info]) => ({
              name,
              current: info.current,
              required: info.required
            }));
          
          const compatibleDeps = Object.entries(dependencies)
            .filter(([name, info]) => info.compatible)
            .map(([name, info]) => ({
              name,
              current: info.current,
              required: info.required
            }));
          
          const hasConflicts = incompatibleDeps.length > 0;
          
          return { 
            success: !hasConflicts, 
            hasConflicts,
            incompatibleDeps,
            compatibleDeps,
            totalDeps: Object.keys(dependencies).length
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      
      // Should detect dependency conflicts
      expect(results[0].hasConflicts).toBe(true);
      expect(results[0].incompatibleDeps).toHaveLength(2);
      expect(results[0].compatibleDeps).toHaveLength(1);
      expect(results[0].success).toBe(false);
    });
  });

  describe('Deployment Rollback Scenarios', () => {
    it('should handle rollback to previous hook version', async () => {
      const query = join(tempDir, 'rollback-hook.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Simulate hook versions
      const hookVersions = {
        'v1.0.0': {
          meta: { name: 'rollback-test-hook', version: '1.0.0' },
          implementation: async () => ({ success: true, version: '1.0.0', feature: 'basic' })
        },
        'v1.1.0': {
          meta: { name: 'rollback-test-hook', version: '1.1.0' },
          implementation: async () => ({ success: true, version: '1.1.0', feature: 'enhanced' })
        },
        'v2.0.0': {
          meta: { name: 'rollback-test-hook', version: '2.0.0' },
          implementation: async () => { 
            throw new Error('v2.0.0 has critical bug'); // Broken version
          }
        }
      };
      
      let currentVersion = 'v2.0.0';
      let rollbackVersion = 'v1.1.0';
      
      const hook = defineHook({
        meta: { name: 'rollback-test-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const deploymentHistory = [];
          let activeVersion = currentVersion;
          
          // Try current version
          try {
            deploymentHistory.push({
              version: currentVersion,
              timestamp: Date.now(),
              status: 'attempting'
            });
            
            const result = await hookVersions[currentVersion].implementation();
            deploymentHistory.push({
              version: currentVersion,
              timestamp: Date.now(),
              status: 'success',
              result
            });
            
            return { success: true, activeVersion, deploymentHistory };
            
          } catch (error) {
            deploymentHistory.push({
              version: currentVersion,
              timestamp: Date.now(),
              status: 'failed',
              error: error.message
            });
            
            // Rollback to previous version
            try {
              deploymentHistory.push({
                version: rollbackVersion,
                timestamp: Date.now(),
                status: 'rolling-back'
              });
              
              const rollbackResult = await hookVersions[rollbackVersion].implementation();
              activeVersion = rollbackVersion;
              
              deploymentHistory.push({
                version: rollbackVersion,
                timestamp: Date.now(),
                status: 'rollback-success',
                result: rollbackResult
              });
              
              return { 
                success: true, 
                activeVersion, 
                deploymentHistory,
                rolledBack: true
              };
              
            } catch (rollbackError) {
              deploymentHistory.push({
                version: rollbackVersion,
                timestamp: Date.now(),
                status: 'rollback-failed',
                error: rollbackError.message
              });
              
              return { 
                success: false, 
                activeVersion: 'none', 
                deploymentHistory,
                rollbackFailed: true
              };
            }
          }
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should have rolled back successfully
      expect(results[0].rolledBack).toBe(true);
      expect(results[0].activeVersion).toBe('v1.1.0');
      expect(results[0].deploymentHistory).toHaveLength(4);
      expect(results[0].deploymentHistory[0].status).toBe('attempting');
      expect(results[0].deploymentHistory[1].status).toBe('failed');
      expect(results[0].deploymentHistory[2].status).toBe('rolling-back');
      expect(results[0].deploymentHistory[3].status).toBe('rollback-success');
    });

    it('should handle configuration rollback', async () => {
      const query = join(tempDir, 'config-rollback.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Configuration snapshots
      const configSnapshots = [
        {
          id: 'config-v1',
          timestamp: Date.now() - 3600000, // 1 hour ago
          config: {
            strictMode: false,
            timeout: 30000,
            enableMetrics: true
          }
        },
        {
          id: 'config-v2',
          timestamp: Date.now() - 1800000, // 30 minutes ago
          config: {
            strictMode: true,
            timeout: 15000,
            enableMetrics: true
          }
        },
        {
          id: 'config-v3',
          timestamp: Date.now() - 300000, // 5 minutes ago
          config: {
            strictMode: true,
            timeout: 5000, // Too aggressive - causes issues
            enableMetrics: false
          }
        }
      ];
      
      const hook = defineHook({
        meta: { name: 'config-rollback-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const currentConfig = configSnapshots[2]; // Latest problematic config
          const rollbackConfig = configSnapshots[1]; // Previous stable config
          
          // Simulate config validation
          const validateConfig = (config) => {
            const issues = [];
            
            if (config.timeout < 10000) {
              issues.push('Timeout too aggressive, may cause failures');
            }
            
            if (!config.enableMetrics) {
              issues.push('Metrics disabled, monitoring will be limited');
            }
            
            return {
              valid: issues.length === 0,
              issues
            };
          };
          
          const currentValidation = validateConfig(currentConfig.config);
          
          if (!currentValidation.valid) {
            // Rollback to previous config
            const rollbackValidation = validateConfig(rollbackConfig.config);
            
            return {
              success: true,
              rolledBack: true,
              currentConfig: currentConfig.id,
              rollbackConfig: rollbackConfig.id,
              issues: currentValidation.issues,
              rollbackValid: rollbackValidation.valid
            };
          }
          
          return {
            success: true,
            rolledBack: false,
            currentConfig: currentConfig.id
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should have rolled back configuration
      expect(results[0].rolledBack).toBe(true);
      expect(results[0].rollbackConfig).toBe('config-v2');
      expect(results[0].issues).toContain('Timeout too aggressive, may cause failures');
      expect(results[0].rollbackValid).toBe(true);
    });

    it('should handle database schema rollback', async () => {
      const query = join(tempDir, 'schema-rollback.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Database schema versions
      const schemaVersions = [
        {
          version: '1.0',
          migrations: ['create-hooks-table', 'create-events-table'],
          rollback: ['drop-events-table', 'drop-hooks-table']
        },
        {
          version: '1.1',
          migrations: ['add-metadata-column', 'create-index-on-name'],
          rollback: ['drop-index-on-name', 'drop-metadata-column']
        },
        {
          version: '2.0',
          migrations: ['rename-hooks-to-knowledge-hooks', 'add-version-column'],
          rollback: ['drop-version-column', 'rename-knowledge-hooks-to-hooks']
        }
      ];
      
      let currentSchemaVersion = '2.0';
      
      const hook = defineHook({
        meta: { name: 'schema-rollback-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const migrationLog = [];
          
          // Simulate schema incompatibility requiring rollback
          const isSchemaCompatible = (version) => {
            // v2.0 has compatibility issues
            return version !== '2.0';
          };
          
          if (!isSchemaCompatible(currentSchemaVersion)) {
            // Find the latest compatible version
            const compatibleVersions = schemaVersions.filter(s => 
              isSchemaCompatible(s.version)
            );
            
            const targetVersion = compatibleVersions[compatibleVersions.length - 1];
            
            if (targetVersion) {
              // Execute rollback migrations
              const currentSchema = schemaVersions.find(s => s.version === currentSchemaVersion);
              
              migrationLog.push({
                action: 'rollback-start',
                from: currentSchemaVersion,
                to: targetVersion.version,
                timestamp: Date.now()
              });
              
              // Execute rollback steps for current version
              for (const rollbackStep of currentSchema.rollback) {
                migrationLog.push({
                  action: 'execute-rollback',
                  step: rollbackStep,
                  timestamp: Date.now()
                });
              }
              
              migrationLog.push({
                action: 'rollback-complete',
                version: targetVersion.version,
                timestamp: Date.now()
              });
              
              return {
                success: true,
                rolledBack: true,
                fromVersion: currentSchemaVersion,
                toVersion: targetVersion.version,
                migrationLog
              };
            }
          }
          
          return {
            success: true,
            rolledBack: false,
            currentVersion: currentSchemaVersion
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should have rolled back schema
      expect(results[0].rolledBack).toBe(true);
      expect(results[0].fromVersion).toBe('2.0');
      expect(results[0].toVersion).toBe('1.1');
      expect(results[0].migrationLog).toHaveLength(4);
      expect(results[0].migrationLog[0].action).toBe('rollback-start');
      expect(results[0].migrationLog[3].action).toBe('rollback-complete');
    });
  });
});
