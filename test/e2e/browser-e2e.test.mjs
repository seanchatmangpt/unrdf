/**
 * @file Browser E2E Tests with Testcontainers
 * @module browser-e2e
 * 
 * @description
 * End-to-end tests for browser compatibility using Testcontainers.
 * Tests the KGC JS sidecar in browser-like environments.
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { E2ETestEnvironment, TestDataManager } from './testcontainer-setup.mjs';
import { createBrowserKnowledgeEngine } from '../../src/knowledge-engine/browser.mjs';

describe('Browser E2E Tests', () => {
  let testEnv;
  let dataManager;
  let browserEngine;

  beforeAll(async () => {
    // Initialize test environment
    testEnv = new E2ETestEnvironment();
    dataManager = new TestDataManager();
    
    // Start services (excluding Node.js app for browser testing)
    await testEnv.startServices();
    await testEnv.waitForServices();
    
    // Initialize browser knowledge engine
    const redis = testEnv.getService('redis');
    const minio = testEnv.getService('minio');
    
    browserEngine = createBrowserKnowledgeEngine({
      cache: {
        type: 'memory', // Use memory cache in browser
        maxSize: 1000
      },
      objectStorage: {
        type: 'browser-storage', // Use browser storage
        quota: 50 * 1024 * 1024 // 50MB
      },
      worker: {
        enabled: true,
        maxWorkers: 4
      }
    });
  }, 60000);

  afterAll(async () => {
    // Clean up test environment
    if (testEnv) {
      await testEnv.cleanup();
    }
  });

  beforeEach(() => {
    // Reset test data for each test
    dataManager.createTestData('sample-kg', dataManager.createSampleKnowledgeGraph());
    dataManager.createTestData('sample-policy', dataManager.createSamplePolicyPack());
  });

  describe('Browser Knowledge Engine', () => {
    it('should load and query knowledge graph in browser environment', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data into the browser engine
      await browserEngine.loadData(sampleData);
      
      // Query for persons
      const persons = await browserEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person ?name WHERE {
          ?person a schema:Person ;
            schema:name ?name .
        }
      `);
      
      expect(persons).toHaveLength(2);
      expect(persons[0].name.value).toBe('Alice Doe');
      expect(persons[1].name.value).toBe('Bob Smith');
    });

    it('should handle Web Worker-based effect execution', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data
      await browserEngine.loadData(sampleData);
      
      // Define a hook that uses Web Workers
      const workerHook = {
        id: 'worker-hook',
        name: 'Web Worker Hook',
        description: 'Hook that executes in Web Worker',
        when: {
          kind: 'sparql-ask',
          query: `
            PREFIX schema: <https://schema.org/>
            ASK WHERE {
              ?person a schema:Person .
            }
          `
        },
        then: {
          kind: 'javascript',
          code: `
            // Simulate some computation
            let result = 0;
            for (let i = 0; i < 1000000; i++) {
              result += Math.sqrt(i);
            }
            
            return {
              success: true,
              message: 'Web Worker computation completed',
              result: result,
              timestamp: new Date().toISOString()
            };
          `
        }
      };
      
      // Register the hook
      await browserEngine.addHook(workerHook);
      
      // Add data that should trigger the hook
      await browserEngine.update(`
        PREFIX schema: <https://schema.org/>
        PREFIX ex: <https://example.org/>
        
        INSERT DATA {
          ex:testPerson a schema:Person ;
            schema:name "Test Person" .
        }
      `);
      
      // Check hook execution
      const hookResults = await browserEngine.getHookResults();
      const workerResult = hookResults.find(r => r.hookId === 'worker-hook');
      
      expect(workerResult).toBeDefined();
      expect(workerResult.success).toBe(true);
      expect(workerResult.result).toBeGreaterThan(0);
    });

    it('should use browser storage for persistence', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data
      await browserEngine.loadData(sampleData);
      
      // Save to browser storage
      await browserEngine.saveToStorage('test-dataset');
      
      // Create new engine instance
      const newEngine = createBrowserKnowledgeEngine({
        cache: { type: 'memory', maxSize: 1000 },
        objectStorage: { type: 'browser-storage', quota: 50 * 1024 * 1024 },
        worker: { enabled: true, maxWorkers: 4 }
      });
      
      // Load from browser storage
      await newEngine.loadFromStorage('test-dataset');
      
      // Verify data was loaded
      const persons = await newEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person ?name WHERE {
          ?person a schema:Person ;
            schema:name ?name .
        }
      `);
      
      expect(persons).toHaveLength(2);
      expect(persons[0].name.value).toBe('Alice Doe');
      expect(persons[1].name.value).toBe('Bob Smith');
    });

    it('should handle memory constraints gracefully', async () => {
      const startTime = Date.now();
      
      // Generate large dataset to test memory limits
      const largeDataset = {
        '@context': {
          '@vocab': 'https://example.org/',
          'schema': 'https://schema.org/'
        },
        '@graph': []
      };
      
      // Create 10000 persons (should trigger memory management)
      for (let i = 0; i < 10000; i++) {
        largeDataset['@graph'].push({
          '@id': `https://example.org/person${i}`,
          '@type': 'schema:Person',
          'schema:name': `Person ${i}`,
          'schema:email': `person${i}@example.org`,
          'schema:description': `This is person ${i} with a longer description to increase memory usage. `.repeat(10)
        });
      }
      
      // Load large dataset
      await browserEngine.loadData(largeDataset);
      
      const loadTime = Date.now() - startTime;
      console.log(`Loaded 10000 persons in ${loadTime}ms`);
      
      // Query should still work
      const persons = await browserEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person ?name WHERE {
          ?person a schema:Person ;
            schema:name ?name .
        }
        LIMIT 10
      `);
      
      expect(persons).toHaveLength(10);
      expect(loadTime).toBeLessThan(30000); // Should load within 30 seconds
    });
  });

  describe('Browser-Specific Features', () => {
    it('should handle browser storage quota limits', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data
      await browserEngine.loadData(sampleData);
      
      // Try to save large amount of data
      try {
        await browserEngine.saveToStorage('large-dataset');
        // Should succeed or fail gracefully
      } catch (error) {
        expect(error.message).toContain('quota');
      }
    });

    it('should handle Web Worker lifecycle', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data
      await browserEngine.loadData(sampleData);
      
      // Define multiple hooks that use workers
      const workerHooks = [];
      for (let i = 0; i < 5; i++) {
        workerHooks.push({
          id: `worker-hook-${i}`,
          name: `Web Worker Hook ${i}`,
          description: `Hook ${i} that executes in Web Worker`,
          when: {
            kind: 'sparql-ask',
            query: `
              PREFIX schema: <https://schema.org/>
              ASK WHERE {
                ?person a schema:Person .
              }
            `
          },
          then: {
            kind: 'javascript',
            code: `
              // Simulate work
              await new Promise(resolve => setTimeout(resolve, 100));
              return {
                success: true,
                message: \`Worker \${${i}} completed\`,
                workerId: ${i}
              };
            `
          }
        });
      }
      
      // Register all hooks
      for (const hook of workerHooks) {
        await browserEngine.addHook(hook);
      }
      
      // Add data that should trigger all hooks
      await browserEngine.update(`
        PREFIX schema: <https://schema.org/>
        PREFIX ex: <https://example.org/>
        
        INSERT DATA {
          ex:testPerson a schema:Person ;
            schema:name "Test Person" .
        }
      `);
      
      // Wait for all workers to complete
      await new Promise(resolve => setTimeout(resolve, 1000));
      
      // Check hook execution
      const hookResults = await browserEngine.getHookResults();
      const workerResults = hookResults.filter(r => r.hookId.startsWith('worker-hook-'));
      
      expect(workerResults).toHaveLength(5);
      workerResults.forEach(result => {
        expect(result.success).toBe(true);
        expect(result.message).toContain('Worker');
      });
    });

    it('should handle browser tab visibility changes', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data
      await browserEngine.loadData(sampleData);
      
      // Simulate tab becoming hidden
      await browserEngine.handleVisibilityChange(false);
      
      // Operations should still work
      const persons = await browserEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(persons).toHaveLength(2);
      
      // Simulate tab becoming visible again
      await browserEngine.handleVisibilityChange(true);
      
      // Operations should still work
      const personsAfter = await browserEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(personsAfter).toHaveLength(2);
    });

    it('should handle browser network connectivity changes', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data
      await browserEngine.loadData(sampleData);
      
      // Simulate network going offline
      await browserEngine.handleNetworkChange(false);
      
      // Operations should work with cached data
      const persons = await browserEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(persons).toHaveLength(2);
      
      // Simulate network coming back online
      await browserEngine.handleNetworkChange(true);
      
      // Operations should work normally
      const personsAfter = await browserEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(personsAfter).toHaveLength(2);
    });
  });

  describe('Cross-Browser Compatibility', () => {
    it('should work with different browser storage implementations', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Test with different storage backends
      const storageBackends = [
        { type: 'localStorage', quota: 10 * 1024 * 1024 },
        { type: 'sessionStorage', quota: 5 * 1024 * 1024 },
        { type: 'indexedDB', quota: 100 * 1024 * 1024 }
      ];
      
      for (const backend of storageBackends) {
        const testEngine = createBrowserKnowledgeEngine({
          cache: { type: 'memory', maxSize: 1000 },
          objectStorage: backend,
          worker: { enabled: true, maxWorkers: 2 }
        });
        
        // Load data
        await testEngine.loadData(sampleData);
        
        // Save to storage
        await testEngine.saveToStorage(`test-${backend.type}`);
        
        // Verify data was saved
        const persons = await testEngine.query(`
          PREFIX schema: <https://schema.org/>
          SELECT ?person WHERE {
            ?person a schema:Person .
          }
        `);
        
        expect(persons).toHaveLength(2);
      }
    });

    it('should handle different Web Worker implementations', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Test with different worker configurations
      const workerConfigs = [
        { enabled: true, maxWorkers: 1 },
        { enabled: true, maxWorkers: 4 },
        { enabled: false } // Fallback to main thread
      ];
      
      for (const config of workerConfigs) {
        const testEngine = createBrowserKnowledgeEngine({
          cache: { type: 'memory', maxSize: 1000 },
          objectStorage: { type: 'browser-storage', quota: 50 * 1024 * 1024 },
          worker: config
        });
        
        // Load data
        await testEngine.loadData(sampleData);
        
        // Define a hook that uses workers
        const workerHook = {
          id: `worker-hook-${config.maxWorkers || 'none'}`,
          name: `Worker Hook ${config.maxWorkers || 'none'}`,
          description: `Hook with ${config.maxWorkers || 'no'} workers`,
          when: {
            kind: 'sparql-ask',
            query: `
              PREFIX schema: <https://schema.org/>
              ASK WHERE {
                ?person a schema:Person .
              }
            `
          },
          then: {
            kind: 'javascript',
            code: `
              return {
                success: true,
                message: \`Hook executed with \${${config.maxWorkers || 0}} workers\`,
                config: ${JSON.stringify(config)}
              };
            `
          }
        };
        
        // Register the hook
        await testEngine.addHook(workerHook);
        
        // Add data that should trigger the hook
        await testEngine.update(`
          PREFIX schema: <https://schema.org/>
          PREFIX ex: <https://example.org/>
          
          INSERT DATA {
            ex:testPerson a schema:Person ;
              schema:name "Test Person" .
          }
        `);
        
        // Check hook execution
        const hookResults = await testEngine.getHookResults();
        const workerResult = hookResults.find(r => r.hookId === workerHook.id);
        
        expect(workerResult).toBeDefined();
        expect(workerResult.success).toBe(true);
      }
    });
  });

  describe('Performance in Browser Environment', () => {
    it('should maintain performance with memory constraints', async () => {
      const startTime = Date.now();
      
      // Generate medium dataset
      const mediumDataset = {
        '@context': {
          '@vocab': 'https://example.org/',
          'schema': 'https://schema.org/'
        },
        '@graph': []
      };
      
      // Create 1000 persons
      for (let i = 0; i < 1000; i++) {
        mediumDataset['@graph'].push({
          '@id': `https://example.org/person${i}`,
          '@type': 'schema:Person',
          'schema:name': `Person ${i}`,
          'schema:email': `person${i}@example.org`
        });
      }
      
      // Load dataset
      await browserEngine.loadData(mediumDataset);
      
      const loadTime = Date.now() - startTime;
      console.log(`Loaded 1000 persons in browser in ${loadTime}ms`);
      
      // Query performance
      const queryStart = Date.now();
      const persons = await browserEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person ?name WHERE {
          ?person a schema:Person ;
            schema:name ?name .
        }
        LIMIT 10
      `);
      const queryTime = Date.now() - queryStart;
      
      expect(persons).toHaveLength(10);
      expect(loadTime).toBeLessThan(15000); // Should load within 15 seconds
      expect(queryTime).toBeLessThan(2000); // Should query within 2 seconds
    });

    it('should handle concurrent operations in browser', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await browserEngine.loadData(sampleData);
      
      // Create concurrent operations
      const operations = [];
      
      for (let i = 0; i < 5; i++) {
        operations.push(
          browserEngine.update(`
            PREFIX schema: <https://schema.org/>
            PREFIX ex: <https://example.org/>
            
            INSERT DATA {
              ex:concurrentPerson${i} a schema:Person ;
                schema:name "Concurrent Person ${i}" .
            }
          `)
        );
      }
      
      // Execute all operations concurrently
      const startTime = Date.now();
      await Promise.all(operations);
      const totalTime = Date.now() - startTime;
      
      // Verify all data was added
      const persons = await browserEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(persons).toHaveLength(7); // Original 2 + 5 new
      expect(totalTime).toBeLessThan(10000); // Should complete within 10 seconds
    });
  });
});






