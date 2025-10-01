/**
 * @file Knowledge Engine E2E Tests with Testcontainers
 * @module knowledge-engine-e2e
 * 
 * @description
 * End-to-end tests for the KGC JS sidecar using Testcontainers.
 * Tests real-world scenarios with containerized services.
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach, afterEach } from 'vitest';
import { E2ETestEnvironment, TestDataManager } from './testcontainer-setup.mjs';
import { createKnowledgeEngine } from '../../src/knowledge-engine/index.mjs';
import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';

describe('Knowledge Engine E2E Tests', () => {
  let testEnv;
  let dataManager;
  let knowledgeEngine;

  beforeAll(async () => {
    // Initialize test environment
    testEnv = new E2ETestEnvironment();
    dataManager = new TestDataManager();
    
    // Start all services
    await testEnv.startServices();
    await testEnv.waitForServices();
    
    // Initialize knowledge engine with test services
    const postgres = testEnv.getService('postgres');
    const redis = testEnv.getService('redis');
    const minio = testEnv.getService('minio');
    const fuseki = testEnv.getService('fuseki');
    
    knowledgeEngine = createKnowledgeEngine({
      storage: {
        type: 'postgres',
        connectionString: postgres.connectionString
      },
      cache: {
        type: 'redis',
        connectionString: redis.connectionString
      },
      objectStorage: {
        type: 's3',
        endpoint: minio.config.endpoint,
        accessKey: minio.config.accessKey,
        secretKey: minio.config.secretKey,
        bucket: minio.config.bucket
      },
      sparqlEndpoint: fuseki.sparqlEndpoint,
      updateEndpoint: fuseki.updateEndpoint
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

  describe('Knowledge Graph Operations', () => {
    it('should load and query knowledge graph from PostgreSQL', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data into the knowledge engine
      await knowledgeEngine.loadData(sampleData);
      
      // Query for persons
      const persons = await knowledgeEngine.query(`
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

    it('should perform reasoning and inference', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data
      await knowledgeEngine.loadData(sampleData);
      
      // Add reasoning rules
      await knowledgeEngine.addRules(`
        PREFIX schema: <https://schema.org/>
        PREFIX ex: <https://example.org/>
        
        # Rule: If someone knows someone, they are connected
        ex:knowsRule a ex:Rule ;
          ex:condition "?person1 schema:knows ?person2" ;
          ex:conclusion "?person1 ex:connectedTo ?person2" .
      `);
      
      // Perform reasoning
      const inferred = await knowledgeEngine.reason();
      
      // Check for inferred connections
      const connections = await knowledgeEngine.query(`
        PREFIX ex: <https://example.org/>
        SELECT ?person1 ?person2 WHERE {
          ?person1 ex:connectedTo ?person2 .
        }
      `);
      
      expect(connections).toHaveLength(1);
      expect(connections[0].person1.value).toBe('https://example.org/alice');
      expect(connections[0].person2.value).toBe('https://example.org/bob');
    });

    it('should validate data against SHACL shapes', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data
      await knowledgeEngine.loadData(sampleData);
      
      // Define SHACL shapes
      const shapes = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix schema: <https://schema.org/> .
        @prefix ex: <https://example.org/> .
        
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass schema:Person ;
          sh:property [
            sh:path schema:name ;
            sh:datatype xsd:string ;
            sh:minCount 1 ;
            sh:maxCount 1
          ] ;
          sh:property [
            sh:path schema:email ;
            sh:datatype xsd:string ;
            sh:pattern "^[^@]+@[^@]+\\.[^@]+$" ;
            sh:minCount 1
          ] .
      `;
      
      // Validate data
      const validation = await knowledgeEngine.validate(shapes);
      
      expect(validation.valid).toBe(true);
      expect(validation.errors).toHaveLength(0);
    });
  });

  describe('Knowledge Hooks Integration', () => {
    it('should execute hooks on data changes', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await knowledgeEngine.loadData(sampleData);
      
      // Define a hook that triggers on person additions
      const personHook = defineHook({
        meta: {
          name: 'person-added-hook',
          description: 'Hook that triggers when a person is added'
        },
        when: {
          kind: 'sparql-ask',
          query: `
            PREFIX schema: <https://schema.org/>
            ASK WHERE {
              ?person a schema:Person ;
                schema:name ?name .
            }
          `
        },
        then: {
          kind: 'javascript',
          code: `
            return {
              success: true,
              message: 'Person detected in knowledge graph',
              timestamp: new Date().toISOString(),
              action: 'log-person-addition'
            };
          `
        }
      });
      
      // Register the hook
      await knowledgeEngine.addHook(personHook);
      
      // Add a new person
      await knowledgeEngine.update(`
        PREFIX schema: <https://schema.org/>
        PREFIX ex: <https://example.org/>
        
        INSERT DATA {
          ex:charlie a schema:Person ;
            schema:name "Charlie Brown" ;
            schema:email "charlie@example.org" .
        }
      `);
      
      // Check if hook was executed
      const hookResults = await knowledgeEngine.getHookResults();
      expect(hookResults).toHaveLength(1);
      expect(hookResults[0].hookId).toBe('person-added-hook');
      expect(hookResults[0].success).toBe(true);
    });

    it('should handle hook failures gracefully', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await knowledgeEngine.loadData(sampleData);
      
      // Define a hook that will fail
      const failingHook = defineHook({
        meta: {
          name: 'failing-hook',
          description: 'Hook that intentionally fails'
        },
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
            throw new Error('Intentional hook failure for testing');
          `
        }
      });
      
      // Register the hook
      await knowledgeEngine.addHook(failingHook);
      
      // Add data that should trigger the hook
      await knowledgeEngine.update(`
        PREFIX schema: <https://schema.org/>
        PREFIX ex: <https://example.org/>
        
        INSERT DATA {
          ex:testPerson a schema:Person ;
            schema:name "Test Person" .
        }
      `);
      
      // Check that the operation completed despite hook failure
      const persons = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(persons).toHaveLength(3); // Original 2 + new 1
      
      // Check hook results
      const hookResults = await knowledgeEngine.getHookResults();
      const failingResult = hookResults.find(r => r.hookId === 'failing-hook');
      expect(failingResult).toBeDefined();
      expect(failingResult.success).toBe(false);
      expect(failingResult.error).toContain('Intentional hook failure');
    });
  });

  describe('Policy Pack Management', () => {
    it('should load and activate policy packs', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      const policyPack = dataManager.getTestData('sample-policy');
      
      // Load data
      await knowledgeEngine.loadData(sampleData);
      
      // Load policy pack
      await knowledgeEngine.loadPolicyPack(policyPack);
      
      // Activate policy pack
      await knowledgeEngine.activatePolicyPack('test-policy-pack');
      
      // Check that hooks from policy pack are registered
      const hooks = await knowledgeEngine.getHooks();
      expect(hooks).toHaveLength(1);
      expect(hooks[0].id).toBe('test-hook-1');
      
      // Execute a transaction that should trigger the hook
      await knowledgeEngine.update(`
        PREFIX schema: <https://schema.org/>
        PREFIX ex: <https://example.org/>
        
        INSERT DATA {
          ex:newPerson a schema:Person ;
            schema:name "New Person" .
        }
      `);
      
      // Check hook execution
      const hookResults = await knowledgeEngine.getHookResults();
      const testHookResult = hookResults.find(r => r.hookId === 'test-hook-1');
      expect(testHookResult).toBeDefined();
      expect(testHookResult.success).toBe(true);
    });

    it('should deactivate policy packs', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      const policyPack = dataManager.getTestData('sample-policy');
      
      // Load data and policy pack
      await knowledgeEngine.loadData(sampleData);
      await knowledgeEngine.loadPolicyPack(policyPack);
      await knowledgeEngine.activatePolicyPack('test-policy-pack');
      
      // Verify hook is active
      let hooks = await knowledgeEngine.getHooks();
      expect(hooks).toHaveLength(1);
      
      // Deactivate policy pack
      await knowledgeEngine.deactivatePolicyPack('test-policy-pack');
      
      // Verify hook is no longer active
      hooks = await knowledgeEngine.getHooks();
      expect(hooks).toHaveLength(0);
    });
  });

  describe('Transaction Management', () => {
    it('should handle atomic transactions', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await knowledgeEngine.loadData(sampleData);
      
      // Start a transaction
      const transaction = await knowledgeEngine.beginTransaction();
      
      try {
        // Add multiple related entities
        await transaction.update(`
          PREFIX schema: <https://schema.org/>
          PREFIX ex: <https://example.org/>
          
          INSERT DATA {
            ex:company a schema:Organization ;
              schema:name "Test Company" .
            
            ex:employee a schema:Person ;
              schema:name "Test Employee" ;
              schema:worksFor ex:company .
          }
        `);
        
        // Commit transaction
        await transaction.commit();
        
        // Verify all data was added
        const companies = await knowledgeEngine.query(`
          PREFIX schema: <https://schema.org/>
          SELECT ?company WHERE {
            ?company a schema:Organization .
          }
        `);
        
        const employees = await knowledgeEngine.query(`
          PREFIX schema: <https://schema.org/>
          SELECT ?employee WHERE {
            ?employee a schema:Person ;
              schema:worksFor ?company .
          }
        `);
        
        expect(companies).toHaveLength(1);
        expect(employees).toHaveLength(1);
        
      } catch (error) {
        await transaction.rollback();
        throw error;
      }
    });

    it('should rollback failed transactions', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await knowledgeEngine.loadData(sampleData);
      
      // Count initial persons
      const initialPersons = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      const initialCount = initialPersons.length;
      
      // Start a transaction
      const transaction = await knowledgeEngine.beginTransaction();
      
      try {
        // Add a person
        await transaction.update(`
          PREFIX schema: <https://schema.org/>
          PREFIX ex: <https://example.org/>
          
          INSERT DATA {
            ex:tempPerson a schema:Person ;
              schema:name "Temp Person" .
          }
        `);
        
        // Simulate an error (e.g., validation failure)
        throw new Error('Simulated transaction failure');
        
      } catch (error) {
        // Rollback transaction
        await transaction.rollback();
        
        // Verify no data was added
        const finalPersons = await knowledgeEngine.query(`
          PREFIX schema: <https://schema.org/>
          SELECT ?person WHERE {
            ?person a schema:Person .
          }
        `);
        
        expect(finalPersons).toHaveLength(initialCount);
      }
    });
  });

  describe('Performance and Scalability', () => {
    it('should handle large datasets efficiently', async () => {
      const startTime = Date.now();
      
      // Generate large dataset
      const largeDataset = {
        '@context': {
          '@vocab': 'https://example.org/',
          'schema': 'https://schema.org/'
        },
        '@graph': []
      };
      
      // Create 1000 persons
      for (let i = 0; i < 1000; i++) {
        largeDataset['@graph'].push({
          '@id': `https://example.org/person${i}`,
          '@type': 'schema:Person',
          'schema:name': `Person ${i}`,
          'schema:email': `person${i}@example.org`
        });
      }
      
      // Load large dataset
      await knowledgeEngine.loadData(largeDataset);
      
      const loadTime = Date.now() - startTime;
      console.log(`Loaded 1000 persons in ${loadTime}ms`);
      
      // Query should be fast
      const queryStart = Date.now();
      const persons = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person ?name WHERE {
          ?person a schema:Person ;
            schema:name ?name .
        }
        LIMIT 10
      `);
      const queryTime = Date.now() - queryStart;
      
      expect(persons).toHaveLength(10);
      expect(loadTime).toBeLessThan(10000); // Should load within 10 seconds
      expect(queryTime).toBeLessThan(1000); // Should query within 1 second
    });

    it('should handle concurrent operations', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await knowledgeEngine.loadData(sampleData);
      
      // Create concurrent operations
      const operations = [];
      
      for (let i = 0; i < 10; i++) {
        operations.push(
          knowledgeEngine.update(`
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
      const persons = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(persons).toHaveLength(12); // Original 2 + 10 new
      expect(totalTime).toBeLessThan(5000); // Should complete within 5 seconds
    });
  });

  describe('Error Handling and Recovery', () => {
    it('should handle service failures gracefully', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await knowledgeEngine.loadData(sampleData);
      
      // Simulate service failure by stopping a container
      const postgres = testEnv.getService('postgres');
      if (postgres) {
        await postgres.container.stop();
      }
      
      // Operations should fail gracefully
      try {
        await knowledgeEngine.query(`
          PREFIX schema: <https://schema.org/>
          SELECT ?person WHERE {
            ?person a schema:Person .
          }
        `);
        expect.fail('Query should have failed due to service unavailability');
      } catch (error) {
        expect(error.message).toContain('connection');
      }
      
      // Restart service
      if (postgres) {
        await postgres.container.start();
        await testEnv.waitForServices(10000);
      }
      
      // Operations should work again
      const persons = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(persons).toHaveLength(2);
    });

    it('should recover from partial failures', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await knowledgeEngine.loadData(sampleData);
      
      // Define a hook that sometimes fails
      const flakyHook = defineHook({
        meta: {
          name: 'flaky-hook',
          description: 'Hook that sometimes fails'
        },
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
            // Fail randomly 50% of the time
            if (Math.random() < 0.5) {
              throw new Error('Random hook failure');
            }
            return { success: true, message: 'Hook succeeded' };
          `
        }
      });
      
      // Register the hook
      await knowledgeEngine.addHook(flakyHook);
      
      // Execute multiple operations
      const results = [];
      for (let i = 0; i < 10; i++) {
        try {
          await knowledgeEngine.update(`
            PREFIX schema: <https://schema.org/>
            PREFIX ex: <https://example.org/>
            
            INSERT DATA {
              ex:testPerson${i} a schema:Person ;
                schema:name "Test Person ${i}" .
            }
          `);
          results.push({ success: true, index: i });
        } catch (error) {
          results.push({ success: false, index: i, error: error.message });
        }
      }
      
      // Some operations should succeed, some should fail
      const successful = results.filter(r => r.success);
      const failed = results.filter(r => !r.success);
      
      expect(successful.length).toBeGreaterThan(0);
      expect(failed.length).toBeGreaterThan(0);
      
      // Verify data consistency
      const persons = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      // Should have original 2 + successful operations
      expect(persons).toHaveLength(2 + successful.length);
    });
  });
});




