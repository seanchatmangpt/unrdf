/**
 * @file Integration E2E Tests with Testcontainers
 * @module integration-e2e
 * 
 * @description
 * Integration tests for the KGC JS sidecar with external services.
 * Tests real-world scenarios with multiple service interactions.
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { E2ETestEnvironment, TestDataManager } from './testcontainer-setup.mjs';
import { createKnowledgeEngine } from '../../src/knowledge-engine/index.mjs';
import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';

describe('Integration E2E Tests', () => {
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
    
    // Initialize knowledge engine with all services
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

  describe('Multi-Service Integration', () => {
    it('should integrate PostgreSQL, Redis, and MinIO', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data into PostgreSQL
      await knowledgeEngine.loadData(sampleData);
      
      // Query data (should use Redis cache)
      const persons = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person ?name WHERE {
          ?person a schema:Person ;
            schema:name ?name .
        }
      `);
      
      expect(persons).toHaveLength(2);
      
      // Store large objects in MinIO
      const largeObject = {
        type: 'knowledge-graph-export',
        timestamp: new Date().toISOString(),
        data: sampleData,
        metadata: {
          size: JSON.stringify(sampleData).length,
          entities: persons.length
        }
      };
      
      const objectKey = `exports/knowledge-graph-${Date.now()}.json`;
      await knowledgeEngine.storeObject(objectKey, largeObject);
      
      // Retrieve object from MinIO
      const retrievedObject = await knowledgeEngine.getObject(objectKey);
      
      expect(retrievedObject.type).toBe('knowledge-graph-export');
      expect(retrievedObject.data).toEqual(sampleData);
      expect(retrievedObject.metadata.entities).toBe(2);
    });

    it('should handle service failures with graceful degradation', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await knowledgeEngine.loadData(sampleData);
      
      // Stop Redis to simulate cache failure
      const redis = testEnv.getService('redis');
      if (redis) {
        await redis.container.stop();
      }
      
      // Operations should still work without cache
      const persons = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(persons).toHaveLength(2);
      
      // Stop MinIO to simulate object storage failure
      const minio = testEnv.getService('minio');
      if (minio) {
        await minio.container.stop();
      }
      
      // Object storage operations should fail gracefully
      try {
        await knowledgeEngine.storeObject('test-key', { test: 'data' });
        expect.fail('Object storage should have failed');
      } catch (error) {
        expect(error.message).toContain('connection');
      }
      
      // Restart services
      if (redis) {
        await redis.container.start();
      }
      if (minio) {
        await minio.container.start();
      }
      
      await testEnv.waitForServices(10000);
      
      // Operations should work again
      const personsAfter = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(personsAfter).toHaveLength(2);
    });

    it('should handle concurrent access from multiple clients', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await knowledgeEngine.loadData(sampleData);
      
      // Create multiple engine instances (simulating multiple clients)
      const engines = [];
      for (let i = 0; i < 3; i++) {
        const postgres = testEnv.getService('postgres');
        const redis = testEnv.getService('redis');
        const minio = testEnv.getService('minio');
        const fuseki = testEnv.getService('fuseki');
        
        const engine = createKnowledgeEngine({
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
        
        engines.push(engine);
      }
      
      // Concurrent operations from different clients
      const operations = engines.map((engine, index) => 
        engine.update(`
          PREFIX schema: <https://schema.org/>
          PREFIX ex: <https://example.org/>
          
          INSERT DATA {
            ex:client${index}Person a schema:Person ;
              schema:name "Client ${index} Person" .
          }
        `)
      );
      
      // Execute all operations concurrently
      await Promise.all(operations);
      
      // Verify all data was added
      const persons = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(persons).toHaveLength(5); // Original 2 + 3 new
    });
  });

  describe('Data Pipeline Integration', () => {
    it('should handle end-to-end data pipeline', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Step 1: Load raw data
      await knowledgeEngine.loadData(sampleData);
      
      // Step 2: Validate data
      const validation = await knowledgeEngine.validate(`
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix schema: <https://schema.org/> .
        @prefix ex: <https://example.org/> .
        
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass schema:Person ;
          sh:property [
            sh:path schema:name ;
            sh:datatype xsd:string ;
            sh:minCount 1
          ] .
      `);
      
      expect(validation.valid).toBe(true);
      
      // Step 3: Apply reasoning rules
      await knowledgeEngine.addRules(`
        PREFIX schema: <https://schema.org/>
        PREFIX ex: <https://example.org/>
        
        # Rule: If someone knows someone, they are connected
        ex:knowsRule a ex:Rule ;
          ex:condition "?person1 schema:knows ?person2" ;
          ex:conclusion "?person1 ex:connectedTo ?person2" .
      `);
      
      const inferred = await knowledgeEngine.reason();
      expect(inferred.length).toBeGreaterThan(0);
      
      // Step 4: Execute hooks
      const pipelineHook = defineHook({
        meta: {
          name: 'pipeline-hook',
          description: 'Hook for data pipeline processing'
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
            return {
              success: true,
              message: 'Data pipeline processing completed',
              timestamp: new Date().toISOString(),
              stage: 'post-reasoning'
            };
          `
        }
      });
      
      await knowledgeEngine.addHook(pipelineHook);
      
      // Step 5: Add more data to trigger pipeline
      await knowledgeEngine.update(`
        PREFIX schema: <https://schema.org/>
        PREFIX ex: <https://example.org/>
        
        INSERT DATA {
          ex:pipelinePerson a schema:Person ;
            schema:name "Pipeline Person" .
        }
      `);
      
      // Step 6: Export processed data
      const exportData = await knowledgeEngine.exportData({
        format: 'jsonld',
        includeInferred: true
      });
      
      expect(exportData['@graph']).toBeDefined();
      expect(exportData['@graph'].length).toBeGreaterThan(2);
      
      // Step 7: Store export in object storage
      const exportKey = `exports/pipeline-export-${Date.now()}.jsonld`;
      await knowledgeEngine.storeObject(exportKey, exportData);
      
      // Step 8: Verify export can be retrieved
      const retrievedExport = await knowledgeEngine.getObject(exportKey);
      expect(retrievedExport['@graph']).toBeDefined();
    });

    it('should handle data transformation pipeline', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await knowledgeEngine.loadData(sampleData);
      
      // Define transformation rules
      const transformationRules = [
        {
          name: 'normalize-names',
          description: 'Normalize person names',
          condition: '?person a schema:Person ; schema:name ?name',
          action: 'UPDATE ?person schema:name ?normalizedName WHERE { ?person a schema:Person ; schema:name ?name . BIND(UCASE(?name) AS ?normalizedName) }'
        },
        {
          name: 'add-timestamps',
          description: 'Add creation timestamps',
          condition: '?person a schema:Person',
          action: 'INSERT { ?person ex:createdAt ?timestamp } WHERE { ?person a schema:Person . BIND(NOW() AS ?timestamp) }'
        }
      ];
      
      // Apply transformations
      for (const rule of transformationRules) {
        await knowledgeEngine.addTransformationRule(rule);
      }
      
      // Execute transformations
      await knowledgeEngine.applyTransformations();
      
      // Verify transformations were applied
      const transformedPersons = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        PREFIX ex: <https://example.org/>
        SELECT ?person ?name ?timestamp WHERE {
          ?person a schema:Person ;
            schema:name ?name ;
            ex:createdAt ?timestamp .
        }
      `);
      
      expect(transformedPersons).toHaveLength(2);
      transformedPersons.forEach(person => {
        expect(person.name.value).toBe(person.name.value.toUpperCase());
        expect(person.timestamp).toBeDefined();
      });
    });
  });

  describe('External Service Integration', () => {
    it('should integrate with external SPARQL endpoints', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data into local Fuseki
      await knowledgeEngine.loadData(sampleData);
      
      // Query external endpoint (simulated)
      const externalQuery = `
        PREFIX schema: <https://schema.org/>
        SELECT ?person ?name WHERE {
          ?person a schema:Person ;
            schema:name ?name .
        }
      `;
      
      // Query local endpoint
      const localResults = await knowledgeEngine.query(externalQuery);
      
      // Query external endpoint (using Fuseki as external)
      const fuseki = testEnv.getService('fuseki');
      const externalResults = await knowledgeEngine.queryExternal(
        fuseki.sparqlEndpoint,
        externalQuery
      );
      
      expect(localResults).toHaveLength(2);
      expect(externalResults).toHaveLength(2);
    });

    it('should handle external API failures', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load data
      await knowledgeEngine.loadData(sampleData);
      
      // Try to query non-existent external endpoint
      try {
        await knowledgeEngine.queryExternal(
          'http://non-existent-endpoint:9999/sparql',
          'SELECT ?s WHERE { ?s ?p ?o }'
        );
        expect.fail('External query should have failed');
      } catch (error) {
        expect(error.message).toContain('connection');
      }
      
      // Local operations should still work
      const persons = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(persons).toHaveLength(2);
    });

    it('should synchronize with external data sources', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await knowledgeEngine.loadData(sampleData);
      
      // Define synchronization hook
      const syncHook = defineHook({
        meta: {
          name: 'external-sync-hook',
          description: 'Hook for external data synchronization'
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
            // Simulate external API call
            const externalData = {
              '@context': {
                '@vocab': 'https://example.org/',
                'schema': 'https://schema.org/'
              },
              '@graph': [
                {
                  '@id': 'https://example.org/externalPerson',
                  '@type': 'schema:Person',
                  'schema:name': 'External Person',
                  'schema:email': 'external@example.org'
                }
              ]
            };
            
            return {
              success: true,
              message: 'External data synchronized',
              data: externalData,
              timestamp: new Date().toISOString()
            };
          `
        }
      });
      
      await knowledgeEngine.addHook(syncHook);
      
      // Add data to trigger synchronization
      await knowledgeEngine.update(`
        PREFIX schema: <https://schema.org/>
        PREFIX ex: <https://example.org/>
        
        INSERT DATA {
          ex:triggerPerson a schema:Person ;
            schema:name "Trigger Person" .
        }
      `);
      
      // Check synchronization results
      const hookResults = await knowledgeEngine.getHookResults();
      const syncResult = hookResults.find(r => r.hookId === 'external-sync-hook');
      
      expect(syncResult).toBeDefined();
      expect(syncResult.success).toBe(true);
      expect(syncResult.data).toBeDefined();
    });
  });

  describe('Performance and Scalability', () => {
    it('should handle high-throughput operations', async () => {
      const startTime = Date.now();
      
      // Generate large dataset
      const largeDataset = {
        '@context': {
          '@vocab': 'https://example.org/',
          'schema': 'https://schema.org/'
        },
        '@graph': []
      };
      
      // Create 5000 persons
      for (let i = 0; i < 5000; i++) {
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
      console.log(`Loaded 5000 persons in ${loadTime}ms`);
      
      // Concurrent queries
      const queryStart = Date.now();
      const queries = [];
      
      for (let i = 0; i < 10; i++) {
        queries.push(
          knowledgeEngine.query(`
            PREFIX schema: <https://schema.org/>
            SELECT ?person ?name WHERE {
              ?person a schema:Person ;
                schema:name ?name .
            }
            LIMIT 100
          `)
        );
      }
      
      const results = await Promise.all(queries);
      const queryTime = Date.now() - queryStart;
      
      results.forEach(result => {
        expect(result).toHaveLength(100);
      });
      
      expect(loadTime).toBeLessThan(20000); // Should load within 20 seconds
      expect(queryTime).toBeLessThan(5000); // Should query within 5 seconds
    });

    it('should handle memory pressure gracefully', async () => {
      const sampleData = dataManager.getTestData('sample-kg');
      
      // Load initial data
      await knowledgeEngine.loadData(sampleData);
      
      // Create memory-intensive operations
      const memoryOperations = [];
      
      for (let i = 0; i < 100; i++) {
        memoryOperations.push(
          knowledgeEngine.update(`
            PREFIX schema: <https://schema.org/>
            PREFIX ex: <https://example.org/>
            
            INSERT DATA {
              ex:memoryPerson${i} a schema:Person ;
                schema:name "Memory Person ${i}" ;
                schema:description "${'Large description '.repeat(100)}" .
            }
          `)
        );
      }
      
      // Execute operations in batches to manage memory
      const batchSize = 10;
      for (let i = 0; i < memoryOperations.length; i += batchSize) {
        const batch = memoryOperations.slice(i, i + batchSize);
        await Promise.all(batch);
        
        // Force garbage collection if available
        if (global.gc) {
          global.gc();
        }
      }
      
      // Verify all data was added
      const persons = await knowledgeEngine.query(`
        PREFIX schema: <https://schema.org/>
        SELECT ?person WHERE {
          ?person a schema:Person .
        }
      `);
      
      expect(persons).toHaveLength(102); // Original 2 + 100 new
    });
  });
});
