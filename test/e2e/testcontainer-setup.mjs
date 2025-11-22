/**
 * @file Testcontainers E2E Test Setup
 * @module testcontainer-setup
 *
 * @description
 * Sets up Testcontainers for end-to-end testing of the KGC JS sidecar.
 * Provides containerized services for realistic testing environments.
 */

import { GenericContainer, _StartedTestContainer } from 'testcontainers';
import { _createWriteStream, _createReadStream } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';

/**
 * Test environment configuration
 */
export const TEST_CONFIG = {
  // Container timeouts
  STARTUP_TIMEOUT: 30000,
  SHUTDOWN_TIMEOUT: 10000,

  // Test data paths
  TEST_DATA_DIR: join(tmpdir(), 'unrdf-e2e-tests'),

  // Service ports
  PORTS: {
    POSTGRES: 5432,
    REDIS: 6379,
    MINIO: 9000,
    MINIO_CONSOLE: 9001,
    FUSEKI: 3030,
  },
};

/**
 * Base container manager for E2E tests
 */
export class E2ETestContainer {
  /**
   *
   */
  constructor() {
    this.containers = new Map();
    this.cleanup = [];
  }

  /**
   * Start a container and track it for cleanup
   * @param {GenericContainer} container - The container to start
   * @param {string} name - Name for tracking
   * @returns {Promise<StartedTestContainer>}
   */
  async startContainer(container, name) {
    console.log(`Starting container: ${name}`);
    const started = await container.start();
    this.containers.set(name, started);

    // Add cleanup function
    this.cleanup.push(async () => {
      console.log(`Stopping container: ${name}`);
      await started.stop();
    });

    return started;
  }

  /**
   * Clean up all containers
   */
  async cleanup() {
    console.log('Cleaning up containers...');
    for (const cleanupFn of this.cleanup) {
      try {
        await cleanupFn();
      } catch (error) {
        console.warn('Error during cleanup:', error.message);
      }
    }
    this.containers.clear();
    this.cleanup = [];
  }

  /**
   * Get a started container by name
   * @param {string} name - Container name
   * @returns {StartedTestContainer}
   */
  getContainer(name) {
    return this.containers.get(name);
  }
}

/**
 * PostgreSQL container for RDF storage testing
 */
export class PostgresContainer {
  /**
   *
   */
  constructor() {
    this.container = new GenericContainer('postgres:15-alpine')
      .withExposedPorts(TEST_CONFIG.PORTS.POSTGRES)
      .withEnvironment({
        POSTGRES_DB: 'unrdf_test',
        POSTGRES_USER: 'testuser',
        POSTGRES_PASSWORD: 'testpass',
      })
      .withStartupTimeout(TEST_CONFIG.STARTUP_TIMEOUT);
  }

  /**
   *
   */
  async start() {
    return await this.container.start();
  }

  /**
   *
   */
  getConnectionString(container) {
    const host = container.getHost();
    const port = container.getMappedPort(TEST_CONFIG.PORTS.POSTGRES);
    return `postgresql://testuser:testpass@${host}:${port}/unrdf_test`;
  }
}

/**
 * Redis container for caching and session storage
 */
export class RedisContainer {
  /**
   *
   */
  constructor() {
    this.container = new GenericContainer('redis:7-alpine')
      .withExposedPorts(TEST_CONFIG.PORTS.REDIS)
      .withStartupTimeout(TEST_CONFIG.STARTUP_TIMEOUT);
  }

  /**
   *
   */
  async start() {
    return await this.container.start();
  }

  /**
   *
   */
  getConnectionString(container) {
    const host = container.getHost();
    const port = container.getMappedPort(TEST_CONFIG.PORTS.REDIS);
    return `redis://${host}:${port}`;
  }
}

/**
 * MinIO container for S3-compatible object storage
 */
export class MinioContainer {
  /**
   *
   */
  constructor() {
    this.container = new GenericContainer('minio/minio:latest')
      .withExposedPorts(TEST_CONFIG.PORTS.MINIO, TEST_CONFIG.PORTS.MINIO_CONSOLE)
      .withEnvironment({
        MINIO_ROOT_USER: 'minioadmin',
        MINIO_ROOT_PASSWORD: 'minioadmin',
      })
      .withCommand(['server', '/data', '--console-address', ':9001'])
      .withStartupTimeout(TEST_CONFIG.STARTUP_TIMEOUT);
  }

  /**
   *
   */
  async start() {
    return await this.container.start();
  }

  /**
   *
   */
  getConnectionConfig(container) {
    const host = container.getHost();
    const port = container.getMappedPort(TEST_CONFIG.PORTS.MINIO);
    const consolePort = container.getMappedPort(TEST_CONFIG.PORTS.MINIO_CONSOLE);

    return {
      endpoint: `http://${host}:${port}`,
      consoleEndpoint: `http://${host}:${consolePort}`,
      accessKey: 'minioadmin',
      secretKey: 'minioadmin',
      bucket: 'unrdf-test',
    };
  }
}

/**
 * Apache Jena Fuseki container for SPARQL endpoint testing
 */
export class FusekiContainer {
  /**
   *
   */
  constructor() {
    this.container = new GenericContainer('stain/jena-fuseki:4.9.0')
      .withExposedPorts(TEST_CONFIG.PORTS.FUSEKI)
      .withEnvironment({
        FUSEKI_DATASET_1: 'unrdf-test',
        ADMIN_PASSWORD: 'admin123',
      })
      .withStartupTimeout(TEST_CONFIG.STARTUP_TIMEOUT);
  }

  /**
   *
   */
  async start() {
    return await this.container.start();
  }

  /**
   *
   */
  getSparqlEndpoint(container) {
    const host = container.getHost();
    const port = container.getMappedPort(TEST_CONFIG.PORTS.FUSEKI);
    return `http://${host}:${port}/unrdf-test/sparql`;
  }

  /**
   *
   */
  getUpdateEndpoint(container) {
    const host = container.getHost();
    const port = container.getMappedPort(TEST_CONFIG.PORTS.FUSEKI);
    return `http://${host}:${port}/unrdf-test/update`;
  }
}

/**
 * Node.js container for testing the KGC sidecar
 */
export class NodeContainer {
  /**
   *
   */
  constructor() {
    this.container = new GenericContainer('node:18-alpine')
      .withExposedPorts(3000)
      .withWorkingDir('/app')
      .withCopyFilesToContainer([
        {
          source: join(process.cwd(), 'dist'),
          target: '/app/dist',
        },
        {
          source: join(process.cwd(), 'package.json'),
          target: '/app/package.json',
        },
        {
          source: join(process.cwd(), 'pnpm-lock.yaml'),
          target: '/app/pnpm-lock.yaml',
        },
      ])
      .withCommand(['sh', '-c', 'npm install -g pnpm && pnpm install && node dist/index.mjs'])
      .withStartupTimeout(TEST_CONFIG.STARTUP_TIMEOUT);
  }

  /**
   *
   */
  async start() {
    return await this.container.start();
  }

  /**
   *
   */
  getApiEndpoint(container) {
    const host = container.getHost();
    const port = container.getMappedPort(3000);
    return `http://${host}:${port}`;
  }
}

/**
 * Test data management utilities
 */
export class TestDataManager {
  /**
   *
   */
  constructor() {
    this.testData = new Map();
  }

  /**
   * Create test RDF data
   * @param {string} name - Dataset name
   * @param {Object} data - RDF data structure
   */
  createTestData(name, data) {
    this.testData.set(name, data);
  }

  /**
   * Get test data by name
   * @param {string} name - Dataset name
   * @returns {Object}
   */
  getTestData(name) {
    return this.testData.get(name);
  }

  /**
   * Create sample knowledge graph data
   * @returns {Object}
   */
  createSampleKnowledgeGraph() {
    return {
      '@context': {
        '@vocab': 'https://example.org/',
        schema: 'https://schema.org/',
        rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
        rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
      },
      '@graph': [
        {
          '@id': 'https://example.org/alice',
          '@type': 'schema:Person',
          'schema:name': 'Alice Doe',
          'schema:email': 'alice@example.org',
          'schema:knows': {
            '@id': 'https://example.org/bob',
          },
        },
        {
          '@id': 'https://example.org/bob',
          '@type': 'schema:Person',
          'schema:name': 'Bob Smith',
          'schema:email': 'bob@example.org',
        },
        {
          '@id': 'https://example.org/acme',
          '@type': 'schema:Organization',
          'schema:name': 'Acme Corp',
          'schema:employee': {
            '@id': 'https://example.org/alice',
          },
        },
      ],
    };
  }

  /**
   * Create sample policy pack data
   * @returns {Object}
   */
  createSamplePolicyPack() {
    return {
      meta: {
        name: 'test-policy-pack',
        version: '1.0.0',
        description: 'Test policy pack for E2E testing',
      },
      hooks: [
        {
          id: 'test-hook-1',
          name: 'Test Hook 1',
          description: 'A test hook for validation',
          when: {
            kind: 'sparql-ask',
            query: 'ASK WHERE { ?s a schema:Person }',
          },
          then: {
            kind: 'javascript',
            code: `
              return {
                success: true,
                message: 'Test hook executed successfully',
                timestamp: new Date().toISOString()
              };
            `,
          },
        },
      ],
      resources: {
        'test-data.ttl': `
          @prefix schema: <https://schema.org/> .
          @prefix ex: <https://example.org/> .
          
          ex:testPerson a schema:Person ;
            schema:name "Test Person" ;
            schema:email "test@example.org" .
        `,
      },
    };
  }
}

/**
 * E2E test environment setup
 */
export class E2ETestEnvironment {
  /**
   *
   */
  constructor() {
    this.containerManager = new E2ETestContainer();
    this.dataManager = new TestDataManager();
    this.services = new Map();
  }

  /**
   * Start all required services
   */
  async startServices() {
    console.log('Starting E2E test environment...');

    // Start PostgreSQL
    const postgres = new PostgresContainer();
    const postgresContainer = await this.containerManager.startContainer(
      postgres.container,
      'postgres'
    );
    this.services.set('postgres', {
      container: postgresContainer,
      connectionString: postgres.getConnectionString(postgresContainer),
    });

    // Start Redis
    const redis = new RedisContainer();
    const redisContainer = await this.containerManager.startContainer(redis.container, 'redis');
    this.services.set('redis', {
      container: redisContainer,
      connectionString: redis.getConnectionString(redisContainer),
    });

    // Start MinIO
    const minio = new MinioContainer();
    const minioContainer = await this.containerManager.startContainer(minio.container, 'minio');
    this.services.set('minio', {
      container: minioContainer,
      config: minio.getConnectionConfig(minioContainer),
    });

    // Start Fuseki
    const fuseki = new FusekiContainer();
    const fusekiContainer = await this.containerManager.startContainer(fuseki.container, 'fuseki');
    this.services.set('fuseki', {
      container: fusekiContainer,
      sparqlEndpoint: fuseki.getSparqlEndpoint(fusekiContainer),
      updateEndpoint: fuseki.getUpdateEndpoint(fusekiContainer),
    });

    // Start Node.js app
    const nodeApp = new NodeContainer();
    const nodeContainer = await this.containerManager.startContainer(nodeApp.container, 'node-app');
    this.services.set('node-app', {
      container: nodeContainer,
      apiEndpoint: nodeApp.getApiEndpoint(nodeContainer),
    });

    console.log('E2E test environment started successfully');
    return this.services;
  }

  /**
   * Get service configuration
   * @param {string} serviceName - Service name
   * @returns {Object}
   */
  getService(serviceName) {
    return this.services.get(serviceName);
  }

  /**
   * Clean up all services
   */
  async cleanup() {
    console.log('Cleaning up E2E test environment...');
    await this.containerManager.cleanup();
    this.services.clear();
  }

  /**
   * Wait for services to be ready
   * @param {number} timeout - Timeout in milliseconds
   */
  async waitForServices(timeout = 30000) {
    const startTime = Date.now();

    while (Date.now() - startTime < timeout) {
      try {
        // Check if all services are responding
        const postgres = this.getService('postgres');
        const redis = this.getService('redis');
        const minio = this.getService('minio');
        const fuseki = this.getService('fuseki');
        const nodeApp = this.getService('node-app');

        if (postgres && redis && minio && fuseki && nodeApp) {
          console.log('All services are ready');
          return;
        }
      } catch (error) {
        console.log('Waiting for services...', error.message);
      }

      await new Promise(resolve => setTimeout(resolve, 1000));
    }

    throw new Error('Services did not become ready within timeout');
  }
}

export default {
  E2ETestEnvironment,
  E2ETestContainer,
  PostgresContainer,
  RedisContainer,
  MinioContainer,
  FusekiContainer,
  NodeContainer,
  TestDataManager,
  TEST_CONFIG,
};
