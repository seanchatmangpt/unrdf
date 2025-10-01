/**
 * @file Testcontainers Setup for KGC Sidecar E2E Tests
 * @module testcontainers-setup
 * 
 * @description
 * Testcontainers configuration and utilities for E2E testing
 * of the KGC sidecar in Kubernetes environment.
 */

import { GenericContainer, StartedTestContainer, Network } from 'testcontainers';
import { PostgreSqlContainer } from '@testcontainers/postgresql';
import { RedisContainer } from '@testcontainers/redis';

/**
 * Testcontainers configuration for KGC sidecar E2E tests
 */
export const testcontainersConfig = {
  // Network configuration
  network: {
    name: 'kgc-test-network',
    driver: 'bridge'
  },
  
  // PostgreSQL configuration
  postgres: {
    image: 'postgres:15-alpine',
    database: 'kgc_test',
    username: 'test',
    password: 'test',
    port: 5432,
    environment: {
      POSTGRES_DB: 'kgc_test',
      POSTGRES_USER: 'test',
      POSTGRES_PASSWORD: 'test',
      POSTGRES_INITDB_ARGS: '--encoding=UTF-8 --lc-collate=C --lc-ctype=C'
    },
    volumes: [
      {
        host: './test-data/postgres',
        container: '/var/lib/postgresql/data'
      }
    ]
  },
  
  // Redis configuration
  redis: {
    image: 'redis:7-alpine',
    port: 6379,
    command: ['redis-server', '--appendonly', 'yes', '--maxmemory', '256mb'],
    volumes: [
      {
        host: './test-data/redis',
        container: '/data'
      }
    ]
  },
  
  // Jaeger configuration
  jaeger: {
    image: 'jaegertracing/all-in-one:latest',
    ports: [14268, 16686, 14250],
    environment: {
      COLLECTOR_OTLP_ENABLED: 'true',
      COLLECTOR_ZIPKIN_HOST_PORT: ':9411',
      SPAN_STORAGE_TYPE: 'memory',
      LOG_LEVEL: 'debug'
    }
  },
  
  // Prometheus configuration
  prometheus: {
    image: 'prom/prometheus:latest',
    port: 9090,
    volumes: [
      {
        host: './test-data/prometheus/prometheus.yml',
        container: '/etc/prometheus/prometheus.yml'
      }
    ]
  },
  
  // Grafana configuration
  grafana: {
    image: 'grafana/grafana:latest',
    port: 3000,
    environment: {
      GF_SECURITY_ADMIN_PASSWORD: 'admin',
      GF_USERS_ALLOW_SIGN_UP: 'false'
    },
    volumes: [
      {
        host: './test-data/grafana',
        container: '/var/lib/grafana'
      }
    ]
  },
  
  // MinIO configuration (for S3-compatible storage)
  minio: {
    image: 'minio/minio:latest',
    ports: [9000, 9001],
    environment: {
      MINIO_ROOT_USER: 'minioadmin',
      MINIO_ROOT_PASSWORD: 'minioadmin'
    },
    command: ['server', '/data', '--console-address', ':9001'],
    volumes: [
      {
        host: './test-data/minio',
        container: '/data'
      }
    ]
  },
  
  // Elasticsearch configuration
  elasticsearch: {
    image: 'docker.elastic.co/elasticsearch/elasticsearch:8.11.0',
    port: 9200,
    environment: {
      'discovery.type': 'single-node',
      'xpack.security.enabled': 'false',
      'ES_JAVA_OPTS': '-Xms512m -Xmx512m'
    },
    volumes: [
      {
        host: './test-data/elasticsearch',
        container: '/usr/share/elasticsearch/data'
      }
    ]
  },
  
  // Kibana configuration
  kibana: {
    image: 'docker.elastic.co/kibana/kibana:8.11.0',
    port: 5601,
    environment: {
      ELASTICSEARCH_HOSTS: 'http://elasticsearch:9200',
      ELASTICSEARCH_USERNAME: 'elastic',
      ELASTICSEARCH_PASSWORD: 'changeme'
    }
  }
};

/**
 * Testcontainers manager for KGC sidecar E2E tests
 */
export class TestcontainersManager {
  constructor() {
    this.containers = new Map();
    this.network = null;
    this.initialized = false;
  }

  /**
   * Initialize testcontainers network
   */
  async initializeNetwork() {
    if (this.network) {
      return this.network;
    }

    console.log('🌐 Creating testcontainers network...');
    this.network = await new Network().create({
      name: testcontainersConfig.network.name,
      driver: testcontainersConfig.network.driver
    });

    console.log(`✅ Network created: ${this.network.getName()}`);
    return this.network;
  }

  /**
   * Start PostgreSQL container
   */
  async startPostgreSQL() {
    if (this.containers.has('postgres')) {
      return this.containers.get('postgres');
    }

    console.log('🐘 Starting PostgreSQL container...');
    const postgres = await new PostgreSqlContainer(testcontainersConfig.postgres.image)
      .withDatabase(testcontainersConfig.postgres.database)
      .withUsername(testcontainersConfig.postgres.username)
      .withPassword(testcontainersConfig.postgres.password)
      .withExposedPorts(testcontainersConfig.postgres.port)
      .withEnvironment(testcontainersConfig.postgres.environment)
      .withNetwork(this.network)
      .withNetworkAliases('postgres')
      .start();

    this.containers.set('postgres', postgres);
    console.log(`✅ PostgreSQL started on port ${postgres.getMappedPort(5432)}`);
    return postgres;
  }

  /**
   * Start Redis container
   */
  async startRedis() {
    if (this.containers.has('redis')) {
      return this.containers.get('redis');
    }

    console.log('🔴 Starting Redis container...');
    const redis = await new RedisContainer(testcontainersConfig.redis.image)
      .withExposedPorts(testcontainersConfig.redis.port)
      .withCommand(testcontainersConfig.redis.command)
      .withNetwork(this.network)
      .withNetworkAliases('redis')
      .start();

    this.containers.set('redis', redis);
    console.log(`✅ Redis started on port ${redis.getMappedPort(6379)}`);
    return redis;
  }

  /**
   * Start Jaeger container
   */
  async startJaeger() {
    if (this.containers.has('jaeger')) {
      return this.containers.get('jaeger');
    }

    console.log('🔍 Starting Jaeger container...');
    const jaeger = await new GenericContainer(testcontainersConfig.jaeger.image)
      .withExposedPorts(...testcontainersConfig.jaeger.ports)
      .withEnvironment(testcontainersConfig.jaeger.environment)
      .withNetwork(this.network)
      .withNetworkAliases('jaeger')
      .start();

    this.containers.set('jaeger', jaeger);
    console.log(`✅ Jaeger started on ports ${testcontainersConfig.jaeger.ports.join(', ')}`);
    return jaeger;
  }

  /**
   * Start Prometheus container
   */
  async startPrometheus() {
    if (this.containers.has('prometheus')) {
      return this.containers.get('prometheus');
    }

    console.log('📊 Starting Prometheus container...');
    const prometheus = await new GenericContainer(testcontainersConfig.prometheus.image)
      .withExposedPorts(testcontainersConfig.prometheus.port)
      .withNetwork(this.network)
      .withNetworkAliases('prometheus')
      .start();

    this.containers.set('prometheus', prometheus);
    console.log(`✅ Prometheus started on port ${prometheus.getMappedPort(9090)}`);
    return prometheus;
  }

  /**
   * Start Grafana container
   */
  async startGrafana() {
    if (this.containers.has('grafana')) {
      return this.containers.get('grafana');
    }

    console.log('📈 Starting Grafana container...');
    const grafana = await new GenericContainer(testcontainersConfig.grafana.image)
      .withExposedPorts(testcontainersConfig.grafana.port)
      .withEnvironment(testcontainersConfig.grafana.environment)
      .withNetwork(this.network)
      .withNetworkAliases('grafana')
      .start();

    this.containers.set('grafana', grafana);
    console.log(`✅ Grafana started on port ${grafana.getMappedPort(3000)}`);
    return grafana;
  }

  /**
   * Start MinIO container
   */
  async startMinIO() {
    if (this.containers.has('minio')) {
      return this.containers.get('minio');
    }

    console.log('🗄️ Starting MinIO container...');
    const minio = await new GenericContainer(testcontainersConfig.minio.image)
      .withExposedPorts(...testcontainersConfig.minio.ports)
      .withEnvironment(testcontainersConfig.minio.environment)
      .withCommand(testcontainersConfig.minio.command)
      .withNetwork(this.network)
      .withNetworkAliases('minio')
      .start();

    this.containers.set('minio', minio);
    console.log(`✅ MinIO started on ports ${testcontainersConfig.minio.ports.join(', ')}`);
    return minio;
  }

  /**
   * Start Elasticsearch container
   */
  async startElasticsearch() {
    if (this.containers.has('elasticsearch')) {
      return this.containers.get('elasticsearch');
    }

    console.log('🔍 Starting Elasticsearch container...');
    const elasticsearch = await new GenericContainer(testcontainersConfig.elasticsearch.image)
      .withExposedPorts(testcontainersConfig.elasticsearch.port)
      .withEnvironment(testcontainersConfig.elasticsearch.environment)
      .withNetwork(this.network)
      .withNetworkAliases('elasticsearch')
      .start();

    this.containers.set('elasticsearch', elasticsearch);
    console.log(`✅ Elasticsearch started on port ${elasticsearch.getMappedPort(9200)}`);
    return elasticsearch;
  }

  /**
   * Start Kibana container
   */
  async startKibana() {
    if (this.containers.has('kibana')) {
      return this.containers.get('kibana');
    }

    console.log('📊 Starting Kibana container...');
    const kibana = await new GenericContainer(testcontainersConfig.kibana.image)
      .withExposedPorts(testcontainersConfig.kibana.port)
      .withEnvironment(testcontainersConfig.kibana.environment)
      .withNetwork(this.network)
      .withNetworkAliases('kibana')
      .start();

    this.containers.set('kibana', kibana);
    console.log(`✅ Kibana started on port ${kibana.getMappedPort(5601)}`);
    return kibana;
  }

  /**
   * Start all required containers for E2E tests
   */
  async startAll() {
    if (this.initialized) {
      return;
    }

    console.log('🚀 Starting all testcontainers...');
    
    // Initialize network
    await this.initializeNetwork();
    
    // Start core services
    await this.startPostgreSQL();
    await this.startRedis();
    await this.startJaeger();
    
    // Start monitoring services
    await this.startPrometheus();
    await this.startGrafana();
    
    // Start storage services
    await this.startMinIO();
    
    // Start logging services
    await this.startElasticsearch();
    await this.startKibana();
    
    this.initialized = true;
    console.log('✅ All testcontainers started successfully');
  }

  /**
   * Start minimal set of containers for basic E2E tests
   */
  async startMinimal() {
    if (this.initialized) {
      return;
    }

    console.log('🚀 Starting minimal testcontainers...');
    
    // Initialize network
    await this.initializeNetwork();
    
    // Start only essential services
    await this.startPostgreSQL();
    await this.startRedis();
    await this.startJaeger();
    
    this.initialized = true;
    console.log('✅ Minimal testcontainers started successfully');
  }

  /**
   * Get container by name
   */
  getContainer(name) {
    return this.containers.get(name);
  }

  /**
   * Get all containers
   */
  getAllContainers() {
    return Array.from(this.containers.values());
  }

  /**
   * Get container connection info
   */
  getConnectionInfo() {
    const info = {};
    
    for (const [name, container] of this.containers.entries()) {
      info[name] = {
        host: container.getHost(),
        ports: container.getMappedPorts(),
        networkAliases: container.getNetworkAliases()
      };
    }
    
    return info;
  }

  /**
   * Get environment variables for containers
   */
  getEnvironmentVariables() {
    const env = {};
    
    // PostgreSQL
    const postgres = this.containers.get('postgres');
    if (postgres) {
      env.DATABASE_URL = `postgresql://${testcontainersConfig.postgres.username}:${testcontainersConfig.postgres.password}@postgres:5432/${testcontainersConfig.postgres.database}`;
      env.POSTGRES_HOST = 'postgres';
      env.POSTGRES_PORT = '5432';
      env.POSTGRES_DB = testcontainersConfig.postgres.database;
      env.POSTGRES_USER = testcontainersConfig.postgres.username;
      env.POSTGRES_PASSWORD = testcontainersConfig.postgres.password;
    }
    
    // Redis
    const redis = this.containers.get('redis');
    if (redis) {
      env.REDIS_URL = 'redis://redis:6379';
      env.REDIS_HOST = 'redis';
      env.REDIS_PORT = '6379';
    }
    
    // Jaeger
    const jaeger = this.containers.get('jaeger');
    if (jaeger) {
      env.OBSERVABILITY_ENDPOINT = 'http://jaeger:14268/api/traces';
      env.JAEGER_ENDPOINT = 'http://jaeger:14268/api/traces';
      env.JAEGER_AGENT_HOST = 'jaeger';
      env.JAEGER_AGENT_PORT = '6831';
    }
    
    // Prometheus
    const prometheus = this.containers.get('prometheus');
    if (prometheus) {
      env.PROMETHEUS_URL = 'http://prometheus:9090';
      env.PROMETHEUS_HOST = 'prometheus';
      env.PROMETHEUS_PORT = '9090';
    }
    
    // Grafana
    const grafana = this.containers.get('grafana');
    if (grafana) {
      env.GRAFANA_URL = 'http://grafana:3000';
      env.GRAFANA_HOST = 'grafana';
      env.GRAFANA_PORT = '3000';
    }
    
    // MinIO
    const minio = this.containers.get('minio');
    if (minio) {
      env.MINIO_URL = 'http://minio:9000';
      env.MINIO_HOST = 'minio';
      env.MINIO_PORT = '9000';
      env.MINIO_ACCESS_KEY = testcontainersConfig.minio.environment.MINIO_ROOT_USER;
      env.MINIO_SECRET_KEY = testcontainersConfig.minio.environment.MINIO_ROOT_PASSWORD;
    }
    
    // Elasticsearch
    const elasticsearch = this.containers.get('elasticsearch');
    if (elasticsearch) {
      env.ELASTICSEARCH_URL = 'http://elasticsearch:9200';
      env.ELASTICSEARCH_HOST = 'elasticsearch';
      env.ELASTICSEARCH_PORT = '9200';
    }
    
    // Kibana
    const kibana = this.containers.get('kibana');
    if (kibana) {
      env.KIBANA_URL = 'http://kibana:5601';
      env.KIBANA_HOST = 'kibana';
      env.KIBANA_PORT = '5601';
    }
    
    return env;
  }

  /**
   * Stop all containers
   */
  async stopAll() {
    console.log('🛑 Stopping all testcontainers...');
    
    for (const [name, container] of this.containers.entries()) {
      try {
        console.log(`🛑 Stopping ${name}...`);
        await container.stop();
        console.log(`✅ ${name} stopped`);
      } catch (error) {
        console.warn(`⚠️ Failed to stop ${name}:`, error.message);
      }
    }
    
    this.containers.clear();
    
    if (this.network) {
      try {
        console.log('🛑 Stopping network...');
        await this.network.stop();
        console.log('✅ Network stopped');
      } catch (error) {
        console.warn('⚠️ Failed to stop network:', error.message);
      }
      this.network = null;
    }
    
    this.initialized = false;
    console.log('✅ All testcontainers stopped');
  }

  /**
   * Cleanup test data
   */
  async cleanup() {
    console.log('🧹 Cleaning up test data...');
    
    // Stop all containers
    await this.stopAll();
    
    // Clean up test data directories
    const fs = await import('fs');
    const path = await import('path');
    
    const testDataDirs = [
      './test-data/postgres',
      './test-data/redis',
      './test-data/prometheus',
      './test-data/grafana',
      './test-data/minio',
      './test-data/elasticsearch'
    ];
    
    for (const dir of testDataDirs) {
      try {
        if (fs.existsSync(dir)) {
          fs.rmSync(dir, { recursive: true, force: true });
          console.log(`✅ Cleaned up ${dir}`);
        }
      } catch (error) {
        console.warn(`⚠️ Failed to cleanup ${dir}:`, error.message);
      }
    }
    
    console.log('✅ Test data cleanup completed');
  }
}

/**
 * Create a new testcontainers manager instance
 */
export function createTestcontainersManager() {
  return new TestcontainersManager();
}

/**
 * Default testcontainers manager instance
 */
export const testcontainersManager = createTestcontainersManager();

export default TestcontainersManager;
