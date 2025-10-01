/**
 * @file KGC Sidecar Configuration for Kubernetes
 * @module config
 * 
 * @description
 * Configuration file for KGC sidecar running in Kubernetes environment.
 * This configuration is optimized for E2E testing with Testcontainers.
 */

export default {
  // Environment configuration
  environment: '${environment}',
  namespace: '${namespace}',
  
  // Base configuration
  basePath: '/app/data',
  strictMode: true,
  enableConditionEvaluation: true,
  maxHooks: 10000,
  timeout: 2000,
  enableCache: true,
  cacheMaxAge: 300000,
  enableMetrics: true,
  logLevel: 'info',
  
  // Performance configuration
  performance: {
    enableProfiling: false,
    maxConcurrency: 10,
    afterHashOnly: true,
    timeoutMs: 2000,
    maxHooks: 10000,
    enableFastPath: true,
    enableCaching: true,
    enableBatchProcessing: true,
    cacheSize: 10000,
    batchSize: 1000
  },
  
  // Observability configuration
  observability: {
    enableTracing: true,
    enableMetrics: true,
    enableLogging: true,
    serviceName: 'kgc-sidecar',
    serviceVersion: '1.0.0',
    endpoint: process.env.OBSERVABILITY_ENDPOINT || 'http://jaeger:14268/api/traces',
    samplingRatio: 1.0,
    maxQueueSize: 2048,
    maxExportBatchSize: 512,
    exportTimeoutMillis: 30000,
    scheduledDelayMillis: 5000,
    resourceAttributes: {
      'k8s.namespace': '${namespace}',
      'k8s.environment': '${environment}',
      'service.instance.id': process.env.HOSTNAME || 'unknown'
    }
  },
  
  // Sandbox configuration
  sandbox: {
    type: 'worker',
    timeout: 30000,
    memoryLimit: 64 * 1024 * 1024, // 64MB
    cpuLimit: 50, // 50% CPU
    allowedModules: [],
    allowedGlobals: ['console', 'Date', 'Math', 'JSON', 'Array', 'Object'],
    enableNetwork: false,
    enableFileSystem: false,
    enableProcess: false,
    strictMode: true
  },
  
  // Lockchain configuration
  lockchain: {
    enableGitAnchoring: false, // Disabled for E2E tests
    repository: '/app/data/lockchain',
    batchMode: true,
    batchSize: 100
  },
  
  // Resolution layer configuration
  resolution: {
    enabled: false, // Disabled for E2E tests
    strategy: 'voting',
    consensusThreshold: 0.6,
    timeoutMs: 5000
  },
  
  // Database configuration
  database: {
    url: process.env.DATABASE_URL || 'postgresql://test:test@postgres:5432/kgc_test',
    pool: {
      min: 2,
      max: 10,
      idleTimeoutMillis: 30000,
      connectionTimeoutMillis: 2000
    }
  },
  
  // API configuration
  api: {
    port: 3000,
    host: '0.0.0.0',
    cors: {
      origin: '*',
      methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
      allowedHeaders: ['Content-Type', 'Authorization', 'X-Requested-With']
    },
    rateLimit: {
      windowMs: 15 * 60 * 1000, // 15 minutes
      max: 1000 // limit each IP to 1000 requests per windowMs
    }
  },
  
  // Health check configuration
  health: {
    enabled: true,
    path: '/health',
    interval: 30000, // 30 seconds
    timeout: 5000,   // 5 seconds
    retries: 3
  },
  
  // Metrics configuration
  metrics: {
    enabled: true,
    port: 8080,
    path: '/metrics',
    collectDefaultMetrics: true,
    customMetrics: {
      transactionLatency: true,
      hookExecutionRate: true,
      errorRate: true,
      memoryUsage: true,
      cacheHitRate: true
    }
  },
  
  // Security configuration
  security: {
    apiKey: process.env.API_KEY,
    encryptionKey: process.env.ENCRYPTION_KEY,
    jwtSecret: process.env.JWT_SECRET || 'test-jwt-secret',
    sessionSecret: process.env.SESSION_SECRET || 'test-session-secret',
    cors: {
      origin: process.env.CORS_ORIGIN || '*',
      credentials: true
    }
  },
  
  // E2E testing configuration
  e2e: {
    enabled: true,
    testMode: true,
    mockExternalServices: true,
    disableRealTimeouts: true,
    fastMode: true,
    testDataPath: '/app/test-data',
    fixturesPath: '/app/fixtures'
  },
  
  // Kubernetes specific configuration
  kubernetes: {
    enabled: true,
    namespace: '${namespace}',
    serviceAccount: 'kgc-sidecar-sa',
    configMap: 'kgc-sidecar-config',
    secret: 'kgc-sidecar-secrets',
    deployment: 'kgc-sidecar',
    service: 'kgc-sidecar-service',
    ingress: 'kgc-sidecar-ingress'
  },
  
  // Testcontainers configuration
  testcontainers: {
    enabled: true,
    networkName: 'kgc-test-network',
    containers: {
      postgres: {
        image: 'postgres:15-alpine',
        environment: {
          POSTGRES_DB: 'kgc_test',
          POSTGRES_USER: 'test',
          POSTGRES_PASSWORD: 'test'
        },
        ports: [5432],
        volumes: ['/app/test-data/postgres:/var/lib/postgresql/data']
      },
      jaeger: {
        image: 'jaegertracing/all-in-one:latest',
        environment: {
          COLLECTOR_OTLP_ENABLED: 'true'
        },
        ports: [14268, 16686]
      },
      redis: {
        image: 'redis:7-alpine',
        ports: [6379],
        command: ['redis-server', '--appendonly', 'yes']
      }
    }
  }
};
