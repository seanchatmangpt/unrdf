/**
 * @file Cleanroom Testcontainer Stack with OpenTelemetry
 * @module test/e2e/cleanroom/testcontainer-stack
 *
 * @description
 * Programmatic testcontainer setup for cleanroom integration testing.
 * Provides isolated, reproducible environment with full OTEL observability.
 *
 * Startup time: < 30 seconds
 * Cleanup: Automatic on test completion
 *
 * @example
 * ```javascript
 * import { CleanroomStack } from './testcontainer-stack.mjs';
 *
 * const stack = new CleanroomStack();
 * await stack.start();
 *
 * // Run tests
 * const sidecar = stack.getSidecar();
 * const jaeger = stack.getJaeger();
 *
 * await stack.cleanup();
 * ```
 */

import {
  GenericContainer,
  Network,
  Wait
} from 'testcontainers';
import { join } from 'path';
import { readFileSync } from 'fs';

/**
 * Cleanroom stack configuration
 */
export const CLEANROOM_CONFIG = {
  // Network configuration
  NETWORK_NAME: 'cleanroom-network',
  SUBNET: '172.30.0.0/16',
  GATEWAY: '172.30.0.1',

  // Static IP assignments
  IPS: {
    OTEL_COLLECTOR: '172.30.0.10',
    JAEGER: '172.30.0.11',
    SIDECAR: '172.30.0.20',
    POSTGRES: '172.30.0.30'
  },

  // Container ports
  PORTS: {
    OTEL_GRPC: 4317,
    OTEL_HTTP: 4318,
    OTEL_METRICS: 8888,
    OTEL_HEALTH: 13133,
    JAEGER_UI: 16686,
    JAEGER_GRPC: 14250,
    JAEGER_HTTP: 14268,
    JAEGER_HEALTH: 14269,
    SIDECAR_GRPC: 50051,
    SIDECAR_METRICS: 9464,
    POSTGRES: 5432
  },

  // Startup timeouts (milliseconds)
  TIMEOUTS: {
    POSTGRES: 15000,
    OTEL_COLLECTOR: 10000,
    JAEGER: 15000,
    SIDECAR: 20000,
    TOTAL: 30000
  }
};

/**
 * Cleanroom testcontainer stack manager
 */
export class CleanroomStack {
  constructor() {
    this.network = null;
    this.containers = new Map();
    this.startTime = null;
  }

  /**
   * Start the complete cleanroom stack
   * @returns {Promise<void>}
   */
  async start() {
    console.log('[Cleanroom] Starting cleanroom stack...');
    this.startTime = Date.now();

    try {
      // Step 1: Create network
      await this._createNetwork();

      // Step 2: Start services in dependency order
      await this._startPostgres();
      await this._startOtelCollector();
      await this._startJaeger();
      await this._startSidecar();

      const duration = Date.now() - this.startTime;
      console.log(`[Cleanroom] Stack started successfully in ${duration}ms`);

      if (duration > CLEANROOM_CONFIG.TIMEOUTS.TOTAL) {
        console.warn(`[Cleanroom] WARNING: Startup took ${duration}ms (target: ${CLEANROOM_CONFIG.TIMEOUTS.TOTAL}ms)`);
      }
    } catch (error) {
      console.error('[Cleanroom] Failed to start stack:', error);
      await this.cleanup();
      throw error;
    }
  }

  /**
   * Create Docker network for container communication
   * @private
   */
  async _createNetwork() {
    console.log('[Cleanroom] Creating network...');
    this.network = await new Network()
      .withName(CLEANROOM_CONFIG.NETWORK_NAME)
      .withDriver('bridge')
      .start();
  }

  /**
   * Start PostgreSQL container
   * @private
   */
  async _startPostgres() {
    console.log('[Cleanroom] Starting PostgreSQL...');

    const container = await new GenericContainer('postgres:16-alpine')
      .withName('cleanroom-postgres')
      .withNetwork(this.network)
      .withNetworkAliases('postgres')
      .withExposedPorts(CLEANROOM_CONFIG.PORTS.POSTGRES)
      .withEnvironment({
        POSTGRES_DB: 'unrdf_cleanroom',
        POSTGRES_USER: 'cleanroom_user',
        POSTGRES_PASSWORD: 'cleanroom_pass',
        POSTGRES_INITDB_ARGS: '--encoding=UTF8 --locale=C'
      })
      .withWaitStrategy(
        Wait.forHealthCheck()
          .withStartupTimeout(CLEANROOM_CONFIG.TIMEOUTS.POSTGRES)
      )
      .withHealthCheck({
        test: ['CMD-SHELL', 'pg_isready -U cleanroom_user -d unrdf_cleanroom'],
        interval: 5000,
        timeout: 3000,
        retries: 5,
        startPeriod: 10000
      })
      .start();

    this.containers.set('postgres', container);
    console.log('[Cleanroom] PostgreSQL started');
  }

  /**
   * Start OTEL Collector container
   * @private
   */
  async _startOtelCollector() {
    console.log('[Cleanroom] Starting OTEL Collector...');

    // Read OTEL config file
    const configPath = join(process.cwd(), 'test/e2e/cleanroom/otel-collector-config.yaml');
    const configContent = readFileSync(configPath, 'utf8');

    const container = await new GenericContainer('otel/opentelemetry-collector-contrib:0.91.0')
      .withName('cleanroom-otel-collector')
      .withNetwork(this.network)
      .withNetworkAliases('otel-collector')
      .withExposedPorts(
        CLEANROOM_CONFIG.PORTS.OTEL_GRPC,
        CLEANROOM_CONFIG.PORTS.OTEL_HTTP,
        CLEANROOM_CONFIG.PORTS.OTEL_METRICS,
        CLEANROOM_CONFIG.PORTS.OTEL_HEALTH
      )
      .withCopyContentToContainer([{
        content: configContent,
        target: '/etc/otel/config.yaml'
      }])
      .withCommand(['--config=/etc/otel/config.yaml'])
      .withEnvironment({
        OTEL_LOG_LEVEL: 'debug'
      })
      .withWaitStrategy(
        Wait.forHttp('/', CLEANROOM_CONFIG.PORTS.OTEL_HEALTH)
          .withStartupTimeout(CLEANROOM_CONFIG.TIMEOUTS.OTEL_COLLECTOR)
      )
      .start();

    this.containers.set('otel-collector', container);
    console.log('[Cleanroom] OTEL Collector started');
  }

  /**
   * Start Jaeger container
   * @private
   */
  async _startJaeger() {
    console.log('[Cleanroom] Starting Jaeger...');

    const container = await new GenericContainer('jaegertracing/all-in-one:1.52')
      .withName('cleanroom-jaeger')
      .withNetwork(this.network)
      .withNetworkAliases('jaeger')
      .withExposedPorts(
        CLEANROOM_CONFIG.PORTS.JAEGER_UI,
        CLEANROOM_CONFIG.PORTS.JAEGER_GRPC,
        CLEANROOM_CONFIG.PORTS.JAEGER_HTTP,
        CLEANROOM_CONFIG.PORTS.JAEGER_HEALTH
      )
      .withEnvironment({
        COLLECTOR_OTLP_ENABLED: 'true',
        SPAN_STORAGE_TYPE: 'memory',
        MEMORY_MAX_TRACES: '10000'
      })
      .withWaitStrategy(
        Wait.forHttp('/', CLEANROOM_CONFIG.PORTS.JAEGER_HEALTH)
          .withStartupTimeout(CLEANROOM_CONFIG.TIMEOUTS.JAEGER)
      )
      .start();

    this.containers.set('jaeger', container);
    console.log('[Cleanroom] Jaeger started');
    console.log(`[Cleanroom] Jaeger UI: http://localhost:${container.getMappedPort(CLEANROOM_CONFIG.PORTS.JAEGER_UI)}`);
  }

  /**
   * Start KGC Sidecar container
   * @private
   */
  async _startSidecar() {
    console.log('[Cleanroom] Starting KGC Sidecar...');

    const otelCollector = this.containers.get('otel-collector');
    const otelEndpoint = `http://otel-collector:${CLEANROOM_CONFIG.PORTS.OTEL_GRPC}`;

    const container = await GenericContainer.fromDockerfile(
      join(process.cwd()),
      'test/e2e/cleanroom/sidecar.Dockerfile'
    )
      .build()
      .then(image =>
        image
          .withName('cleanroom-kgc-sidecar')
          .withNetwork(this.network)
          .withNetworkAliases('sidecar')
          .withExposedPorts(
            CLEANROOM_CONFIG.PORTS.SIDECAR_GRPC,
            CLEANROOM_CONFIG.PORTS.SIDECAR_METRICS
          )
          .withEnvironment({
            // OpenTelemetry configuration
            OTEL_EXPORTER_OTLP_ENDPOINT: otelEndpoint,
            OTEL_EXPORTER_OTLP_PROTOCOL: 'grpc',
            OTEL_SERVICE_NAME: 'kgc-sidecar',
            OTEL_RESOURCE_ATTRIBUTES: 'service.version=2.0.0,deployment.environment=cleanroom',
            OTEL_TRACES_EXPORTER: 'otlp',
            OTEL_METRICS_EXPORTER: 'otlp',
            OTEL_LOGS_EXPORTER: 'otlp',
            OTEL_LOG_LEVEL: 'debug',

            // Sidecar configuration
            GRPC_PORT: String(CLEANROOM_CONFIG.PORTS.SIDECAR_GRPC),
            POSTGRES_URL: 'postgresql://cleanroom_user:cleanroom_pass@postgres:5432/unrdf_cleanroom',
            NODE_ENV: 'test',
            LOG_LEVEL: 'debug'
          })
          .withWaitStrategy(
            Wait.forLogMessage(/gRPC server listening/i)
              .withStartupTimeout(CLEANROOM_CONFIG.TIMEOUTS.SIDECAR)
          )
          .start()
      );

    this.containers.set('sidecar', container);
    console.log('[Cleanroom] KGC Sidecar started');
  }

  /**
   * Get PostgreSQL container
   * @returns {StartedTestContainer}
   */
  getPostgres() {
    return this.containers.get('postgres');
  }

  /**
   * Get OTEL Collector container
   * @returns {StartedTestContainer}
   */
  getOtelCollector() {
    return this.containers.get('otel-collector');
  }

  /**
   * Get Jaeger container
   * @returns {StartedTestContainer}
   */
  getJaeger() {
    return this.containers.get('jaeger');
  }

  /**
   * Get KGC Sidecar container
   * @returns {StartedTestContainer}
   */
  getSidecar() {
    return this.containers.get('sidecar');
  }

  /**
   * Get PostgreSQL connection string
   * @returns {string}
   */
  getPostgresUrl() {
    const postgres = this.getPostgres();
    const host = postgres.getHost();
    const port = postgres.getMappedPort(CLEANROOM_CONFIG.PORTS.POSTGRES);
    return `postgresql://cleanroom_user:cleanroom_pass@${host}:${port}/unrdf_cleanroom`;
  }

  /**
   * Get Jaeger UI URL
   * @returns {string}
   */
  getJaegerUrl() {
    const jaeger = this.getJaeger();
    const host = jaeger.getHost();
    const port = jaeger.getMappedPort(CLEANROOM_CONFIG.PORTS.JAEGER_UI);
    return `http://${host}:${port}`;
  }

  /**
   * Get OTEL Collector endpoint
   * @returns {string}
   */
  getOtelEndpoint() {
    const otel = this.getOtelCollector();
    const host = otel.getHost();
    const port = otel.getMappedPort(CLEANROOM_CONFIG.PORTS.OTEL_GRPC);
    return `http://${host}:${port}`;
  }

  /**
   * Get Sidecar gRPC endpoint
   * @returns {string}
   */
  getSidecarEndpoint() {
    const sidecar = this.getSidecar();
    const host = sidecar.getHost();
    const port = sidecar.getMappedPort(CLEANROOM_CONFIG.PORTS.SIDECAR_GRPC);
    return `${host}:${port}`;
  }

  /**
   * Clean up all containers and network
   * @returns {Promise<void>}
   */
  async cleanup() {
    console.log('[Cleanroom] Cleaning up stack...');

    // Stop containers in reverse order
    const containerNames = ['sidecar', 'jaeger', 'otel-collector', 'postgres'];

    for (const name of containerNames) {
      const container = this.containers.get(name);
      if (container) {
        try {
          console.log(`[Cleanroom] Stopping ${name}...`);
          await container.stop();
          this.containers.delete(name);
        } catch (error) {
          console.warn(`[Cleanroom] Error stopping ${name}:`, error.message);
        }
      }
    }

    // Remove network
    if (this.network) {
      try {
        await this.network.stop();
        this.network = null;
      } catch (error) {
        console.warn('[Cleanroom] Error removing network:', error.message);
      }
    }

    console.log('[Cleanroom] Cleanup complete');
  }

  /**
   * Get startup duration in milliseconds
   * @returns {number}
   */
  getStartupDuration() {
    return this.startTime ? Date.now() - this.startTime : 0;
  }

  /**
   * Check if all services are healthy
   * @returns {Promise<boolean>}
   */
  async isHealthy() {
    try {
      const postgres = this.getPostgres();
      const otel = this.getOtelCollector();
      const jaeger = this.getJaeger();
      const sidecar = this.getSidecar();

      return postgres && otel && jaeger && sidecar;
    } catch {
      return false;
    }
  }
}

export default CleanroomStack;
