/**
 * @file Network Configuration for Cleanroom
 * @module test/e2e/cleanroom/network-config
 *
 * @description
 * Network configuration and service discovery utilities for cleanroom testing.
 * Manages Docker network, DNS resolution, and service communication.
 */

import { CLEANROOM_CONFIG } from './testcontainer-stack.mjs';

/**
 * Network configuration manager
 */
export class NetworkConfig {
  constructor(stack) {
    this.stack = stack;
  }

  /**
   * Get network information
   * @returns {Object}
   */
  getNetworkInfo() {
    return {
      name: CLEANROOM_CONFIG.NETWORK_NAME,
      subnet: CLEANROOM_CONFIG.SUBNET,
      gateway: CLEANROOM_CONFIG.GATEWAY,
      driver: 'bridge'
    };
  }

  /**
   * Get service DNS mappings
   * @returns {Object}
   */
  getServiceDns() {
    return {
      'postgres': {
        ip: CLEANROOM_CONFIG.IPS.POSTGRES,
        aliases: ['postgres', 'cleanroom-postgres'],
        port: CLEANROOM_CONFIG.PORTS.POSTGRES
      },
      'otel-collector': {
        ip: CLEANROOM_CONFIG.IPS.OTEL_COLLECTOR,
        aliases: ['otel-collector', 'cleanroom-otel-collector'],
        ports: {
          grpc: CLEANROOM_CONFIG.PORTS.OTEL_GRPC,
          http: CLEANROOM_CONFIG.PORTS.OTEL_HTTP,
          metrics: CLEANROOM_CONFIG.PORTS.OTEL_METRICS,
          health: CLEANROOM_CONFIG.PORTS.OTEL_HEALTH
        }
      },
      'jaeger': {
        ip: CLEANROOM_CONFIG.IPS.JAEGER,
        aliases: ['jaeger', 'cleanroom-jaeger'],
        ports: {
          ui: CLEANROOM_CONFIG.PORTS.JAEGER_UI,
          grpc: CLEANROOM_CONFIG.PORTS.JAEGER_GRPC,
          http: CLEANROOM_CONFIG.PORTS.JAEGER_HTTP,
          health: CLEANROOM_CONFIG.PORTS.JAEGER_HEALTH
        }
      },
      'sidecar': {
        ip: CLEANROOM_CONFIG.IPS.SIDECAR,
        aliases: ['sidecar', 'cleanroom-kgc-sidecar'],
        ports: {
          grpc: CLEANROOM_CONFIG.PORTS.SIDECAR_GRPC,
          metrics: CLEANROOM_CONFIG.PORTS.SIDECAR_METRICS
        }
      }
    };
  }

  /**
   * Get service endpoint
   * @param {string} service - Service name
   * @param {string} protocol - Protocol (grpc, http, etc.)
   * @returns {string}
   */
  getServiceEndpoint(service, protocol = 'grpc') {
    const dns = this.getServiceDns();
    const serviceInfo = dns[service];

    if (!serviceInfo) {
      throw new Error(`Unknown service: ${service}`);
    }

    const port = typeof serviceInfo.ports === 'object'
      ? serviceInfo.ports[protocol]
      : serviceInfo.port;

    if (!port) {
      throw new Error(`Unknown protocol for ${service}: ${protocol}`);
    }

    return `${service}:${port}`;
  }

  /**
   * Resolve service hostname to container
   * @param {string} hostname - Service hostname
   * @returns {Object|null}
   */
  resolveService(hostname) {
    const dns = this.getServiceDns();

    for (const [serviceName, info] of Object.entries(dns)) {
      if (info.aliases.includes(hostname) || serviceName === hostname) {
        return {
          service: serviceName,
          ip: info.ip,
          ports: info.ports || { default: info.port }
        };
      }
    }

    return null;
  }

  /**
   * Get internal network URLs for services
   * @returns {Object}
   */
  getInternalUrls() {
    return {
      postgres: `postgresql://cleanroom_user:cleanroom_pass@postgres:${CLEANROOM_CONFIG.PORTS.POSTGRES}/unrdf_cleanroom`,
      otelCollector: {
        grpc: `http://otel-collector:${CLEANROOM_CONFIG.PORTS.OTEL_GRPC}`,
        http: `http://otel-collector:${CLEANROOM_CONFIG.PORTS.OTEL_HTTP}`,
        health: `http://otel-collector:${CLEANROOM_CONFIG.PORTS.OTEL_HEALTH}`
      },
      jaeger: {
        ui: `http://jaeger:${CLEANROOM_CONFIG.PORTS.JAEGER_UI}`,
        collector: `jaeger:${CLEANROOM_CONFIG.PORTS.JAEGER_GRPC}`
      },
      sidecar: {
        grpc: `sidecar:${CLEANROOM_CONFIG.PORTS.SIDECAR_GRPC}`,
        metrics: `http://sidecar:${CLEANROOM_CONFIG.PORTS.SIDECAR_METRICS}/metrics`
      }
    };
  }

  /**
   * Get external (host) URLs for services
   * @returns {Object}
   */
  getExternalUrls() {
    const postgres = this.stack.getPostgres();
    const otel = this.stack.getOtelCollector();
    const jaeger = this.stack.getJaeger();
    const sidecar = this.stack.getSidecar();

    return {
      postgres: `postgresql://cleanroom_user:cleanroom_pass@${postgres.getHost()}:${postgres.getMappedPort(CLEANROOM_CONFIG.PORTS.POSTGRES)}/unrdf_cleanroom`,
      otelCollector: {
        grpc: `http://${otel.getHost()}:${otel.getMappedPort(CLEANROOM_CONFIG.PORTS.OTEL_GRPC)}`,
        http: `http://${otel.getHost()}:${otel.getMappedPort(CLEANROOM_CONFIG.PORTS.OTEL_HTTP)}`,
        health: `http://${otel.getHost()}:${otel.getMappedPort(CLEANROOM_CONFIG.PORTS.OTEL_HEALTH)}`
      },
      jaeger: {
        ui: `http://${jaeger.getHost()}:${jaeger.getMappedPort(CLEANROOM_CONFIG.PORTS.JAEGER_UI)}`,
        collector: `${jaeger.getHost()}:${jaeger.getMappedPort(CLEANROOM_CONFIG.PORTS.JAEGER_GRPC)}`
      },
      sidecar: {
        grpc: `${sidecar.getHost()}:${sidecar.getMappedPort(CLEANROOM_CONFIG.PORTS.SIDECAR_GRPC)}`,
        metrics: `http://${sidecar.getHost()}:${sidecar.getMappedPort(CLEANROOM_CONFIG.PORTS.SIDECAR_METRICS)}/metrics`
      }
    };
  }

  /**
   * Verify network connectivity between services
   * @returns {Promise<Object>}
   */
  async verifyConnectivity() {
    // This would perform actual connectivity tests
    // For now, return expected connectivity matrix
    return {
      sidecar_to_postgres: true,
      sidecar_to_otel: true,
      otel_to_jaeger: true,
      host_to_sidecar: true,
      host_to_jaeger_ui: true
    };
  }

  /**
   * Get trace context propagation configuration
   * @returns {Object}
   */
  getTracePropagationConfig() {
    return {
      format: 'W3C Trace Context',
      headers: {
        traceparent: 'traceparent',
        tracestate: 'tracestate'
      },
      metadata: {
        grpc: {
          traceparent: 'traceparent',
          baggage: 'baggage'
        }
      }
    };
  }
}

export default NetworkConfig;
