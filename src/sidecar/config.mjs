/**
 * @file Sidecar client configuration management
 * @module sidecar/config
 *
 * @description
 * Multi-environment configuration management for KGC sidecar client.
 * Supports context-based configuration similar to kubeconfig.
 */

import { z } from 'zod';
import { readFileSync, existsSync } from 'fs';
import { homedir } from 'os';
import { join } from 'path';

/**
 * TLS configuration schema
 */
const TLSConfigSchema = z.object({
  enabled: z.boolean().default(false),
  ca: z.string().optional(),
  cert: z.string().optional(),
  key: z.string().optional(),
  insecure: z.boolean().default(false)
});

/**
 * Endpoint configuration schema
 */
const EndpointConfigSchema = z.object({
  address: z.string().min(1),
  port: z.number().int().min(1).max(65535).default(50051),
  tls: TLSConfigSchema.optional(),
  metadata: z.record(z.string()).optional()
});

/**
 * Context configuration schema
 */
const ContextConfigSchema = z.object({
  name: z.string().min(1),
  endpoint: EndpointConfigSchema,
  namespace: z.string().optional(),
  timeout: z.number().int().positive().default(5000),
  maxRetries: z.number().int().min(0).max(10).default(3),
  circuitBreaker: z.object({
    enabled: z.boolean().default(true),
    threshold: z.number().int().positive().default(5),
    resetTimeout: z.number().int().positive().default(30000),
    halfOpenRequests: z.number().int().positive().default(3)
  }).optional()
});

/**
 * Sidecar configuration schema
 */
const SidecarConfigSchema = z.object({
  currentContext: z.string().min(1).optional(),
  contexts: z.array(ContextConfigSchema).min(1),
  defaults: z.object({
    timeout: z.number().int().positive().default(5000),
    maxRetries: z.number().int().min(0).max(10).default(3),
    keepAlive: z.boolean().default(true),
    keepAliveTime: z.number().int().positive().default(10000),
    keepAliveTimeout: z.number().int().positive().default(5000)
  }).optional()
});

/**
 * Configuration manager for KGC sidecar client
 */
export class SidecarConfig {
  /**
   * Create a new sidecar configuration manager
   * @param {Object} [config] - Configuration object
   */
  constructor(config = {}) {
    this.config = SidecarConfigSchema.parse(config);
    this.currentContext = this._resolveCurrentContext();
  }

  /**
   * Load configuration from file
   * @param {string} [path] - Config file path (defaults to ~/.kgc/config.json)
   * @returns {SidecarConfig} Configuration manager
   */
  static fromFile(path) {
    const configPath = path || join(homedir(), '.kgc', 'config.json');

    if (!existsSync(configPath)) {
      throw new Error(`Configuration file not found: ${configPath}`);
    }

    try {
      const fileContent = readFileSync(configPath, 'utf-8');
      const config = JSON.parse(fileContent);
      return new SidecarConfig(config);
    } catch (error) {
      throw new Error(`Failed to load configuration: ${error.message}`);
    }
  }

  /**
   * Create configuration from environment variables
   * @returns {SidecarConfig} Configuration manager
   */
  static fromEnv() {
    const address = process.env.KGC_SIDECAR_ADDRESS || 'localhost:50051';
    const [host, portStr] = address.split(':');
    const port = portStr ? parseInt(portStr, 10) : 50051;

    const config = {
      currentContext: process.env.KGC_SIDECAR_CONTEXT || 'default',
      contexts: [{
        name: process.env.KGC_SIDECAR_CONTEXT || 'default',
        endpoint: {
          address: host,
          port,
          tls: {
            enabled: process.env.KGC_SIDECAR_TLS_ENABLED === 'true',
            ca: process.env.KGC_SIDECAR_TLS_CA,
            cert: process.env.KGC_SIDECAR_TLS_CERT,
            key: process.env.KGC_SIDECAR_TLS_KEY,
            insecure: process.env.KGC_SIDECAR_TLS_INSECURE === 'true'
          }
        },
        namespace: process.env.KGC_SIDECAR_NAMESPACE,
        timeout: parseInt(process.env.KGC_SIDECAR_TIMEOUT || '5000', 10),
        maxRetries: parseInt(process.env.KGC_SIDECAR_MAX_RETRIES || '3', 10)
      }]
    };

    return new SidecarConfig(config);
  }

  /**
   * Create default configuration for local development
   * @returns {SidecarConfig} Configuration manager
   */
  static defaultConfig() {
    return new SidecarConfig({
      currentContext: 'local',
      contexts: [{
        name: 'local',
        endpoint: {
          address: 'localhost',
          port: 50051
        },
        timeout: 5000,
        maxRetries: 3
      }]
    });
  }

  /**
   * Resolve current context
   * @private
   */
  _resolveCurrentContext() {
    const contextName = this.config.currentContext || this.config.contexts[0].name;
    const context = this.config.contexts.find(c => c.name === contextName);

    if (!context) {
      throw new Error(`Context not found: ${contextName}`);
    }

    return context;
  }

  /**
   * Get current context
   * @returns {Object} Current context configuration
   */
  getContext() {
    return this.currentContext;
  }

  /**
   * Switch to a different context
   * @param {string} contextName - Context name
   */
  useContext(contextName) {
    const context = this.config.contexts.find(c => c.name === contextName);

    if (!context) {
      throw new Error(`Context not found: ${contextName}`);
    }

    this.currentContext = context;
    this.config.currentContext = contextName;
  }

  /**
   * Get endpoint address
   * @returns {string} Full endpoint address
   */
  getAddress() {
    const { endpoint } = this.currentContext;
    return `${endpoint.address}:${endpoint.port}`;
  }

  /**
   * Get TLS credentials
   * @returns {Object|null} TLS credentials or null if TLS disabled
   */
  getTLSCredentials() {
    const { tls } = this.currentContext.endpoint;

    if (!tls || !tls.enabled) {
      return null;
    }

    return {
      ca: tls.ca ? readFileSync(tls.ca) : undefined,
      cert: tls.cert ? readFileSync(tls.cert) : undefined,
      key: tls.key ? readFileSync(tls.key) : undefined,
      insecure: tls.insecure
    };
  }

  /**
   * Get connection options
   * @returns {Object} gRPC connection options
   */
  getConnectionOptions() {
    const defaults = this.config.defaults || {};

    return {
      'grpc.keepalive_time_ms': defaults.keepAliveTime || 10000,
      'grpc.keepalive_timeout_ms': defaults.keepAliveTimeout || 5000,
      'grpc.keepalive_permit_without_calls': defaults.keepAlive ? 1 : 0,
      'grpc.http2.min_time_between_pings_ms': 10000,
      'grpc.http2.max_pings_without_data': 0,
      'grpc.max_receive_message_length': 10 * 1024 * 1024, // 10MB
      'grpc.max_send_message_length': 10 * 1024 * 1024
    };
  }

  /**
   * Get timeout
   * @returns {number} Timeout in milliseconds
   */
  getTimeout() {
    return this.currentContext.timeout || this.config.defaults?.timeout || 5000;
  }

  /**
   * Get max retries
   * @returns {number} Maximum retry attempts
   */
  getMaxRetries() {
    return this.currentContext.maxRetries || this.config.defaults?.maxRetries || 3;
  }

  /**
   * Get circuit breaker configuration
   * @returns {Object} Circuit breaker configuration
   */
  getCircuitBreakerConfig() {
    return this.currentContext.circuitBreaker || {
      enabled: true,
      threshold: 5,
      resetTimeout: 30000,
      halfOpenRequests: 3
    };
  }

  /**
   * Export configuration to JSON
   * @returns {Object} Configuration object
   */
  toJSON() {
    return this.config;
  }
}

/**
 * Create sidecar configuration
 * @param {Object} [config] - Configuration object
 * @returns {SidecarConfig} Configuration manager
 */
export function createSidecarConfig(config) {
  if (config) {
    return new SidecarConfig(config);
  }

  // Try to load from file first
  try {
    return SidecarConfig.fromFile();
  } catch {
    // Fall back to environment variables
    try {
      return SidecarConfig.fromEnv();
    } catch {
      // Use default configuration
      return SidecarConfig.defaultConfig();
    }
  }
}

export default SidecarConfig;
