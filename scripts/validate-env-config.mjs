#!/usr/bin/env node
/**
 * @file Environment Configuration Validation Script
 * @description Validates environment configuration files for UNRDF v6
 * Ensures all required variables are present and properly formatted
 */

import { readFileSync, existsSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import { z } from 'zod';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = resolve(__dirname, '..');

// Validation schemas
const PortSchema = z.number().int().min(1).max(65535);
const PositiveIntSchema = z.number().int().positive();
const URLSchema = z.string().url();
const BooleanSchema = z.boolean();

const ConfigSchema = z.object({
  // Application
  NODE_ENV: z.enum(['production', 'staging', 'development']),
  APP_VERSION: z.string().regex(/^\d+\.\d+\.\d+(-\w+\.\d+)?$/),
  LOG_LEVEL: z.enum(['error', 'warn', 'info', 'debug', 'trace']),

  // Server
  API_PORT: PortSchema,
  HEALTH_CHECK_PORT: PortSchema,
  PROMETHEUS_PORT: PortSchema,
  HOST: z.string(),
  MAX_REQUEST_SIZE: z.string(),
  REQUEST_TIMEOUT: PositiveIntSchema,

  // Database
  RDF_STORE_BACKEND: z.enum(['oxigraph', 'memory', 'file']),
  DB_PATH: z.string().optional(),
  DB_POOL_SIZE: PositiveIntSchema,
  DB_QUERY_TIMEOUT: PositiveIntSchema,

  // SPARQL
  SPARQL_ENABLED: BooleanSchema,
  SPARQL_ENDPOINT: z.string(),
  SPARQL_MAX_COMPLEXITY: PositiveIntSchema,
  SPARQL_RESULT_LIMIT: PositiveIntSchema,

  // Federation
  FEDERATION_ENABLED: BooleanSchema,
  FEDERATION_MODE: z.enum(['raft', 'gossip', 'none']),

  // KGC
  KGC_RECEIPTS_ENABLED: BooleanSchema,
  KGC_VERIFY_RECEIPTS: BooleanSchema,
  KGC_RECEIPT_BACKEND: z.enum(['memory', 'file', 's3']),

  // Streaming
  STREAMING_ENABLED: BooleanSchema,
  STREAM_BUFFER_SIZE: PositiveIntSchema,

  // Hooks
  HOOKS_ENABLED: BooleanSchema,
  HOOK_TIMEOUT: PositiveIntSchema,
  HOOK_CONCURRENCY: PositiveIntSchema,

  // Caching
  CACHE_ENABLED: BooleanSchema,
  CACHE_BACKEND: z.enum(['memory', 'redis']),
  CACHE_TTL: PositiveIntSchema,
  CACHE_MAX_SIZE: PositiveIntSchema,

  // Security
  AUTH_ENABLED: BooleanSchema,

  // OpenTelemetry
  OTEL_ENABLED: BooleanSchema,
  OTEL_SERVICE_NAME: z.string(),
  OTEL_TRACE_SAMPLE_RATE: z.number().min(0).max(1),
  OTEL_METRICS_ENABLED: BooleanSchema,
});

/**
 * Parse environment file
 * @param {string} filePath - Path to env file
 * @returns {Object} Parsed environment variables
 */
function parseEnvFile(filePath) {
  if (!existsSync(filePath)) {
    throw new Error(`Environment file not found: ${filePath}`);
  }

  const content = readFileSync(filePath, 'utf-8');
  const env = {};

  content.split('\n').forEach((line) => {
    line = line.trim();

    // Skip comments and empty lines
    if (!line || line.startsWith('#')) {
      return;
    }

    const match = line.match(/^([^=]+)=(.*)$/);
    if (match) {
      const key = match[1].trim();
      let value = match[2].trim();

      // Remove quotes if present
      if ((value.startsWith('"') && value.endsWith('"')) ||
          (value.startsWith("'") && value.endsWith("'"))) {
        value = value.slice(1, -1);
      }

      env[key] = value;
    }
  });

  return env;
}

/**
 * Convert string values to appropriate types
 * @param {Object} env - Environment variables
 * @returns {Object} Typed environment variables
 */
function convertTypes(env) {
  const typed = { ...env };

  // Convert numbers
  const numberFields = [
    'API_PORT', 'HEALTH_CHECK_PORT', 'PROMETHEUS_PORT',
    'REQUEST_TIMEOUT', 'DB_POOL_SIZE', 'DB_QUERY_TIMEOUT',
    'SPARQL_MAX_COMPLEXITY', 'SPARQL_RESULT_LIMIT',
    'STREAM_BUFFER_SIZE', 'STREAM_RETENTION',
    'HOOK_TIMEOUT', 'HOOK_CONCURRENCY',
    'CACHE_TTL', 'CACHE_MAX_SIZE',
    'OTEL_METRICS_INTERVAL'
  ];

  numberFields.forEach((field) => {
    if (typed[field]) {
      typed[field] = parseInt(typed[field], 10);
    }
  });

  // Convert floats
  if (typed.OTEL_TRACE_SAMPLE_RATE) {
    typed.OTEL_TRACE_SAMPLE_RATE = parseFloat(typed.OTEL_TRACE_SAMPLE_RATE);
  }

  // Convert booleans
  const booleanFields = [
    'SPARQL_ENABLED', 'FEDERATION_ENABLED',
    'KGC_RECEIPTS_ENABLED', 'KGC_VERIFY_RECEIPTS',
    'STREAMING_ENABLED', 'HOOKS_ENABLED',
    'CACHE_ENABLED', 'AUTH_ENABLED',
    'OTEL_ENABLED', 'OTEL_METRICS_ENABLED',
    'BACKUP_ENABLED', 'EXPERIMENTAL_FEATURES',
    'DEBUG_MODE', 'PROFILING_ENABLED'
  ];

  booleanFields.forEach((field) => {
    if (typed[field]) {
      typed[field] = typed[field] === 'true' || typed[field] === '1';
    }
  });

  return typed;
}

/**
 * Validate configuration
 * @param {string} envFile - Path to environment file
 * @returns {Object} Validation result
 */
function validateConfig(envFile) {
  console.log(`\nValidating ${envFile}...`);

  try {
    const env = parseEnvFile(envFile);
    const typed = convertTypes(env);

    // Validate with Zod
    const result = ConfigSchema.safeParse(typed);

    if (!result.success) {
      console.error('❌ Validation failed:');
      result.error.issues.forEach((issue) => {
        console.error(`  - ${issue.path.join('.')}: ${issue.message}`);
      });
      return { valid: false, errors: result.error.issues };
    }

    console.log('✅ Configuration valid');

    // Additional warnings
    const warnings = [];

    if (typed.NODE_ENV === 'production') {
      if (!env.JWT_SECRET) {
        warnings.push('JWT_SECRET not set (authentication will fail)');
      }
      if (!env.KGC_SIGNING_KEY) {
        warnings.push('KGC_SIGNING_KEY not set (receipts will not be signed)');
      }
      if (!env.GRAFANA_ADMIN_PASSWORD) {
        warnings.push('GRAFANA_ADMIN_PASSWORD not set (using default)');
      }
      if (typed.CORS_ORIGINS === '*') {
        warnings.push('CORS_ORIGINS set to * (insecure for production)');
      }
      if (typed.DEBUG_MODE) {
        warnings.push('DEBUG_MODE enabled in production');
      }
    }

    if (warnings.length > 0) {
      console.log('\n⚠️  Warnings:');
      warnings.forEach((warning) => {
        console.log(`  - ${warning}`);
      });
    }

    return { valid: true, warnings };

  } catch (error) {
    console.error(`❌ Error: ${error.message}`);
    return { valid: false, errors: [error.message] };
  }
}

/**
 * Main validation
 */
function main() {
  console.log('='.repeat(60));
  console.log('UNRDF v6 Environment Configuration Validation');
  console.log('='.repeat(60));

  const envFiles = [
    '.env.example',
    '.env.staging',
  ];

  // Add .env.production if it exists
  if (existsSync(resolve(rootDir, '.env.production'))) {
    envFiles.push('.env.production');
  }

  let allValid = true;

  envFiles.forEach((file) => {
    const filePath = resolve(rootDir, file);
    const result = validateConfig(filePath);

    if (!result.valid) {
      allValid = false;
    }
  });

  console.log('\n' + '='.repeat(60));

  if (allValid) {
    console.log('✅ All configuration files are valid');
    process.exit(0);
  } else {
    console.log('❌ Configuration validation failed');
    process.exit(1);
  }
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { validateConfig, parseEnvFile, convertTypes };
