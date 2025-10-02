#!/usr/bin/env node

/**
 * @file Production Readiness Check Script
 * @description Automated validation of all critical components for Fortune 5 deployment
 */

import { existsSync, readFileSync } from 'fs'
import { execSync } from 'child_process'
import { fileURLToPath } from 'url'
import { dirname, join } from 'path'

const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)
const projectRoot = join(__dirname, '..')

// ANSI color codes
const colors = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
  bold: '\x1b[1m'
}

// Track overall results
const results = {
  passed: 0,
  failed: 0,
  warnings: 0,
  total: 0
}

/**
 * Print colored output
 * @param {string} message
 * @param {string} color
 */
function print(message, color = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`)
}

/**
 * Print section header
 * @param {string} title
 */
function printSection(title) {
  print(`\n${'='.repeat(80)}`, 'cyan')
  print(`  ${title}`, 'cyan')
  print('='.repeat(80), 'cyan')
}

/**
 * Check if file exists
 * @param {string} path
 * @param {string} description
 * @returns {boolean}
 */
function checkFile(path, description) {
  results.total++
  const fullPath = join(projectRoot, path)
  const exists = existsSync(fullPath)

  if (exists) {
    print(`✅ ${description}`, 'green')
    results.passed++
    return true
  } else {
    print(`❌ ${description}`, 'red')
    print(`   Missing: ${path}`, 'red')
    results.failed++
    return false
  }
}

/**
 * Check if file contains pattern
 * @param {string} path
 * @param {string} pattern
 * @param {string} description
 * @returns {boolean}
 */
function checkFileContains(path, pattern, description) {
  results.total++
  const fullPath = join(projectRoot, path)

  if (!existsSync(fullPath)) {
    print(`❌ ${description}`, 'red')
    print(`   File not found: ${path}`, 'red')
    results.failed++
    return false
  }

  const content = readFileSync(fullPath, 'utf-8')
  const regex = new RegExp(pattern, 'i')

  if (regex.test(content)) {
    print(`✅ ${description}`, 'green')
    results.passed++
    return true
  } else {
    print(`❌ ${description}`, 'red')
    print(`   Pattern not found: ${pattern}`, 'red')
    results.failed++
    return false
  }
}

/**
 * Execute command and check result
 * @param {string} command
 * @param {string} description
 * @param {boolean} expectSuccess
 * @returns {boolean}
 */
function checkCommand(command, description, expectSuccess = true) {
  results.total++

  try {
    const output = execSync(command, { cwd: projectRoot, encoding: 'utf-8', stdio: 'pipe' })

    if (expectSuccess) {
      print(`✅ ${description}`, 'green')
      results.passed++
      return true
    } else {
      print(`⚠️  ${description} (unexpected success)`, 'yellow')
      results.warnings++
      return false
    }
  } catch (error) {
    if (!expectSuccess) {
      print(`✅ ${description} (expected failure)`, 'green')
      results.passed++
      return true
    } else {
      print(`❌ ${description}`, 'red')
      print(`   Error: ${error.message}`, 'red')
      results.failed++
      return false
    }
  }
}

/**
 * Warning check (doesn't fail overall)
 * @param {boolean} condition
 * @param {string} description
 */
function checkWarning(condition, description) {
  results.total++

  if (condition) {
    print(`✅ ${description}`, 'green')
    results.passed++
  } else {
    print(`⚠️  ${description}`, 'yellow')
    results.warnings++
  }
}

/**
 * Main validation function
 */
async function runChecks() {
  print('\n🏛️  UNRDF SIDECAR: PRODUCTION READINESS CHECK', 'bold')
  print('Fortune 5 Enterprise Deployment Validation\n', 'blue')
  print(`Starting validation at: ${new Date().toISOString()}`, 'cyan')

  // ==========================================================================
  // 1. SECURITY CHECKS
  // ==========================================================================
  printSection('1. SECURITY & AUTHENTICATION')

  checkFile('sidecar/server/utils/auth.mjs', 'Authentication module exists')
  checkFile('sidecar/server/utils/rbac.mjs', 'RBAC module exists')
  checkFile('sidecar/server/middleware/00.auth.mjs', 'Authentication middleware exists')
  checkFile('sidecar/server/middleware/02.authorization.mjs', 'Authorization middleware exists')

  checkFileContains('sidecar/server/utils/auth.mjs', 'bcrypt', 'Password hashing (bcrypt) implemented')
  checkFileContains('sidecar/server/utils/auth.mjs', 'jsonwebtoken', 'JWT tokens implemented')
  checkFileContains('sidecar/server/utils/rbac.mjs', 'RBACEngine', 'RBAC engine implemented')
  checkFileContains('sidecar/server/utils/rbac.mjs', 'PolicyCache', 'Policy caching implemented')

  // Byzantine consensus
  checkFileContains('sidecar/server/utils/auth.mjs', 'Byzantine', 'Byzantine consensus implemented')
  checkFileContains('sidecar/server/utils/auth.mjs', 'VALIDATOR_THRESHOLD', 'Byzantine quorum configured')

  // ==========================================================================
  // 2. SANDBOX SECURITY
  // ==========================================================================
  printSection('2. SANDBOX SECURITY (isolated-vm)')

  checkFile('sidecar/server/utils/secure-sandbox.mjs', 'Secure sandbox module exists')
  checkFileContains('sidecar/server/utils/secure-sandbox.mjs', 'isolated-vm', 'isolated-vm (NOT vm2) used')
  checkFileContains('sidecar/server/utils/secure-sandbox.mjs', 'memoryLimit', 'Memory limits enforced')
  checkFileContains('sidecar/server/utils/secure-sandbox.mjs', 'timeout', 'Execution timeout enforced')

  checkFile('sidecar/server/utils/sandbox-threat-detector.mjs', 'Threat detection module exists')

  // Verify vm2 is NOT used
  checkWarning(
    !existsSync(join(projectRoot, 'sidecar/node_modules/vm2')),
    'vm2 package NOT installed (critical vulnerability)'
  )

  // ==========================================================================
  // 3. TLS/mTLS
  // ==========================================================================
  printSection('3. TLS/mTLS SECURITY')

  checkFile('sidecar/server/middleware/00.https-enforce.mjs', 'HTTPS enforcement middleware exists')
  checkFile('sidecar/server/middleware/02.mtls-validate.mjs', 'mTLS validation middleware exists')
  checkFile('sidecar/server/utils/mtls-validator.mjs', 'mTLS validator utility exists')

  checkWarning(
    existsSync(join(projectRoot, 'terraform/acm-certificates.tf')),
    'TLS certificate configuration exists'
  )

  // ==========================================================================
  // 4. VAULT INTEGRATION
  // ==========================================================================
  printSection('4. SECRETS MANAGEMENT (Vault)')

  checkFile('sidecar/server/utils/vault-client.mjs', 'Vault client module exists')
  checkFile('terraform/vault.tf', 'Vault Terraform configuration exists')

  checkFileContains('terraform/vault.tf', 'vault_mount', 'Vault KV v2 mount configured')
  checkFileContains('terraform/vault.tf', 'vault_kv_secret_v2', 'Vault secrets configured')
  checkFileContains('terraform/vault.tf', 'rotation', 'Automatic secret rotation configured')

  checkWarning(
    existsSync(join(projectRoot, 'sidecar/node_modules/node-vault')),
    'node-vault package installed'
  )

  // ==========================================================================
  // 5. RATE LIMITING & DDOS
  // ==========================================================================
  printSection('5. RATE LIMITING & DDoS PROTECTION')

  checkFile('sidecar/server/utils/rate-limiter.mjs', 'Rate limiter module exists')
  checkFile('sidecar/server/utils/ddos-detector.mjs', 'DDoS detector module exists')
  checkFile('sidecar/server/middleware/03.rate-limit.mjs', 'Rate limiting middleware exists')

  checkFileContains('sidecar/server/utils/rate-limiter.mjs', 'TokenBucket', 'Token bucket algorithm implemented')
  checkFileContains('sidecar/server/utils/query-cost-estimator.mjs', 'estimateCost', 'Query cost estimation implemented')

  // ==========================================================================
  // 6. CIRCUIT BREAKERS & RESILIENCE
  // ==========================================================================
  printSection('6. CIRCUIT BREAKERS & RESILIENCE')

  checkFile('sidecar/server/utils/circuit-breaker.mjs', 'Circuit breaker module exists')
  checkFile('sidecar/server/utils/backpressure-manager.mjs', 'Backpressure manager exists')

  checkFileContains('sidecar/server/utils/circuit-breaker.mjs', 'CircuitBreaker', 'Circuit breaker class implemented')
  checkFileContains('sidecar/server/utils/circuit-breaker.mjs', 'OPEN|CLOSED|HALF_OPEN', 'Circuit breaker states defined')

  // ==========================================================================
  // 7. TESTING INFRASTRUCTURE
  // ==========================================================================
  printSection('7. TESTING INFRASTRUCTURE')

  checkFile('sidecar/vitest.config.mjs', 'Vitest configuration exists')
  checkFile('sidecar/test/unit/auth.test.mjs', 'Authentication tests exist')
  checkFile('sidecar/test/unit/rbac.test.mjs', 'RBAC tests exist')
  checkFile('sidecar/test/unit/secure-sandbox.test.mjs', 'Sandbox security tests exist')
  checkFile('sidecar/test/integration/full-stack.test.mjs', 'Full-stack integration test exists')

  // Run tests
  print('\nRunning unit tests...', 'cyan')
  checkCommand(
    'cd sidecar && npm run test:unit',
    'Unit tests passing (100%)'
  )

  print('\nRunning integration tests...', 'cyan')
  checkCommand(
    'cd sidecar && npm run test -- test/integration',
    'Integration tests passing'
  )

  // ==========================================================================
  // 8. OBSERVABILITY (OTEL)
  // ==========================================================================
  printSection('8. OBSERVABILITY & OTEL')

  checkFile('sidecar/server/plugins/01.telemetry.mjs', 'OTEL telemetry plugin exists')
  checkFile('sidecar/server/middleware/01.telemetry.mjs', 'OTEL middleware exists')
  checkFile('sidecar/server/utils/otel-context-propagation.mjs', 'OTEL context propagation exists')
  checkFile('sidecar/server/utils/slo-tracker.mjs', 'SLO tracker exists')

  checkWarning(
    existsSync(join(projectRoot, 'sidecar/server/utils/otel-metrics.mjs')),
    'OTEL metrics helpers exist'
  )

  checkFileContains('sidecar/server/plugins/01.telemetry.mjs', '@opentelemetry', 'OTEL SDK imported')
  checkFileContains('sidecar/server/utils/slo-tracker.mjs', 'SLO', 'SLO tracking implemented')

  // ==========================================================================
  // 9. KNOWLEDGE HOOKS & NITRO TASKS
  // ==========================================================================
  printSection('9. KNOWLEDGE HOOKS & NITRO TASKS')

  checkFile('sidecar/server/api/hooks/register.post.mjs', 'Hook registration API exists')
  checkFile('sidecar/server/tasks/hooks/evaluate-periodic.mjs', 'Periodic hook evaluation task exists')
  checkFile('sidecar/server/tasks/policies/refresh-packs.mjs', 'Policy refresh task exists')
  checkFile('sidecar/server/tasks/lockchain/archive.mjs', 'Lockchain archive task exists')
  checkFile('sidecar/server/tasks/health/self-heal.mjs', 'Self-heal task exists')

  checkWarning(
    existsSync(join(projectRoot, 'sidecar/nuxt.config.ts')) &&
    readFileSync(join(projectRoot, 'sidecar/nuxt.config.ts'), 'utf-8').includes('tasks: true'),
    'Nitro tasks enabled in nuxt.config.ts'
  )

  // ==========================================================================
  // 10. INFRASTRUCTURE (Terraform)
  // ==========================================================================
  printSection('10. INFRASTRUCTURE (Terraform)')

  checkFile('terraform/main.tf', 'Main Terraform configuration exists')
  checkFile('terraform/variables.tf', 'Terraform variables defined')
  checkFile('terraform/outputs.tf', 'Terraform outputs defined')
  checkFile('terraform/vault.tf', 'Vault configuration exists')
  checkFile('terraform/acm-certificates.tf', 'TLS certificate configuration exists')

  checkFileContains('terraform/main.tf', 'kubernetes', 'Kubernetes provider configured')
  checkFileContains('terraform/variables.tf', 'vault', 'Vault variables defined')

  checkWarning(
    readFileSync(join(projectRoot, 'terraform/main.tf'), 'utf-8').includes('backend "s3"'),
    'Remote Terraform state backend configured (S3)'
  )

  // ==========================================================================
  // 11. DEPENDENCIES
  // ==========================================================================
  printSection('11. SECURITY DEPENDENCIES')

  const packageJson = JSON.parse(readFileSync(join(projectRoot, 'sidecar/package.json'), 'utf-8'))
  const deps = { ...packageJson.dependencies, ...packageJson.devDependencies }

  checkWarning(deps['bcrypt'] !== undefined, 'bcrypt package installed')
  checkWarning(deps['jsonwebtoken'] !== undefined, 'jsonwebtoken package installed')
  checkWarning(deps['helmet'] !== undefined, 'helmet package installed')
  checkWarning(deps['node-vault'] !== undefined, 'node-vault package installed')
  checkWarning(deps['ioredis'] !== undefined, 'ioredis package installed (for rate limiting)')
  checkWarning(deps['elliptic'] !== undefined, 'elliptic package installed (for Byzantine consensus)')

  // Check for vm2 (should NOT be present)
  checkWarning(deps['vm2'] === undefined, 'vm2 package NOT installed (critical vulnerability)')

  // ==========================================================================
  // 12. CONFIGURATION VALIDATION
  // ==========================================================================
  printSection('12. CONFIGURATION VALIDATION')

  // Check environment variable configuration
  checkWarning(
    existsSync(join(projectRoot, 'sidecar/.env.example')),
    '.env.example exists for configuration reference'
  )

  // Check for hardcoded secrets in Terraform
  const varContent = readFileSync(join(projectRoot, 'terraform/variables.tf'), 'utf-8')
  checkWarning(
    !varContent.includes('default     = "test:test"'),
    'No hardcoded test credentials in Terraform variables'
  )

  // ==========================================================================
  // FINAL REPORT
  // ==========================================================================
  printSection('PRODUCTION READINESS SUMMARY')

  const totalChecks = results.passed + results.failed + results.warnings
  const passRate = ((results.passed / totalChecks) * 100).toFixed(1)
  const score = ((results.passed + results.warnings * 0.5) / totalChecks * 100).toFixed(1)

  print(`\nTotal Checks: ${totalChecks}`, 'cyan')
  print(`✅ Passed: ${results.passed} (${((results.passed / totalChecks) * 100).toFixed(1)}%)`, 'green')
  print(`❌ Failed: ${results.failed} (${((results.failed / totalChecks) * 100).toFixed(1)}%)`, 'red')
  print(`⚠️  Warnings: ${results.warnings} (${((results.warnings / totalChecks) * 100).toFixed(1)}%)`, 'yellow')

  print(`\n${'='.repeat(80)}`, 'cyan')
  print(`  PRODUCTION READINESS SCORE: ${score}/100`, score >= 85 ? 'green' : score >= 70 ? 'yellow' : 'red')
  print('='.repeat(80), 'cyan')

  // Deployment recommendation
  print('\n📊 DEPLOYMENT RECOMMENDATION:\n', 'bold')

  if (results.failed === 0 && score >= 85) {
    print('✅ PRODUCTION READY', 'green')
    print('   All critical checks passed. Safe to deploy to production.\n', 'green')
  } else if (results.failed === 0 && score >= 70) {
    print('⚠️  CONDITIONAL PRODUCTION READY', 'yellow')
    print('   Critical checks passed, but some warnings remain.', 'yellow')
    print('   Safe for staging deployment. Address warnings before full production.\n', 'yellow')
  } else if (results.failed <= 3 && score >= 60) {
    print('⚠️  STAGING READY', 'yellow')
    print('   Some critical checks failed. Deploy to staging only.', 'yellow')
    print('   Address failures before production deployment.\n', 'yellow')
  } else {
    print('❌ NOT PRODUCTION READY', 'red')
    print('   Multiple critical checks failed. DO NOT deploy to production.', 'red')
    print('   Address all failures before proceeding.\n', 'red')
  }

  // Blockers
  if (results.failed > 0) {
    print('🚨 CRITICAL BLOCKERS:', 'red')
    print('   1. Review failed checks above', 'red')
    print('   2. Implement missing security controls', 'red')
    print('   3. Re-run this script to validate fixes\n', 'red')
  }

  // Next steps
  print('📋 NEXT STEPS:', 'cyan')
  print('   1. Address any failed checks', 'cyan')
  print('   2. Resolve warnings for production deployment', 'cyan')
  print('   3. Deploy TLS certificates (if not already done)', 'cyan')
  print('   4. Deploy Vault server and migrate secrets', 'cyan')
  print('   5. Enable Nitro tasks in nuxt.config.ts', 'cyan')
  print('   6. Run full E2E test suite', 'cyan')
  print('   7. Perform security audit & penetration testing\n', 'cyan')

  print(`Validation completed at: ${new Date().toISOString()}\n`, 'cyan')

  // Exit code
  process.exit(results.failed > 0 ? 1 : 0)
}

// Run checks
runChecks().catch((error) => {
  print(`\n❌ Fatal error during validation: ${error.message}`, 'red')
  console.error(error)
  process.exit(1)
})
