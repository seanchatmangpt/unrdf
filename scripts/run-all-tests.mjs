#!/usr/bin/env node
/**
 * @file Run All Tests Script
 * @description Execute tests for all UNRDF examples and generate summary
 */

import { execSync } from 'node:child_process'
import { readdirSync, existsSync, readFileSync } from 'node:fs'
import { join, resolve } from 'node:path'

const ROOT = resolve(process.cwd())
const EXAMPLES_DIR = join(ROOT, 'examples')
const PACKAGES_DIR = join(ROOT, 'packages')

/**
 * Find all packages with tests
 * @returns {Array<{name: string, path: string, type: string}>}
 */
function findTestablePackages() {
  const packages = []

  // Check examples directory
  if (existsSync(EXAMPLES_DIR)) {
    const examples = readdirSync(EXAMPLES_DIR, { withFileTypes: true })
    for (const example of examples) {
      if (example.isDirectory()) {
        const examplePath = join(EXAMPLES_DIR, example.name)
        const packageJsonPath = join(examplePath, 'package.json')
        const testDir = join(examplePath, 'test')

        if (existsSync(packageJsonPath) && existsSync(testDir)) {
          const pkg = JSON.parse(readFileSync(packageJsonPath, 'utf-8'))
          packages.push({
            name: pkg.name || example.name,
            path: examplePath,
            type: 'example'
          })
        }
      }
    }
  }

  // Check packages directory
  if (existsSync(PACKAGES_DIR)) {
    const pkgs = readdirSync(PACKAGES_DIR, { withFileTypes: true })
    for (const pkg of pkgs) {
      if (pkg.isDirectory()) {
        const pkgPath = join(PACKAGES_DIR, pkg.name)
        const packageJsonPath = join(pkgPath, 'package.json')
        const testDir = join(pkgPath, 'test')

        if (existsSync(packageJsonPath) && existsSync(testDir)) {
          const pkgData = JSON.parse(readFileSync(packageJsonPath, 'utf-8'))
          packages.push({
            name: pkgData.name || pkg.name,
            path: pkgPath,
            type: 'package'
          })
        }
      }
    }
  }

  return packages
}

/**
 * Run tests for a package
 * @param {Object} pkg - Package info
 * @returns {Object} - Test results
 */
function runTests(pkg) {
  console.log(`\n📦 Testing ${pkg.name}...`)

  const startTime = Date.now()
  let passed = 0
  let failed = 0
  let skipped = 0
  let coverage = 0
  let error = null

  try {
    const output = execSync('pnpm test', {
      cwd: pkg.path,
      encoding: 'utf-8',
      stdio: 'pipe'
    })

    // Parse test output
    const passMatch = output.match(/(\d+) passed/)
    const failMatch = output.match(/(\d+) failed/)
    const skipMatch = output.match(/(\d+) skipped/)
    const coverageMatch = output.match(/All files\s+\|\s+([\d.]+)/)

    passed = passMatch ? parseInt(passMatch[1], 10) : 0
    failed = failMatch ? parseInt(failMatch[1], 10) : 0
    skipped = skipMatch ? parseInt(skipMatch[1], 10) : 0
    coverage = coverageMatch ? parseFloat(coverageMatch[1]) : 0

    console.log(`  ✓ ${passed} tests passing`)
    if (skipped > 0) console.log(`  ⊘ ${skipped} tests skipped`)
    if (coverage > 0) console.log(`  📊 ${coverage.toFixed(2)}% coverage`)

  } catch (err) {
    failed = 1
    error = err.message
    console.log(`  ✗ Tests failed`)
  }

  const duration = Date.now() - startTime

  return {
    name: pkg.name,
    type: pkg.type,
    passed,
    failed,
    skipped,
    coverage,
    duration,
    error,
    success: failed === 0 && error === null
  }
}

/**
 * Generate summary report
 * @param {Array<Object>} results - Test results
 */
function generateSummary(results) {
  console.log('\n' + '='.repeat(80))
  console.log('📊 TEST SUMMARY')
  console.log('='.repeat(80))

  const totalPassed = results.reduce((sum, r) => sum + r.passed, 0)
  const totalFailed = results.reduce((sum, r) => sum + r.failed, 0)
  const totalSkipped = results.reduce((sum, r) => sum + r.skipped, 0)
  const avgCoverage = results.reduce((sum, r) => sum + r.coverage, 0) / results.length
  const totalDuration = results.reduce((sum, r) => sum + r.duration, 0)
  const successCount = results.filter(r => r.success).length

  console.log(`\nPackages Tested: ${results.length}`)
  console.log(`Packages Passed: ${successCount}`)
  console.log(`Packages Failed: ${results.length - successCount}`)
  console.log(`\nTotal Tests: ${totalPassed + totalFailed + totalSkipped}`)
  console.log(`  ✓ Passed: ${totalPassed}`)
  console.log(`  ✗ Failed: ${totalFailed}`)
  console.log(`  ⊘ Skipped: ${totalSkipped}`)
  console.log(`\nAverage Coverage: ${avgCoverage.toFixed(2)}%`)
  console.log(`Total Duration: ${(totalDuration / 1000).toFixed(2)}s`)

  // List failures
  const failures = results.filter(r => !r.success)
  if (failures.length > 0) {
    console.log('\n❌ FAILED PACKAGES:')
    for (const result of failures) {
      console.log(`  - ${result.name}`)
      if (result.error) {
        console.log(`    ${result.error.split('\n')[0]}`)
      }
    }
  }

  // List successes
  const successes = results.filter(r => r.success)
  if (successes.length > 0) {
    console.log('\n✅ PASSED PACKAGES:')
    for (const result of successes) {
      console.log(`  - ${result.name} (${result.passed} tests, ${result.coverage.toFixed(1)}% coverage)`)
    }
  }

  console.log('\n' + '='.repeat(80))

  return {
    totalPassed,
    totalFailed,
    totalSkipped,
    avgCoverage,
    totalDuration,
    successCount,
    results
  }
}

/**
 * Main execution
 */
async function main() {
  console.log('🚀 Running UNRDF Test Suite\n')

  const packages = findTestablePackages()

  if (packages.length === 0) {
    console.log('⚠️  No testable packages found')
    process.exit(0)
  }

  console.log(`Found ${packages.length} testable packages`)

  const results = []
  for (const pkg of packages) {
    const result = runTests(pkg)
    results.push(result)
  }

  const summary = generateSummary(results)

  // Exit with error code if any tests failed
  if (summary.totalFailed > 0 || summary.successCount < packages.length) {
    process.exit(1)
  } else {
    process.exit(0)
  }
}

main().catch(err => {
  console.error('Fatal error:', err)
  process.exit(1)
})
