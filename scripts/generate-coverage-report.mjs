#!/usr/bin/env node
/**
 * @file Generate Coverage Report
 * @description Consolidate coverage reports from all packages
 */

import { readdirSync, existsSync, readFileSync, writeFileSync, mkdirSync } from 'node:fs'
import { join, resolve } from 'node:path'

const ROOT = resolve(process.cwd())
const COVERAGE_DIR = join(ROOT, 'coverage-consolidated')

/**
 * Find all coverage reports
 * @returns {Array<{name: string, path: string, data: Object}>}
 */
function findCoverageReports() {
  const reports = []
  const dirs = ['examples', 'packages']

  for (const dir of dirs) {
    const dirPath = join(ROOT, dir)
    if (!existsSync(dirPath)) continue

    const items = readdirSync(dirPath, { withFileTypes: true })
    for (const item of items) {
      if (!item.isDirectory()) continue

      const coveragePath = join(dirPath, item.name, 'coverage', 'coverage-summary.json')
      if (existsSync(coveragePath)) {
        try {
          const data = JSON.parse(readFileSync(coveragePath, 'utf-8'))
          reports.push({
            name: item.name,
            path: coveragePath,
            data
          })
        } catch (err) {
          console.warn(`⚠️  Failed to parse coverage for ${item.name}:`, err.message)
        }
      }
    }
  }

  return reports
}

/**
 * Calculate aggregate coverage metrics
 * @param {Array<Object>} reports - Coverage reports
 * @returns {Object} - Aggregate metrics
 */
function calculateAggregate(reports) {
  const aggregate = {
    lines: { total: 0, covered: 0, pct: 0 },
    statements: { total: 0, covered: 0, pct: 0 },
    functions: { total: 0, covered: 0, pct: 0 },
    branches: { total: 0, covered: 0, pct: 0 }
  }

  for (const report of reports) {
    const total = report.data.total
    if (!total) continue

    aggregate.lines.total += total.lines?.total || 0
    aggregate.lines.covered += total.lines?.covered || 0

    aggregate.statements.total += total.statements?.total || 0
    aggregate.statements.covered += total.statements?.covered || 0

    aggregate.functions.total += total.functions?.total || 0
    aggregate.functions.covered += total.functions?.covered || 0

    aggregate.branches.total += total.branches?.total || 0
    aggregate.branches.covered += total.branches?.covered || 0
  }

  // Calculate percentages
  aggregate.lines.pct = aggregate.lines.total > 0
    ? (aggregate.lines.covered / aggregate.lines.total * 100)
    : 0

  aggregate.statements.pct = aggregate.statements.total > 0
    ? (aggregate.statements.covered / aggregate.statements.total * 100)
    : 0

  aggregate.functions.pct = aggregate.functions.total > 0
    ? (aggregate.functions.covered / aggregate.functions.total * 100)
    : 0

  aggregate.branches.pct = aggregate.branches.total > 0
    ? (aggregate.branches.covered / aggregate.branches.total * 100)
    : 0

  return aggregate
}

/**
 * Generate HTML report
 * @param {Object} aggregate - Aggregate metrics
 * @param {Array<Object>} reports - Individual reports
 * @returns {string} - HTML content
 */
function generateHTMLReport(aggregate, reports) {
  const timestamp = new Date().toISOString()

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>UNRDF Coverage Report</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      max-width: 1200px;
      margin: 0 auto;
      padding: 2rem;
      background: #f5f5f5;
    }
    h1, h2 { color: #333; }
    .summary {
      background: white;
      padding: 1.5rem;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      margin-bottom: 2rem;
    }
    .metrics {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 1rem;
      margin-top: 1rem;
    }
    .metric {
      background: #f9f9f9;
      padding: 1rem;
      border-radius: 4px;
      text-align: center;
    }
    .metric-value {
      font-size: 2rem;
      font-weight: bold;
      color: #333;
    }
    .metric-label {
      color: #666;
      font-size: 0.875rem;
      text-transform: uppercase;
      margin-top: 0.5rem;
    }
    .packages {
      background: white;
      padding: 1.5rem;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    table {
      width: 100%;
      border-collapse: collapse;
      margin-top: 1rem;
    }
    th, td {
      padding: 0.75rem;
      text-align: left;
      border-bottom: 1px solid #e0e0e0;
    }
    th {
      background: #f5f5f5;
      font-weight: 600;
    }
    .good { color: #22c55e; }
    .medium { color: #f59e0b; }
    .poor { color: #ef4444; }
    .footer {
      margin-top: 2rem;
      text-align: center;
      color: #666;
      font-size: 0.875rem;
    }
  </style>
</head>
<body>
  <h1>UNRDF Coverage Report</h1>

  <div class="summary">
    <h2>Overall Coverage</h2>
    <div class="metrics">
      <div class="metric">
        <div class="metric-value ${aggregate.lines.pct >= 80 ? 'good' : aggregate.lines.pct >= 60 ? 'medium' : 'poor'}">
          ${aggregate.lines.pct.toFixed(1)}%
        </div>
        <div class="metric-label">Lines</div>
        <div>${aggregate.lines.covered} / ${aggregate.lines.total}</div>
      </div>
      <div class="metric">
        <div class="metric-value ${aggregate.statements.pct >= 80 ? 'good' : aggregate.statements.pct >= 60 ? 'medium' : 'poor'}">
          ${aggregate.statements.pct.toFixed(1)}%
        </div>
        <div class="metric-label">Statements</div>
        <div>${aggregate.statements.covered} / ${aggregate.statements.total}</div>
      </div>
      <div class="metric">
        <div class="metric-value ${aggregate.functions.pct >= 80 ? 'good' : aggregate.functions.pct >= 60 ? 'medium' : 'poor'}">
          ${aggregate.functions.pct.toFixed(1)}%
        </div>
        <div class="metric-label">Functions</div>
        <div>${aggregate.functions.covered} / ${aggregate.functions.total}</div>
      </div>
      <div class="metric">
        <div class="metric-value ${aggregate.branches.pct >= 80 ? 'good' : aggregate.branches.pct >= 60 ? 'medium' : 'poor'}">
          ${aggregate.branches.pct.toFixed(1)}%
        </div>
        <div class="metric-label">Branches</div>
        <div>${aggregate.branches.covered} / ${aggregate.branches.total}</div>
      </div>
    </div>
  </div>

  <div class="packages">
    <h2>Package Coverage</h2>
    <table>
      <thead>
        <tr>
          <th>Package</th>
          <th>Lines</th>
          <th>Statements</th>
          <th>Functions</th>
          <th>Branches</th>
        </tr>
      </thead>
      <tbody>
        ${reports.map(r => {
          const t = r.data.total
          return `
        <tr>
          <td>${r.name}</td>
          <td class="${(t.lines?.pct || 0) >= 80 ? 'good' : (t.lines?.pct || 0) >= 60 ? 'medium' : 'poor'}">
            ${(t.lines?.pct || 0).toFixed(1)}%
          </td>
          <td class="${(t.statements?.pct || 0) >= 80 ? 'good' : (t.statements?.pct || 0) >= 60 ? 'medium' : 'poor'}">
            ${(t.statements?.pct || 0).toFixed(1)}%
          </td>
          <td class="${(t.functions?.pct || 0) >= 80 ? 'good' : (t.functions?.pct || 0) >= 60 ? 'medium' : 'poor'}">
            ${(t.functions?.pct || 0).toFixed(1)}%
          </td>
          <td class="${(t.branches?.pct || 0) >= 80 ? 'good' : (t.branches?.pct || 0) >= 60 ? 'medium' : 'poor'}">
            ${(t.branches?.pct || 0).toFixed(1)}%
          </td>
        </tr>
          `
        }).join('')}
      </tbody>
    </table>
  </div>

  <div class="footer">
    Generated on ${timestamp}
  </div>
</body>
</html>`
}

/**
 * Main execution
 */
function main() {
  console.log('📊 Generating consolidated coverage report...\n')

  const reports = findCoverageReports()

  if (reports.length === 0) {
    console.log('⚠️  No coverage reports found')
    console.log('Run tests with coverage first: pnpm test:coverage')
    process.exit(0)
  }

  console.log(`Found ${reports.length} coverage reports`)

  const aggregate = calculateAggregate(reports)

  // Create output directory
  if (!existsSync(COVERAGE_DIR)) {
    mkdirSync(COVERAGE_DIR, { recursive: true })
  }

  // Write JSON summary
  const summaryPath = join(COVERAGE_DIR, 'summary.json')
  writeFileSync(summaryPath, JSON.stringify({
    timestamp: new Date().toISOString(),
    aggregate,
    packages: reports.map(r => ({
      name: r.name,
      coverage: r.data.total
    }))
  }, null, 2))

  console.log(`✓ Written JSON summary: ${summaryPath}`)

  // Write HTML report
  const htmlPath = join(COVERAGE_DIR, 'index.html')
  const html = generateHTMLReport(aggregate, reports)
  writeFileSync(htmlPath, html)

  console.log(`✓ Written HTML report: ${htmlPath}`)

  // Print summary
  console.log('\n' + '='.repeat(80))
  console.log('COVERAGE SUMMARY')
  console.log('='.repeat(80))
  console.log(`Lines:      ${aggregate.lines.pct.toFixed(1)}% (${aggregate.lines.covered}/${aggregate.lines.total})`)
  console.log(`Statements: ${aggregate.statements.pct.toFixed(1)}% (${aggregate.statements.covered}/${aggregate.statements.total})`)
  console.log(`Functions:  ${aggregate.functions.pct.toFixed(1)}% (${aggregate.functions.covered}/${aggregate.functions.total})`)
  console.log(`Branches:   ${aggregate.branches.pct.toFixed(1)}% (${aggregate.branches.covered}/${aggregate.branches.total})`)
  console.log('='.repeat(80))

  console.log(`\n✨ Open ${htmlPath} in your browser to view the report`)
}

main()
