#!/usr/bin/env node
/**
 * @file Sync PM4Py OTEL Configuration
 * @module @unrdf/otel/scripts/sync-pm4py-otel
 *
 * Synchronize OTEL configuration with pm4py-rust project.
 * This script copies the latest OTEL collector config and monitoring setup from pm4py.
 */

import { copyFile, readFile, writeFile } from 'fs/promises';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import { execSync } from 'child_process';
import { homedir } from 'os';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Paths
const pm4pyPath = process.env.PM4PY_PATH || resolve(homedir(), 'chatmangpt/pm4py');
const deployDir = resolve(__dirname, '../deploy');

async function main() {
  console.log('🔄 Syncing OTEL configuration with pm4py-rust...\n');

  try {
    // Copy OTEL collector config
    const collectorSrc = resolve(pm4pyPath, 'deploy/otel-collector-config.yaml');
    const collectorDest = resolve(deployDir, 'otel-collector-config.yaml');

    console.log(`📋 Copying collector config:`);
    console.log(`   From: ${collectorSrc}`);
    console.log(`   To:   ${collectorDest}`);

    execSync(`cp "${collectorSrc}" "${collectorDest}"`);
    console.log('✅ Collector config copied\n');

    // Update service name in config for UNRDF
    let config = await readFile(collectorDest, 'utf-8');
    config = config.replace(/service\.name: pm4py/g, 'service.name: unrdf');
    config = config.replace(/service\.version: .*/g, 'service.version: 26.4.7');
    await writeFile(collectorDest, config);
    console.log('✅ Config updated for UNRDF\n');

    // Create symbolic link to pm4py monitoring
    const monitoringLink = resolve(deployDir, 'pm4py-monitoring');
    try {
      execSync(`ln -sf "${resolve(pm4pyPath, 'pm4py/monitoring')}" "${monitoringLink}"`);
      console.log('✅ Created symlink to pm4py monitoring module\n');
    } catch {
      console.log('⚠️  Could not create symlink (may already exist)\n');
    }

    // Copy monitoring Python module reference
    const readmePath = resolve(deployDir, 'README.md');
    const readmeContent = `# UNRDF OTEL Configuration

This directory contains OpenTelemetry configuration synchronized with pm4py-rust.

## Files

- \`otel-collector-config.yaml\`: OTEL collector configuration (synced from pm4py)
- \`pm4py-monitoring\`: Symlink to pm4py monitoring module

## PM4Py Integration

The OTEL package now integrates with pm4py-rust's monitoring infrastructure:

\`\`\`javascript
import { getPM4Py, recordUNRDFMetrics } from '@unrdf/otel/pm4py';
import { monitorOperation } from '@unrdf/otel/monitoring';

// Record metrics
await recordUNRDFMetrics('sparql_query', { graph: 'my-graph' });

// Monitor operations
await monitorOperation('graph_load', async () => {
  // Your operation here
  return result;
});
\`\`\`

## PM4Py Location

\`\`\bash
export PM4PY_PATH=~/chatmangpt/pm4py
\`\`\`

## Monitoring Stack

- **Prometheus**: http://localhost:3001
- **Grafana**: http://localhost:3000
- **Jaeger**: http://localhost:16686
- **OTEL Collector**: http://localhost:14317 (gRPC), http://localhost:14318 (HTTP)

## Sync Command

To sync with latest pm4py configuration:

\`\`\`bash
pnpm --filter @unrdf/otel run sync:pm4py
\`\`\`
`;

    await writeFile(readmePath, readmeContent);
    console.log('✅ Created README.md\n');

    console.log('✨ OTEL configuration synced successfully!');
    console.log('\nNext steps:');
    console.log('1. Set PM4PY_PATH environment variable');
    console.log('2. Import monitoring utilities in your code');
    console.log('3. Start OTEL collector: docker-compose up -d');

  } catch (error) {
    console.error('❌ Sync failed:', error.message);
    process.exit(1);
  }
}

main();
