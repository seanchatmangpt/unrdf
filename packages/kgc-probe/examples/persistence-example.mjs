#!/usr/bin/env node
/**
 * @file Persistence Probe Example
 * @description Example usage of the persistence probe
 */

import { probePersistence } from '../src/probes/persistence.mjs';
import { promises as fs } from 'node:fs';
import { join } from 'node:path';

async function main() {
  // Create output directory
  const outputDir = join(process.cwd(), 'probe-output');
  await fs.mkdir(outputDir, { recursive: true });

  console.log('Running persistence probe...\n');

  // Configure probe
  const config = {
    out: outputDir,           // Output directory for test files
    timeout: 5000,            // 5 second timeout per operation
    maxQuota: 100 * 1024 * 1024,  // 100 MB max quota test
    chunkSize: 1024 * 1024,   // 1 MB chunks
  };

  try {
    // Run probe
    const observations = await probePersistence(config);

    console.log(`✅ Probe completed with ${observations.length} observations\n`);

    // Display observations
    console.log('Key Observations:\n');

    // Storage observations
    const storageObs = observations.filter(obs => obs.category === 'storage');
    console.log(`📦 Storage (${storageObs.length} observations):`);
    storageObs.forEach(obs => {
      console.log(`  ${obs.value ? '✓' : '✗'} ${obs.observation}`);
    });

    // Filesystem observations
    const filesystemObs = observations.filter(obs => obs.category === 'filesystem');
    console.log(`\n📁 Filesystem (${filesystemObs.length} observations):`);
    filesystemObs.forEach(obs => {
      console.log(`  ${obs.value ? '✓' : '✗'} ${obs.observation}`);
    });

    // Quota observations
    const quotaObs = observations.filter(obs => obs.category === 'quota');
    console.log(`\n💾 Quota (${quotaObs.length} observations):`);
    quotaObs.forEach(obs => {
      console.log(`  ${obs.observation}`);
      console.log(`    Bytes written: ${obs.value?.toLocaleString() || 'N/A'}`);
      if (obs.metadata?.throughputMBps) {
        console.log(`    Throughput: ${obs.metadata.throughputMBps.toFixed(2)} MB/s`);
      }
    });

    // Performance observations
    const perfObs = observations.filter(obs => obs.category === 'performance');
    console.log(`\n⚡ Performance (${perfObs.length} observations):`);
    perfObs.forEach(obs => {
      console.log(`  ${obs.observation}: ${obs.value}`);
      if (obs.metadata?.avgLatency) {
        console.log(`    Avg latency: ${obs.metadata.avgLatency.toFixed(2)}ms`);
      }
    });

    // Permissions observations
    const permObs = observations.filter(obs => obs.category === 'permissions');
    console.log(`\n🔒 Permissions (${permObs.length} observations):`);
    permObs.forEach(obs => {
      console.log(`  ${obs.value ? '✓' : '✗'} ${obs.observation}`);
    });

    // Guard decisions
    const guardDenials = observations.filter(obs => obs.guardDecision && !obs.guardDecision.allowed);
    if (guardDenials.length > 0) {
      console.log(`\n🛡️  Guard Denials (${guardDenials.length}):`);
      guardDenials.forEach(obs => {
        console.log(`  ✗ ${obs.observation}`);
        console.log(`    Reason: ${obs.guardDecision.reason}`);
      });
    }

    // Save observations to file
    const outputFile = join(outputDir, 'persistence-observations.json');
    await fs.writeFile(outputFile, JSON.stringify(observations, null, 2));
    console.log(`\n💾 Saved observations to: ${outputFile}`);

  } catch (error) {
    console.error('❌ Probe failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

main();
