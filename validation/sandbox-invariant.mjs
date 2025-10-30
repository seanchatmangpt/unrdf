#!/usr/bin/env node
import { createSandboxAdapter } from '../src/security/sandbox-adapter.mjs';

const sandbox = createSandboxAdapter({ timeoutMs: 500 });

const result = sandbox.run('const x = 2 + 3; typeof process === "undefined" ? x : "leak"');

if (result === 'leak') {
  console.error('❌ Sandbox invariant failed: process is accessible');
  process.exit(1);
}

if (result !== 5) {
  console.error('❌ Sandbox computation incorrect');
  process.exit(1);
}

console.log('✅ Sandbox invariant passed');


