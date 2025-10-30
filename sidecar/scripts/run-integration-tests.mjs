#!/usr/bin/env node
import { spawn } from 'node:child_process';
import { setTimeout as delay } from 'node:timers/promises';
import http from 'node:http';

function waitForServer(url, timeoutMs = 30000) {
  const start = Date.now();
  return new Promise(async (resolve, reject) => {
    while (Date.now() - start < timeoutMs) {
      try {
        await new Promise((res, rej) => {
          const req = http.get(url, (resStream) => {
            resStream.resume();
            res(0);
          });
          req.on('error', rej);
          req.setTimeout(2000, () => {
            req.destroy(new Error('timeout'));
          });
        });
        return resolve();
      } catch {}
      await delay(1000);
    }
    reject(new Error(`Server not available at ${url} within ${timeoutMs}ms`));
  });
}

async function main() {
  const dev = spawn('pnpm', ['dev'], { cwd: new URL('..', import.meta.url), stdio: 'inherit', shell: true });
  try {
    await waitForServer('http://localhost:3000');
    const test = spawn('pnpm', ['test:nuxt'], { cwd: new URL('..', import.meta.url), stdio: 'inherit', shell: true });
    await new Promise((resolve, reject) => {
      test.on('exit', (code) => (code === 0 ? resolve() : reject(new Error(`tests failed with code ${code}`))));
    });
  } finally {
    dev.kill('SIGINT');
  }
}

main().catch((err) => {
  console.error(err.message);
  process.exit(1);
});


