#!/usr/bin/env node
/**
 * @fileoverview Type checking in watch mode
 * Instant feedback on type errors during development
 *
 * Usage:
 *   node scripts/typecheck-watch.mjs
 */

import { spawn } from "child_process";

/**
 * @param {string} msg
 */
function log(msg) {
  const timestamp = new Date().toISOString().slice(11, 19);
  console.log(`[${timestamp}] ${msg}`);
}

log("🔍 Starting TypeScript type checking in watch mode...");
log("Press Ctrl+C to exit");

const tsc = spawn(
  "pnpm",
  [
    "exec",
    "tsc",
    "--watch",
    "--noEmit",
    "--preserveWatchOutput",
  ],
  {
    stdio: "inherit",
    shell: true,
  },
);

tsc.on("error", (err) => {
  console.error(`❌ Failed to start type checker: ${err.message}`);
  process.exit(1);
});

process.on("SIGINT", () => {
  log("👋 Stopping type checker...");
  tsc.kill();
  process.exit(0);
});
