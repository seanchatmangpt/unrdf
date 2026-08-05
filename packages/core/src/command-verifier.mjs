/** Exact command execution verifier. */
import { spawn } from 'node:child_process';
import { createHash } from 'node:crypto';

export async function verifyCommand(command, args = [], options = {}) {
  if (!command) throw new TypeError('command is required');
  const { cwd = process.cwd(), env = {}, timeoutMs = 300000, maxOutputBytes = 1024 * 1024 } = options;
  const started = process.hrtime.bigint();
  let stdout = Buffer.alloc(0);
  let stderr = Buffer.alloc(0);
  let timedOut = false;
  let spawnError = null;
  const child = spawn(command, args, { cwd, env: { ...process.env, ...env }, stdio: ['ignore', 'pipe', 'pipe'] });
  const collect = (current, chunk) => Buffer.concat([current, chunk]).subarray(-maxOutputBytes);
  child.stdout.on('data', chunk => { stdout = collect(stdout, chunk); });
  child.stderr.on('data', chunk => { stderr = collect(stderr, chunk); });
  child.once('error', error => { spawnError = error.message; });
  const timer = setTimeout(() => {
    timedOut = true;
    child.kill('SIGTERM');
  }, timeoutMs);
  const exitCode = await new Promise(resolve => child.once('close', code => resolve(code ?? 1)));
  clearTimeout(timer);
  const durationMs = Number(process.hrtime.bigint() - started) / 1e6;
  const output = Buffer.concat([stdout, stderr]);
  return {
    command: [command, ...args],
    cwd,
    exitCode,
    timedOut,
    spawnError,
    durationMs,
    stdout: stdout.toString(),
    stderr: stderr.toString(),
    outputDigest: createHash('sha256').update(output).digest('hex'),
    state: exitCode === 0 && !timedOut && !spawnError ? 'ALIVE' : timedOut ? 'BLOCKED' : 'BUILD_BROKEN',
  };
}

export function commandVerifier(command, args = [], options = {}) {
  return async () => {
    const result = await verifyCommand(command, args, options);
    if (result.state !== 'ALIVE') throw Object.assign(new Error(`COMMAND_FAILED:${command}`), { result });
    return result;
  };
}
