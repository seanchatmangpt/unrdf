import { createHash } from 'node:crypto';
import { spawn } from 'node:child_process';
import { stat } from 'node:fs/promises';

function sha256(value) {
  return createHash('sha256').update(value).digest('hex');
}

export class AtomVMProcessRefusal extends Error {
  constructor(code, message, details = {}) {
    super(message);
    this.name = 'AtomVMProcessRefusal';
    this.code = code;
    this.details = Object.freeze({ ...details });
  }
}

async function requireFile(path, code, label) {
  if (typeof path !== 'string' || path.length === 0) {
    throw new AtomVMProcessRefusal(code, `${label} path is required`, { path });
  }
  try {
    const info = await stat(path);
    if (!info.isFile()) throw new Error('not a file');
  } catch (error) {
    throw new AtomVMProcessRefusal(code, `${label} is unavailable: ${path}`, {
      path,
      cause: error.message,
    });
  }
}

function runProcess({ binary, args, cwd, env, timeoutMs }) {
  return new Promise((resolve, reject) => {
    const child = spawn(binary, args, {
      cwd,
      env: { ...process.env, ...env },
      stdio: ['ignore', 'pipe', 'pipe'],
      shell: false,
    });

    let stdout = '';
    let stderr = '';
    let settled = false;
    const timer = setTimeout(() => {
      if (settled) return;
      settled = true;
      child.kill('SIGKILL');
      reject(new AtomVMProcessRefusal(
        'ATOMVM_TIMEOUT_REFUSED',
        `AtomVM exceeded ${timeoutMs} ms`,
        { binary, args, timeoutMs },
      ));
    }, timeoutMs);

    child.stdout.on('data', chunk => { stdout += chunk.toString(); });
    child.stderr.on('data', chunk => { stderr += chunk.toString(); });
    child.on('error', error => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      reject(new AtomVMProcessRefusal(
        'ATOMVM_SPAWN_REFUSED',
        `Unable to start AtomVM: ${error.message}`,
        { binary, args, cause: error.message },
      ));
    });
    child.on('close', (exitCode, signal) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      resolve({ exitCode, signal, stdout, stderr });
    });
  });
}

export class AtomVMProcessBroker {
  constructor({ atomvmBinary, swarms, runtimeRef = 'unknown', timeoutMs = 10_000 } = {}) {
    if (!(swarms instanceof Map) && (typeof swarms !== 'object' || swarms === null)) {
      throw new TypeError('swarms must be a Map or object keyed by admitted swarm id');
    }
    this.atomvmBinary = atomvmBinary;
    this.swarms = swarms instanceof Map ? new Map(swarms) : new Map(Object.entries(swarms));
    this.runtimeRef = runtimeRef;
    this.timeoutMs = timeoutMs;
  }

  async execute({ intent, target, route }) {
    if (!intent || intent.operation !== 'atomvm.execute') {
      throw new AtomVMProcessRefusal(
        'OPERATION_NOT_ADMITTED_REFUSED',
        'AtomVMProcessBroker only admits atomvm.execute',
        { operation: intent?.operation },
      );
    }
    if (!target || !Array.isArray(route) || route.at(-1) !== target.id) {
      throw new AtomVMProcessRefusal(
        'ROUTE_TARGET_MISMATCH_REFUSED',
        'The admitted route must terminate at the target swarm',
        { targetId: target?.id, route },
      );
    }

    const config = this.swarms.get(target.id);
    if (!config) {
      throw new AtomVMProcessRefusal(
        'SWARM_RUNTIME_NOT_CONFIGURED_REFUSED',
        `No AtomVM runtime is configured for swarm ${target.id}`,
        { targetId: target.id },
      );
    }

    await requireFile(this.atomvmBinary, 'ATOMVM_BINARY_NOT_FOUND_REFUSED', 'AtomVM binary');
    await requireFile(config.avmPath, 'AVM_NOT_FOUND_REFUSED', 'AVM application');
    for (const libraryPath of config.libraryPaths ?? []) {
      await requireFile(libraryPath, 'AVM_LIBRARY_NOT_FOUND_REFUSED', 'AVM library');
    }

    const args = [config.avmPath, ...(config.libraryPaths ?? [])];
    const observed = await runProcess({
      binary: this.atomvmBinary,
      args,
      cwd: config.cwd,
      env: config.env,
      timeoutMs: this.timeoutMs,
    });

    if (observed.exitCode !== 0) {
      throw new AtomVMProcessRefusal(
        'ATOMVM_EXIT_BLOCKED',
        `AtomVM exited with code ${observed.exitCode}`,
        {
          targetId: target.id,
          exitCode: observed.exitCode,
          signal: observed.signal,
          stderr: observed.stderr,
        },
      );
    }

    const marker = config.expectedMarker ?? 'atomvm_swarm_alive';
    if (!observed.stdout.includes(marker) && !observed.stderr.includes(marker)) {
      throw new AtomVMProcessRefusal(
        'ATOMVM_MARKER_MISSING_REFUSED',
        `AtomVM completed without required marker ${marker}`,
        { targetId: target.id, marker, stdout: observed.stdout, stderr: observed.stderr },
      );
    }

    return Object.freeze({
      runtime: 'AtomVM',
      runtimeRef: this.runtimeRef,
      targetId: target.id,
      route: Object.freeze([...route]),
      exitCode: observed.exitCode,
      marker,
      markerObserved: true,
      stdoutDigest: sha256(observed.stdout),
      stderrDigest: sha256(observed.stderr),
      stdout: observed.stdout,
      stderr: observed.stderr,
    });
  }
}

export function createAtomVMProcessBroker(options) {
  return new AtomVMProcessBroker(options);
}
