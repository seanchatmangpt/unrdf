import { spawn } from 'child_process';
import path from 'path';
import fs from 'fs';

/**
 * Manages the Open Ontologies binary as a production-hardened sidecar.
 */
export class SemanticSidecarManager {
  constructor(logger = console) {
    this.logger = logger;
    this.process = null;
    
    // Find the correct path regardless of where the daemon is run from
    const rootPath = path.resolve(process.cwd(), 'open-ontologies/target/debug/open-ontologies');
    const parentPath = path.resolve(process.cwd(), '../open-ontologies/target/debug/open-ontologies');
    const grandParentPath = path.resolve(process.cwd(), '../../open-ontologies/target/debug/open-ontologies');
    
    this.binaryPath = process.env.OPEN_ONTOLOGIES_PATH || 
                     (fs.existsSync(rootPath) ? rootPath : 
                     (fs.existsSync(parentPath) ? parentPath : grandParentPath));
  }

  start() {
    this.logger.info('🚀 Starting Open Ontologies sidecar...');
    this._spawn();
    
    // Heartbeat/Watchdog: Check health every 5 seconds
    this.watchdog = setInterval(() => {
      if (!this.process) {
        this.logger.warn('⚠️ Sidecar crashed. Restarting...');
        this._spawn();
      }
    }, 5000);
  }

  _spawn() {
    this.process = spawn(this.binaryPath, ['serve'], {
      stdio: ['pipe', 'pipe', 'pipe'],
      detached: true,
      env: { ...process.env, RUST_LOG: 'info' }
    });
    this.process.unref();

    this.process.stdout.on('data', (data) => this.logger.debug(`[OO] ${data}`));
    this.process.stderr.on('data', (data) => this.logger.error(`[OO] ${data}`));

    this.process.on('exit', (code) => {
      this.logger.warn(`⚠️ Open Ontologies sidecar exited with code ${code}`);
      this.process = null;
    });
  }

  async stop() {
    if (this.watchdog) clearInterval(this.watchdog);
    if (!this.process) return;
    this.logger.info('🛑 Stopping Open Ontologies sidecar...');
    this.process.kill('SIGTERM');
    return new Promise((resolve) => {
      this.process.on('exit', () => resolve());
      setTimeout(resolve, 2000); // Fail-safe
    });
  }
}
