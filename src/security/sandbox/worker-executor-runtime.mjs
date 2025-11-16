/**
 * @file Worker Thread Runtime
 * @description Runtime environment for worker thread execution
 */

import { parentPort, workerData } from 'worker_threads';

try {
  const { code, context, config } = workerData;

  // Create safe global environment
  const sandbox = {
    // Allowed globals
    console: {
      log: (...args) => console.log('[Worker]', ...args),
      error: (...args) => console.error('[Worker]', ...args),
      warn: (...args) => console.warn('[Worker]', ...args),
      info: (...args) => console.info('[Worker]', ...args)
    },
    Date: {
      now: () => Date.now()
    },
    Math,
    JSON,
    // Context data
    ...context
  };

  // Add allowed globals from config
  if (config.allowedGlobals) {
    for (const globalName of config.allowedGlobals) {
      if (globalName in globalThis && !(globalName in sandbox)) {
        sandbox[globalName] = globalThis[globalName];
      }
    }
  }

  // Execute code
  const func = new Function(...Object.keys(sandbox), `
    ${config.strictMode ? '"use strict";' : ''}
    ${code}
  `);

  const result = func(...Object.values(sandbox));

  // Get memory usage
  const memoryUsage = process.memoryUsage();

  // Send result back
  parentPort.postMessage({
    success: true,
    result,
    memoryUsed: {
      used: memoryUsage.heapUsed,
      total: memoryUsage.heapTotal,
      rss: memoryUsage.rss
    }
  });

} catch (error) {
  parentPort.postMessage({
    success: false,
    error: error.message,
    stack: error.stack
  });
}
