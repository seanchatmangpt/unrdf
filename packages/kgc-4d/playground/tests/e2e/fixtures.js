import { test as base, expect } from '@playwright/test';

/**
 * Custom fixtures for KGC-4D Playground tests
 */

/**
 * Custom fixture: waitForSSE
 * Wait for SSE event of a specific type
 */
export const test = base.extend({
  // Wait for an SSE event with timeout
  waitForSSE: async ({ page }, use) => {
    await use(async (eventType, timeout = 5000) => {
      const eventReceived = page.evaluate(
        async (type, dur) => {
          return new Promise((resolve) => {
            const listener = (e) => {
              if (e.data && JSON.parse(e.data).type === type) {
                resolve(true);
              }
            };

            const timer = setTimeout(() => resolve(false), dur);

            // Listen for EventSource events
            window.addEventListener('message', listener);

            return () => {
              clearTimeout(timer);
              window.removeEventListener('message', listener);
            };
          });
        },
        eventType,
        timeout
      );

      return eventReceived;
    });
  },

  // Helper to submit delta and wait for ACK/REJECT
  submitAndWaitForResult: async ({ page }, use) => {
    await use(async (operations, timeout = 5000) => {
      return page.evaluate(
        async (ops, dur) => {
          const response = await fetch('/api/delta', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
              operations: ops,
              source: 'test',
            }),
          });

          return response.json();
        },
        operations,
        timeout
      );
    });
  },

  // Helper to fetch current shard
  getCurrentShard: async ({ page }, use) => {
    await use(async () => {
      return page.evaluate(async () => {
        const response = await fetch('/api/shard');
        const data = await response.json();
        return data.shard;
      });
    });
  },

  // Helper to fetch stats
  getUniverseStats: async ({ page }, use) => {
    await use(async () => {
      return page.evaluate(async () => {
        const response = await fetch('/api/shard?stats=true');
        return response.json();
      });
    });
  },
});

export { expect };
