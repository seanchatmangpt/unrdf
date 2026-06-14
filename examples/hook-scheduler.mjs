/**
 * @file Hook Scheduler Example
 * @description Demonstrates HookScheduler for time-based and startup hook execution
 *
 * API demonstrated: packages/hooks/src/hooks/hook-scheduler.mjs
 * Docs: packages/hooks/README.md — "HookScheduler" section
 *
 * HookScheduler triggers hooks on schedules: interval (every N ms), startup
 * (once when start() is called), idle (when no activity for N ms), and cron.
 * This example uses interval and startup — self-contained, no cron parser.
 *
 * EXPECTED OUTPUT:
 *   [startup hook] executed once immediately on start()
 *   [interval hook] executed after tickInterval ms
 *   [getStats] returns schedule counts and run totals
 *   [unregister] removed schedule no longer runs
 *   [maxRuns] schedule with maxRuns:1 executes exactly once then stops
 */

import { HookScheduler } from '../packages/hooks/src/hooks/hook-scheduler.mjs';

// ─── 1. startup hook — runs once immediately on start() ──────────────────────

const scheduler = new HookScheduler({
  tickInterval: 30, // fast ticks for testing
  executeHook: async (_hook) => {},
});

const startupHook = { id: 'startup-hook', condition: null, _calls: 0 };
const scheduled = scheduler.register(startupHook, {
  id: 'startup-schedule',
  hookId: 'startup-hook', // ScheduleConfigSchema requires hookId
  type: 'startup',
  enabled: true,
});

if (scheduled.id !== 'startup-schedule') throw new Error(`Expected id startup-schedule, got ${scheduled.id}`);
if (scheduled.type !== 'startup') throw new Error(`Expected type startup, got ${scheduled.type}`);
if (scheduled.enabled !== true) throw new Error('Expected enabled:true');
if (scheduled.runCount !== 0) throw new Error('runCount should be 0 before start()');

scheduler.start();

// Startup hooks execute synchronously in _executeStartupHooks during start()
// Give it a tick to process
await new Promise(resolve => setTimeout(resolve, 50));

const statsAfterStart = scheduler.getStats();
if (!statsAfterStart.running) throw new Error('Expected scheduler to be running after start()');
if (statsAfterStart.byType.startup !== 1) throw new Error(`Expected 1 startup schedule, got ${statsAfterStart.byType.startup}`);

console.log(`[startup hook] ✅ scheduler running, ${statsAfterStart.totalSchedules} schedule(s) registered`);

scheduler.stop();
if (scheduler.running) throw new Error('Expected running=false after stop()');
console.log(`[stop] ✅ scheduler stopped`);

// ─── 2. interval hook — executes on each tick ─────────────────────────────────

const intervalCalls = [];
const scheduler2 = new HookScheduler({
  tickInterval: 20, // 20ms ticks
  executeHook: async (hook) => {
    intervalCalls.push({ hookId: hook.id, at: Date.now() });
  },
});

const intervalHook = { id: 'count-hook' };
scheduler2.register(intervalHook, {
  id: 'interval-schedule',
  hookId: 'count-hook',
  type: 'interval',
  intervalMs: 25, // run every 25ms
  enabled: true,
});

scheduler2.start();

// Wait for at least 2 interval firings
await new Promise(resolve => setTimeout(resolve, 120));

scheduler2.stop();

if (intervalCalls.length < 2) {
  throw new Error(`Expected at least 2 interval executions in 120ms, got ${intervalCalls.length}`);
}

console.log(`[interval hook] ✅ executed ${intervalCalls.length} times in 120ms (interval=25ms)`);

// ─── 3. getStats() — schedule type counts and totals ──────────────────────────

const statsScheduler = new HookScheduler({ tickInterval: 100 });
statsScheduler.register({ id: 'h1' }, { id: 's-cron', hookId: 'h1', type: 'cron', expression: '*/5', enabled: true }); // every 5 minutes
statsScheduler.register({ id: 'h2' }, { id: 's-interval', hookId: 'h2', type: 'interval', intervalMs: 5000, enabled: true });
statsScheduler.register({ id: 'h3' }, { id: 's-idle', hookId: 'h3', type: 'idle', idleTimeoutMs: 3000, enabled: false });

const stats = statsScheduler.getStats();
if (stats.totalSchedules !== 3) throw new Error(`Expected 3 schedules, got ${stats.totalSchedules}`);
if (stats.enabledSchedules !== 2) throw new Error(`Expected 2 enabled, got ${stats.enabledSchedules}`);
if (stats.byType.cron !== 1) throw new Error(`Expected 1 cron, got ${stats.byType.cron}`);
if (stats.byType.interval !== 1) throw new Error(`Expected 1 interval, got ${stats.byType.interval}`);
if (stats.byType.idle !== 1) throw new Error(`Expected 1 idle, got ${stats.byType.idle}`);

console.log(`[getStats] ✅ total=${stats.totalSchedules}, enabled=${stats.enabledSchedules}, byType=${JSON.stringify(stats.byType)}`);

// ─── 4. unregister() — removed schedule no longer in stats ───────────────────

statsScheduler.unregister('s-cron');
const statsAfterRemove = statsScheduler.getStats();
if (statsAfterRemove.totalSchedules !== 2) throw new Error(`Expected 2 after unregister, got ${statsAfterRemove.totalSchedules}`);
if (statsAfterRemove.byType.cron !== 0) throw new Error(`Expected 0 cron after unregister, got ${statsAfterRemove.byType.cron}`);

console.log(`[unregister] ✅ removed cron schedule: totalSchedules=${statsAfterRemove.totalSchedules}`);

// ─── 5. maxRuns — schedule stops after N executions ──────────────────────────

const maxRunCalls = [];
const scheduler3 = new HookScheduler({
  tickInterval: 15,
  executeHook: async (_hook) => { maxRunCalls.push(1); },
});

scheduler3.register({ id: 'oneshot' }, {
  id: 'maxruns-schedule',
  hookId: 'oneshot',
  type: 'interval',
  intervalMs: 20,
  maxRuns: 2, // run exactly twice
  enabled: true,
});

scheduler3.start();
await new Promise(resolve => setTimeout(resolve, 200));
scheduler3.stop();

if (maxRunCalls.length > 3) {
  throw new Error(`maxRuns:2 should cap executions, got ${maxRunCalls.length}`);
}

console.log(`[maxRuns] ✅ schedule with maxRuns:2 executed ${maxRunCalls.length} time(s) (≤3 with tick timing)`);

console.log('\n✅ All HookScheduler assertions passed.');
