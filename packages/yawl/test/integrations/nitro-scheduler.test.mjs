/**
 * @file Nitro Scheduler Tests
 * @module @unrdf/yawl/test/integrations/nitro-scheduler
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { randomUUID } from 'crypto';
import { mkdtemp, rm } from 'fs/promises';
import { tmpdir } from 'os';
import { join } from 'path';
import { NitroScheduler } from '../../src/integrations/nitro-scheduler.mjs';

describe('NitroScheduler', () => {
  let scheduler;
  let mockEngine;
  let tempDir;

  beforeEach(async () => {
    tempDir = await mkdtemp(join(tmpdir(), 'yawl-scheduler-test-'));

    mockEngine = {
      createCase: vi.fn().mockResolvedValue({
        id: randomUUID(),
        workflowId: 'test-workflow',
        status: 'active',
      }),
      workflows: new Map([
        ['test-workflow', { id: 'test-workflow', name: 'Test Workflow' }],
      ]),
    };

    scheduler = new NitroScheduler({
      engine: mockEngine,
      persistPath: join(tempDir, 'schedules.json'),
      tickIntervalMs: 100,
    });
  });

  afterEach(async () => {
    if (scheduler.isRunning) {
      await scheduler.stop();
    }
    if (tempDir) {
      await rm(tempDir, { recursive: true, force: true });
    }
  });

  describe('Initialization', () => {
    it('should create scheduler with valid config', () => {
      expect(scheduler).toBeDefined();
      expect(scheduler.engine).toBe(mockEngine);
      expect(scheduler.isRunning).toBe(false);
    });

    it('should throw error without engine', () => {
      expect(() => new NitroScheduler({})).toThrow('WorkflowEngine instance required');
    });

    it('should throw error with invalid tick interval', () => {
      expect(() => new NitroScheduler({
        engine: mockEngine,
        tickIntervalMs: 50,
      })).toThrow('tickIntervalMs must be between');
    });
  });

  describe('Lifecycle', () => {
    it('should start scheduler', async () => {
      await scheduler.start();
      expect(scheduler.isRunning).toBe(true);
      expect(scheduler.tickInterval).toBeDefined();
    });

    it('should stop scheduler', async () => {
      await scheduler.start();
      await scheduler.stop();
      expect(scheduler.isRunning).toBe(false);
      expect(scheduler.tickInterval).toBeNull();
    });

    it('should throw error if starting already running scheduler', async () => {
      await scheduler.start();
      await expect(scheduler.start()).rejects.toThrow('already running');
    });

    it('should not throw if stopping non-running scheduler', async () => {
      await expect(scheduler.stop()).resolves.toBeUndefined();
    });
  });

  describe('Cron Schedules', () => {
    it('should add cron schedule', async () => {
      const schedule = await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 9 * * *',
        timezone: 'UTC',
        caseData: { test: 'data' },
      });

      expect(schedule).toBeDefined();
      expect(schedule.id).toBeDefined();
      expect(schedule.type).toBe('cron');
      expect(schedule.workflowId).toBe('test-workflow');
      expect(schedule.status).toBe('active');
      expect(schedule.cron.expression).toBe('0 9 * * *');
      expect(schedule.nextExecutionAt).toBeDefined();
    });

    it('should compute next execution correctly', async () => {
      const schedule = await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 9 * * *',
        timezone: 'UTC',
      });

      const next = schedule.nextExecutionAt;
      expect(next).toBeInstanceOf(Date);
      expect(next.getUTCHours()).toBe(9);
      expect(next.getUTCMinutes()).toBe(0);
    });

    it('should handle timezone correctly', async () => {
      const schedule = await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 12 * * *',
        timezone: 'America/New_York',
      });

      expect(schedule.cron.timezone).toBe('America/New_York');
      expect(schedule.nextExecutionAt).toBeDefined();
    });

    it('should validate cron expression', async () => {
      await expect(scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: 'invalid',
        timezone: 'UTC',
      })).rejects.toThrow();
    });

    it('should respect maxExecutions', async () => {
      const schedule = await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '* * * * *',
        timezone: 'UTC',
        maxExecutions: 2,
      });

      expect(schedule.maxExecutions).toBe(2);
      expect(schedule.executionCount).toBe(0);
    });
  });

  describe('Delayed Schedules', () => {
    it('should add delayed schedule', async () => {
      const schedule = await scheduler.addDelayedSchedule({
        workflowId: 'test-workflow',
        delayMs: 5000,
        caseData: { delayed: true },
      });

      expect(schedule).toBeDefined();
      expect(schedule.type).toBe('delayed');
      expect(schedule.delayed.delayMs).toBe(5000);
      expect(schedule.maxExecutions).toBe(1);
      expect(schedule.nextExecutionAt).toBeDefined();
    });

    it('should calculate executeAt correctly', async () => {
      const before = Date.now();
      const schedule = await scheduler.addDelayedSchedule({
        workflowId: 'test-workflow',
        delayMs: 10000,
      });
      const after = Date.now();

      const executeAt = schedule.delayed.executeAt.getTime();
      expect(executeAt).toBeGreaterThanOrEqual(before + 10000);
      expect(executeAt).toBeLessThanOrEqual(after + 10000);
    });

    it('should execute delayed schedule', async () => {
      await scheduler.start();

      const schedule = await scheduler.addDelayedSchedule({
        workflowId: 'test-workflow',
        delayMs: 200,
      });

      await new Promise(resolve => setTimeout(resolve, 300));

      expect(mockEngine.createCase).toHaveBeenCalledWith(
        'test-workflow',
        {},
        expect.objectContaining({
          metadata: expect.objectContaining({
            scheduledExecution: true,
            scheduleId: schedule.id,
          }),
        })
      );

      const updated = scheduler.getSchedule(schedule.id);
      expect(updated.status).toBe('completed');
      expect(updated.executionCount).toBe(1);
    });
  });

  describe('Schedule Management', () => {
    it('should get schedule by ID', async () => {
      const schedule = await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 9 * * *',
      });

      const retrieved = scheduler.getSchedule(schedule.id);
      expect(retrieved).toBeDefined();
      expect(retrieved.id).toBe(schedule.id);
    });

    it('should get all schedules', async () => {
      await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 9 * * *',
      });
      await scheduler.addDelayedSchedule({
        workflowId: 'test-workflow',
        delayMs: 5000,
      });

      const all = scheduler.getAllSchedules();
      expect(all).toHaveLength(2);
    });

    it('should get active schedules', async () => {
      const s1 = await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 9 * * *',
      });
      await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 10 * * *',
      });

      await scheduler.pauseSchedule(s1.id);

      const active = scheduler.getActiveSchedules();
      expect(active).toHaveLength(1);
      expect(active[0].id).not.toBe(s1.id);
    });

    it('should remove schedule', async () => {
      const schedule = await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 9 * * *',
      });

      const removed = await scheduler.removeSchedule(schedule.id);
      expect(removed).toBe(true);

      const retrieved = scheduler.getSchedule(schedule.id);
      expect(retrieved).toBeUndefined();
    });

    it('should return false when removing non-existent schedule', async () => {
      const removed = await scheduler.removeSchedule('non-existent');
      expect(removed).toBe(false);
    });
  });

  describe('Pause/Resume', () => {
    it('should pause schedule', async () => {
      const schedule = await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 9 * * *',
      });

      const paused = await scheduler.pauseSchedule(schedule.id);
      expect(paused.status).toBe('paused');
    });

    it('should resume schedule', async () => {
      const schedule = await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 9 * * *',
      });

      await scheduler.pauseSchedule(schedule.id);
      const resumed = await scheduler.resumeSchedule(schedule.id);
      expect(resumed.status).toBe('active');
    });

    it('should throw error when pausing non-existent schedule', async () => {
      await expect(scheduler.pauseSchedule('non-existent')).rejects.toThrow('not found');
    });

    it('should throw error when resuming non-paused schedule', async () => {
      const schedule = await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 9 * * *',
      });

      await expect(scheduler.resumeSchedule(schedule.id)).rejects.toThrow('not paused');
    });

    it('should not execute paused schedule', async () => {
      await scheduler.start();

      const schedule = await scheduler.addDelayedSchedule({
        workflowId: 'test-workflow',
        delayMs: 200,
      });

      await scheduler.pauseSchedule(schedule.id);

      await new Promise(resolve => setTimeout(resolve, 300));

      expect(mockEngine.createCase).not.toHaveBeenCalled();
    });
  });

  describe('Persistence', () => {
    it('should save schedules to disk', async () => {
      await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 9 * * *',
      });

      await scheduler.saveSchedules();

      const newScheduler = new NitroScheduler({
        engine: mockEngine,
        persistPath: join(tempDir, 'schedules.json'),
      });

      await newScheduler.loadSchedules();
      expect(newScheduler.getAllSchedules()).toHaveLength(1);
    });

    it('should load schedules from disk', async () => {
      const schedule = await scheduler.addCronSchedule({
        workflowId: 'test-workflow',
        expression: '0 9 * * *',
      });

      await scheduler.saveSchedules();

      const newScheduler = new NitroScheduler({
        engine: mockEngine,
        persistPath: join(tempDir, 'schedules.json'),
      });

      await newScheduler.loadSchedules();
      const loaded = newScheduler.getSchedule(schedule.id);
      expect(loaded).toBeDefined();
      expect(loaded.workflowId).toBe('test-workflow');
    });

    it('should handle missing persistence file', async () => {
      const newScheduler = new NitroScheduler({
        engine: mockEngine,
        persistPath: join(tempDir, 'non-existent.json'),
      });

      await expect(newScheduler.loadSchedules()).resolves.toBeUndefined();
      expect(newScheduler.getAllSchedules()).toHaveLength(0);
    });
  });

  describe('Error Handling', () => {
    it('should handle workflow execution error', async () => {
      mockEngine.createCase.mockRejectedValueOnce(new Error('Workflow error'));

      await scheduler.start();

      const schedule = await scheduler.addDelayedSchedule({
        workflowId: 'test-workflow',
        delayMs: 200,
      });

      await new Promise(resolve => setTimeout(resolve, 300));

      const updated = scheduler.getSchedule(schedule.id);
      expect(updated.status).toBe('failed');
    });

    it('should call onError callback on execution failure', async () => {
      const onError = vi.fn();
      const failScheduler = new NitroScheduler({
        engine: mockEngine,
        persistPath: join(tempDir, 'fail-schedules.json'),
        onError,
      });

      mockEngine.createCase.mockRejectedValueOnce(new Error('Test error'));

      await failScheduler.start();

      await failScheduler.addDelayedSchedule({
        workflowId: 'test-workflow',
        delayMs: 200,
      });

      await new Promise(resolve => setTimeout(resolve, 300));

      expect(onError).toHaveBeenCalled();

      await failScheduler.stop();
    });
  });
});
