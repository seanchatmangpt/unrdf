/**
 * @file Logger Tests
 * @description Tests for structured logger
 */

import { describe, it, expect } from 'vitest';
import { createLogger, performanceTimer } from '../src/logger.mjs';
import { Writable } from 'stream';

describe('Logger System', () => {
  describe('createLogger', () => {
    it('should create logger instance', () => {
      const logger = createLogger({
        service: 'test-service',
        level: 'info'
      });

      expect(logger).toHaveProperty('trace');
      expect(logger).toHaveProperty('debug');
      expect(logger).toHaveProperty('info');
      expect(logger).toHaveProperty('warn');
      expect(logger).toHaveProperty('error');
      expect(logger).toHaveProperty('fatal');
      expect(logger).toHaveProperty('performance');
      expect(logger).toHaveProperty('slowQuery');
      expect(logger).toHaveProperty('child');
    });

    it('should log info messages', () => {
      const output = [];
      const destination = new Writable({
        write(chunk, encoding, callback) {
          output.push(chunk.toString());
          callback();
        }
      });

      const logger = createLogger({
        service: 'test-service',
        level: 'info',
        destination
      });

      logger.info('test message', { userId: '123' });

      expect(output.length).toBe(1);
      const log = JSON.parse(output[0]);
      expect(log).toMatchObject({
        level: 'info',
        service: 'test-service',
        message: 'test message',
        userId: '123'
      });
      expect(log).toHaveProperty('timestamp');
    });

    it('should log error messages with error objects', () => {
      const output = [];
      const destination = new Writable({
        write(chunk, encoding, callback) {
          output.push(chunk.toString());
          callback();
        }
      });

      const logger = createLogger({
        service: 'test-service',
        level: 'error',
        destination
      });

      const error = new Error('Test error');
      logger.error('operation failed', { operation: 'test' }, error);

      expect(output.length).toBe(1);
      const log = JSON.parse(output[0]);
      expect(log).toMatchObject({
        level: 'error',
        service: 'test-service',
        message: 'operation failed',
        operation: 'test'
      });
      expect(log.error).toMatchObject({
        name: 'Error',
        message: 'Test error'
      });
      expect(log.error).toHaveProperty('stack');
    });

    it('should respect log level filtering', () => {
      const output = [];
      const destination = new Writable({
        write(chunk, encoding, callback) {
          output.push(chunk.toString());
          callback();
        }
      });

      const logger = createLogger({
        service: 'test-service',
        level: 'warn',
        destination
      });

      logger.trace('trace message');
      logger.debug('debug message');
      logger.info('info message');
      logger.warn('warn message');
      logger.error('error message');

      expect(output.length).toBe(2); // Only warn and error
      expect(output[0]).toContain('warn message');
      expect(output[1]).toContain('error message');
    });

    it('should create child logger with additional context', () => {
      const output = [];
      const destination = new Writable({
        write(chunk, encoding, callback) {
          output.push(chunk.toString());
          callback();
        }
      });

      const logger = createLogger({
        service: 'test-service',
        level: 'info',
        context: { requestId: '123' },
        destination
      });

      const child = logger.child({ userId: '456' });
      child.info('child message');

      expect(output.length).toBe(1);
      const log = JSON.parse(output[0]);
      expect(log).toMatchObject({
        requestId: '123',
        userId: '456',
        message: 'child message'
      });
    });

    it('should log performance metrics', () => {
      const output = [];
      const destination = new Writable({
        write(chunk, encoding, callback) {
          output.push(chunk.toString());
          callback();
        }
      });

      const logger = createLogger({
        service: 'test-service',
        level: 'info',
        destination
      });

      logger.performance('query-execution', {
        duration: 42.5,
        rows: 100
      });

      expect(output.length).toBe(1);
      const log = JSON.parse(output[0]);
      expect(log).toMatchObject({
        level: 'info',
        message: 'Performance: query-execution',
        type: 'performance',
        operation: 'query-execution',
        duration: 42.5,
        rows: 100
      });
    });

    it('should detect slow queries', () => {
      const output = [];
      const destination = new Writable({
        write(chunk, encoding, callback) {
          output.push(chunk.toString());
          callback();
        }
      });

      const logger = createLogger({
        service: 'test-service',
        level: 'warn',
        destination
      });

      // Fast query - should not log
      logger.slowQuery('fast-query', 50, 100);
      expect(output.length).toBe(0);

      // Slow query - should log
      logger.slowQuery('slow-query', 150, 100);
      expect(output.length).toBe(1);

      const log = JSON.parse(output[0]);
      expect(log).toMatchObject({
        level: 'warn',
        type: 'slow_query',
        query: 'slow-query',
        duration: 150,
        threshold: 100
      });
    });
  });

  describe('performanceTimer', () => {
    it('should measure duration', async () => {
      const timer = performanceTimer();
      await new Promise(resolve => setTimeout(resolve, 100));
      const metrics = timer.end();

      expect(metrics).toHaveProperty('duration');
      expect(metrics).toHaveProperty('timestamp');
      expect(metrics.duration).toBeGreaterThanOrEqual(100);
      expect(metrics.duration).toBeLessThan(150);
    });

    it('should get elapsed time without ending', async () => {
      const timer = performanceTimer();
      await new Promise(resolve => setTimeout(resolve, 50));
      const elapsed = timer.elapsed();

      // Allow 5ms tolerance for timing variance in CI/test environments
      expect(elapsed).toBeGreaterThanOrEqual(45);
      expect(elapsed).toBeLessThan(110);

      // Timer should still work after elapsed()
      await new Promise(resolve => setTimeout(resolve, 50));
      const final = timer.end();
      expect(final.duration).toBeGreaterThanOrEqual(100);
    });
  });
});
