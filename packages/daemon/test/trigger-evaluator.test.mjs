/**
 * @file Trigger Evaluator Tests
 * @module @unrdf/daemon/test/trigger-evaluator
 * @description Unit tests for trigger evaluation functions
 */

import { describe, it, expect } from 'vitest';
import {
  evaluateTrigger,
  calculateNextExecutionTime,
  isValidTrigger,
} from '../src/trigger-evaluator.mjs';

describe('Trigger Evaluator', () => {
  describe('evaluateTrigger()', () => {
    it('should evaluate interval trigger when interval has passed', () => {
      // Arrange
      const now = Date.now();
      const trigger = { type: 'interval', value: 5000 };
      const lastExecuted = now - 6000; // 6 seconds ago

      // Act
      const result = evaluateTrigger(trigger, lastExecuted);

      // Assert
      expect(result.shouldExecute).toBe(true);
      expect(result.nextExecutionTime).toBe(0);
    });

    it('should not execute interval trigger when interval has not passed', () => {
      // Arrange
      const now = Date.now();
      const trigger = { type: 'interval', value: 5000 };
      const lastExecuted = now - 3000; // 3 seconds ago

      // Act
      const result = evaluateTrigger(trigger, lastExecuted);

      // Assert
      expect(result.shouldExecute).toBe(false);
      expect(result.nextExecutionTime).toBeGreaterThan(0);
      expect(result.nextExecutionTime).toBeLessThanOrEqual(2000);
    });

    it('should reject invalid interval values', () => {
      // Arrange
      const trigger = { type: 'interval', value: 0 };
      const lastExecuted = Date.now();

      // Act
      const result = evaluateTrigger(trigger, lastExecuted);

      // Assert
      expect(result.shouldExecute).toBe(false);
      expect(result.nextExecutionTime).toBe(Infinity);
    });

    it('should handle cron trigger type', () => {
      // Arrange
      const trigger = { type: 'cron', value: '* * * * *' }; // Every minute
      const lastExecuted = Date.now() - 100000;

      // Act
      const result = evaluateTrigger(trigger, lastExecuted);

      // Assert
      expect(result).toHaveProperty('shouldExecute');
      expect(result).toHaveProperty('nextExecutionTime');
      expect(typeof result.nextExecutionTime).toBe('number');
    });

    it('should handle idle trigger type', () => {
      // Arrange
      const trigger = { type: 'idle', value: 5000 };
      const lastExecuted = Date.now();

      // Act
      const result = evaluateTrigger(trigger, lastExecuted);

      // Assert
      expect(result.shouldExecute).toBe(false);
      expect(result.nextExecutionTime).toBe(Infinity);
    });

    it('should handle reactive trigger type', () => {
      // Arrange
      const trigger = { type: 'reactive', value: null };
      const lastExecuted = Date.now();

      // Act
      const result = evaluateTrigger(trigger, lastExecuted);

      // Assert
      expect(result.shouldExecute).toBe(false);
      expect(result.nextExecutionTime).toBe(Infinity);
    });

    it('should handle event trigger type', () => {
      // Arrange
      const trigger = { type: 'event', value: null };
      const lastExecuted = Date.now();

      // Act
      const result = evaluateTrigger(trigger, lastExecuted);

      // Assert
      expect(result.shouldExecute).toBe(false);
      expect(result.nextExecutionTime).toBe(Infinity);
    });

    it('should handle unknown trigger type', () => {
      // Arrange
      const trigger = { type: 'unknown', value: 5000 };
      const lastExecuted = Date.now();

      // Act
      const result = evaluateTrigger(trigger, lastExecuted);

      // Assert
      expect(result.shouldExecute).toBe(false);
      expect(result.nextExecutionTime).toBe(Infinity);
    });
  });

  describe('calculateNextExecutionTime()', () => {
    it('should calculate next execution time for interval trigger', () => {
      // Arrange
      const now = Date.now();
      const trigger = { type: 'interval', value: 5000 };
      const lastExecuted = now - 3000;

      // Act
      const delayMs = calculateNextExecutionTime(trigger, lastExecuted);

      // Assert
      expect(delayMs).toBeGreaterThan(0);
      expect(delayMs).toBeLessThanOrEqual(2000);
    });

    it('should return 0 when trigger should execute immediately', () => {
      // Arrange
      const now = Date.now();
      const trigger = { type: 'interval', value: 5000 };
      const lastExecuted = now - 6000;

      // Act
      const delayMs = calculateNextExecutionTime(trigger, lastExecuted);

      // Assert
      expect(delayMs).toBe(0);
    });

    it('should return Infinity for non-schedule triggers', () => {
      // Arrange
      const trigger = { type: 'event', value: null };
      const lastExecuted = Date.now();

      // Act
      const delayMs = calculateNextExecutionTime(trigger, lastExecuted);

      // Assert
      expect(delayMs).toBe(Infinity);
    });
  });

  describe('isValidTrigger()', () => {
    it('should validate interval trigger with valid value', () => {
      // Arrange & Act
      const result = isValidTrigger({ type: 'interval', value: 5000 });

      // Assert
      expect(result).toBe(true);
    });

    it('should reject interval trigger with invalid value', () => {
      // Arrange & Act
      const result = isValidTrigger({ type: 'interval', value: 0 });

      // Assert
      expect(result).toBe(false);
    });

    it('should validate cron trigger with valid value', () => {
      // Arrange & Act
      const result = isValidTrigger({ type: 'cron', value: '0 * * * *' });

      // Assert
      expect(result).toBe(true);
    });

    it('should reject cron trigger with invalid value', () => {
      // Arrange & Act
      const result = isValidTrigger({ type: 'cron', value: '' });

      // Assert
      expect(result).toBe(false);
    });

    it('should validate idle trigger with valid value', () => {
      // Arrange & Act
      const result = isValidTrigger({ type: 'idle', value: 30000 });

      // Assert
      expect(result).toBe(true);
    });

    it('should validate reactive trigger', () => {
      // Arrange & Act
      const result = isValidTrigger({ type: 'reactive', value: null });

      // Assert
      expect(result).toBe(true);
    });

    it('should validate event trigger', () => {
      // Arrange & Act
      const result = isValidTrigger({ type: 'event', value: null });

      // Assert
      expect(result).toBe(true);
    });

    it('should reject unknown trigger type', () => {
      // Arrange & Act
      const result = isValidTrigger({ type: 'unknown', value: 5000 });

      // Assert
      expect(result).toBe(false);
    });

    it('should reject null trigger', () => {
      // Arrange & Act
      const result = isValidTrigger(null);

      // Assert
      expect(result).toBe(false);
    });

    it('should reject non-object trigger', () => {
      // Arrange & Act
      const result = isValidTrigger('not an object');

      // Assert
      expect(result).toBe(false);
    });

    it('should reject trigger without type property', () => {
      // Arrange & Act
      const result = isValidTrigger({ value: 5000 });

      // Assert
      expect(result).toBe(false);
    });
  });
});
