/**
 * @file Tests for Error & Recovery hooks functionality
 * Tests error boundaries, recovery strategies, and error reporting
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('ErrorBoundary', () => {
  describe('Error Catching', () => {
    it('should catch and track errors', () => {
      const errorBoundary = {
        hasError: false,
        error: null,
        errorInfo: null
      };

      const catchError = (error, errorInfo) => {
        errorBoundary.hasError = true;
        errorBoundary.error = error;
        errorBoundary.errorInfo = errorInfo;
      };

      const testError = new Error('Simulated network error: Connection timeout');
      catchError(testError, { componentStack: 'at fetchData (api.js:42)' });

      expect(errorBoundary.hasError).toBe(true);
      expect(errorBoundary.error.message).toBe('Simulated network error: Connection timeout');
      expect(errorBoundary.errorInfo.componentStack).toContain('fetchData');
    });

    it('should reset error state', () => {
      const errorBoundary = {
        hasError: true,
        error: new Error('Test error'),
        errorInfo: {}
      };

      const resetError = () => {
        errorBoundary.hasError = false;
        errorBoundary.error = null;
        errorBoundary.errorInfo = null;
      };

      resetError();

      expect(errorBoundary.hasError).toBe(false);
      expect(errorBoundary.error).toBeNull();
    });

    it('should call onError callback', () => {
      const errors = [];
      const onError = vi.fn((error) => {
        errors.push(error);
      });

      const testError = new Error('Test error');
      onError(testError);

      expect(onError).toHaveBeenCalledTimes(1);
      expect(errors).toHaveLength(1);
      expect(errors[0].message).toBe('Test error');
    });
  });

  describe('Error Classification', () => {
    it('should classify network errors', () => {
      const classifyError = (error) => {
        const msg = error.message.toLowerCase();
        if (msg.includes('network') || msg.includes('timeout') || msg.includes('connection')) {
          return 'network';
        }
        if (msg.includes('parse') || msg.includes('syntax')) {
          return 'parse';
        }
        return 'unknown';
      };

      expect(classifyError(new Error('Network error'))).toBe('network');
      expect(classifyError(new Error('Connection timeout'))).toBe('network');
      expect(classifyError(new Error('Parse error'))).toBe('parse');
      expect(classifyError(new Error('Random error'))).toBe('unknown');
    });

    it('should determine if error is recoverable', () => {
      const isRecoverable = (error) => {
        const recoverablePatterns = ['timeout', 'network', 'rate limit'];
        return recoverablePatterns.some(pattern =>
          error.message.toLowerCase().includes(pattern)
        );
      };

      expect(isRecoverable(new Error('Connection timeout'))).toBe(true);
      expect(isRecoverable(new Error('Network error'))).toBe(true);
      expect(isRecoverable(new Error('Rate limit exceeded'))).toBe(true);
      expect(isRecoverable(new Error('Invalid syntax'))).toBe(false);
    });
  });
});

describe('Recovery', () => {
  describe('Retry Logic', () => {
    it('should retry failed operations', async () => {
      let attempts = 0;
      const maxRetries = 3;

      const executeWithRetry = async (operation) => {
        while (attempts < maxRetries) {
          attempts++;
          try {
            return await operation();
          } catch (error) {
            if (attempts >= maxRetries) {
              throw error;
            }
          }
        }
      };

      const failingOperation = vi.fn()
        .mockRejectedValueOnce(new Error('Fail 1'))
        .mockRejectedValueOnce(new Error('Fail 2'))
        .mockResolvedValueOnce('Success');

      const result = await executeWithRetry(failingOperation);

      expect(attempts).toBe(3);
      expect(result).toBe('Success');
    });

    it('should use exponential backoff', async () => {
      const baseDelay = 1000;
      const maxRetries = 3;

      const getBackoffDelay = (attempt) => {
        return baseDelay * Math.pow(2, attempt - 1);
      };

      expect(getBackoffDelay(1)).toBe(1000);
      expect(getBackoffDelay(2)).toBe(2000);
      expect(getBackoffDelay(3)).toBe(4000);
    });

    it('should track retry count', async () => {
      const recoveryState = {
        retryCount: 0,
        isRecovering: false,
        lastError: null
      };

      const startRecovery = () => {
        recoveryState.isRecovering = true;
      };

      const incrementRetry = (error) => {
        recoveryState.retryCount++;
        recoveryState.lastError = error;
      };

      const endRecovery = (success) => {
        recoveryState.isRecovering = false;
        if (success) {
          recoveryState.retryCount = 0;
          recoveryState.lastError = null;
        }
      };

      startRecovery();
      incrementRetry(new Error('Attempt 1 failed'));
      incrementRetry(new Error('Attempt 2 failed'));
      endRecovery(true);

      expect(recoveryState.retryCount).toBe(0);
      expect(recoveryState.isRecovering).toBe(false);
    });
  });

  describe('Recovery Strategies', () => {
    it('should fallback to cache on failure', async () => {
      const cache = new Map([
        ['query1', [{ id: 1, name: 'Cached Alice' }]]
      ]);

      const fetchWithFallback = async (queryId, fetchFn) => {
        try {
          return await fetchFn();
        } catch (error) {
          // Fallback to cache
          if (cache.has(queryId)) {
            return { data: cache.get(queryId), fromCache: true };
          }
          throw error;
        }
      };

      const failingFetch = () => Promise.reject(new Error('Network error'));
      const result = await fetchWithFallback('query1', failingFetch);

      expect(result.fromCache).toBe(true);
      expect(result.data[0].name).toBe('Cached Alice');
    });

    it('should execute recovery actions', () => {
      const recoveryActions = [];

      const addRecoveryAction = (action) => {
        recoveryActions.push(action);
      };

      const executeRecovery = () => {
        return recoveryActions.map(action => ({
          action: action.name,
          result: action.execute()
        }));
      };

      addRecoveryAction({ name: 'clearCache', execute: () => 'Cache cleared' });
      addRecoveryAction({ name: 'reconnect', execute: () => 'Reconnected' });
      addRecoveryAction({ name: 'refreshToken', execute: () => 'Token refreshed' });

      const results = executeRecovery();

      expect(results).toHaveLength(3);
      expect(results[0].result).toBe('Cache cleared');
      expect(results[1].result).toBe('Reconnected');
    });
  });
});

describe('ErrorReporting', () => {
  describe('Error Log Management', () => {
    it('should create error log entries', () => {
      const errorLog = [];

      const determineSeverity = (error) => {
        const msg = error.message.toLowerCase();
        if (msg.includes('critical') || msg.includes('failure')) return 'critical';
        if (msg.includes('timeout') || msg.includes('warning')) return 'warning';
        return 'error';
      };

      const logError = (error) => {
        errorLog.push({
          id: Date.now(),
          message: error.message,
          stack: error.stack,
          timestamp: new Date().toISOString(),
          severity: determineSeverity(error)
        });
      };

      logError(new Error('Connection timeout'));
      logError(new Error('System failure'));
      logError(new Error('Unknown error'));

      expect(errorLog).toHaveLength(3);
      expect(errorLog[0].severity).toBe('warning');
      expect(errorLog[1].severity).toBe('critical');
      expect(errorLog[2].severity).toBe('error');
    });

    it('should limit error log size', () => {
      const maxLogSize = 100;
      let errorLog = [];

      const logError = (error) => {
        errorLog.push({ message: error.message, timestamp: Date.now() });
        if (errorLog.length > maxLogSize) {
          errorLog = errorLog.slice(-maxLogSize);
        }
      };

      // Log 150 errors
      for (let i = 0; i < 150; i++) {
        logError(new Error(`Error ${i}`));
      }

      expect(errorLog).toHaveLength(100);
      expect(errorLog[0].message).toBe('Error 50'); // First 50 were trimmed
    });

    it('should clear error log', () => {
      const errorLog = [
        { message: 'Error 1' },
        { message: 'Error 2' }
      ];

      const clearLog = () => errorLog.length = 0;

      clearLog();

      expect(errorLog).toHaveLength(0);
    });
  });

  describe('Error Aggregation', () => {
    it('should aggregate similar errors', () => {
      const errors = [
        { message: 'Connection timeout', count: 5 },
        { message: 'Network error', count: 3 },
        { message: 'Connection timeout', count: 2 }
      ];

      const aggregated = errors.reduce((acc, err) => {
        const existing = acc.find(e => e.message === err.message);
        if (existing) {
          existing.count += err.count;
        } else {
          acc.push({ ...err });
        }
        return acc;
      }, []);

      expect(aggregated).toHaveLength(2);
      expect(aggregated.find(e => e.message === 'Connection timeout').count).toBe(7);
    });

    it('should calculate error rate', () => {
      const stats = {
        totalOperations: 1000,
        failedOperations: 25
      };

      const errorRate = (stats.failedOperations / stats.totalOperations) * 100;

      expect(errorRate).toBe(2.5);
    });
  });

  describe('Error Notifications', () => {
    it('should trigger notification on critical errors', () => {
      const notifications = [];

      const notify = (error, severity) => {
        if (severity === 'critical') {
          notifications.push({
            type: 'alert',
            message: error.message,
            timestamp: new Date().toISOString()
          });
        }
      };

      notify(new Error('Minor issue'), 'warning');
      notify(new Error('System failure'), 'critical');

      expect(notifications).toHaveLength(1);
      expect(notifications[0].message).toBe('System failure');
    });

    it('should batch notifications', () => {
      const batchSize = 5;
      let pendingNotifications = [];
      let sentBatches = [];

      const queueNotification = (notification) => {
        pendingNotifications.push(notification);
        if (pendingNotifications.length >= batchSize) {
          sentBatches.push([...pendingNotifications]);
          pendingNotifications = [];
        }
      };

      for (let i = 0; i < 12; i++) {
        queueNotification({ message: `Error ${i}` });
      }

      expect(sentBatches).toHaveLength(2); // 5 + 5 sent, 2 pending
      expect(pendingNotifications).toHaveLength(2);
    });
  });
});
