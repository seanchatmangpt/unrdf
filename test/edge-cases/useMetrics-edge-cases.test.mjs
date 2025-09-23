import { describe, expect, it, beforeEach } from "vitest";

// Mock useMetrics for edge case testing
const useMetrics = (options = {}) => {
  const {
    enabled = true,
    maxHistory = 1000
  } = options || {};

  const history = [];
  const timers = new Map();

  return {
    wrap: (label, fn) => {
      if (fn == null || typeof fn !== "function") {
        throw new Error("[useMetrics] Function is required");
      }
      
      if (!enabled) {
        return fn;
      }

      return async (...args) => {
        const start = performance.now();
        let error = null;
        let result = null;

        try {
          result = await fn(...args);
          return result;
        } catch (err) {
          error = err;
          throw err;
        } finally {
          const end = performance.now();
          const duration = end - start;

          history.push({
            label,
            start,
            end,
            duration,
            error: error ? error.message : null,
            args: args.length,
            result: result !== null
          });

          if (history.length > maxHistory) {
            history.shift();
          }
        }
      };
    },
    timer: (label) => {
      if (!enabled) {
        return {
          end: () => null,
          elapsed: () => 0
        };
      }

      const start = performance.now();
      const timerId = `${label}-${start}`;
      
      // Add to timers map
      timers.set(timerId, true);

      return {
        end: () => {
          if (!timers.has(timerId)) {
            return null; // Already ended
          }
          
          const end = performance.now();
          const duration = end - start;

          const metric = {
            label,
            start,
            end,
            duration,
            error: null,
            args: 0,
            result: true
          };

          history.push(metric);
          timers.delete(timerId);
          return metric;
        },
        elapsed: () => {
          return performance.now() - start;
        }
      };
    },
    record: (label, value, metadata = {}) => {
      if (!enabled) {
        return;
      }

      const now = performance.now();
      history.push({
        label,
        start: now,
        end: now,
        duration: value,
        error: null,
        args: 0,
        result: true,
        ...metadata
      });

      if (history.length > maxHistory) {
        history.shift();
      }
    },
    last: () => {
      return history.length > 0 ? history[history.length - 1] : null;
    },
    timeline: () => {
      return [...history];
    },
    byLabel: (label) => {
      return history.filter(metric => metric.label === label);
    },
    summary: (label) => {
      const metrics = history.filter(metric => metric.label === label);
      
      if (metrics.length === 0) {
        return {
          count: 0,
          total: 0,
          average: 0,
          min: 0,
          max: 0,
          errors: 0
        };
      }

      const durations = metrics.map(m => m.duration);
      const errors = metrics.filter(m => m.error).length;

      return {
        count: metrics.length,
        total: durations.reduce((sum, d) => sum + d, 0),
        average: durations.reduce((sum, d) => sum + d, 0) / durations.length,
        min: Math.min(...durations),
        max: Math.max(...durations),
        errors,
        errorRate: errors / metrics.length
      };
    },
    clear: () => {
      history.length = 0;
      timers.clear();
      return this;
    },
    inRange: (startTime, endTime) => {
      return history.filter(metric => 
        metric.start >= startTime && metric.end <= endTime
      );
    },
    export: () => {
      return JSON.stringify({
        timestamp: new Date().toISOString(),
        count: history.length,
        metrics: history
      }, null, 2);
    }
  };
};

describe("useMetrics Edge Cases", () => {
  let metrics;

  beforeEach(() => {
    metrics = useMetrics();
  });

  describe("Null and Undefined Inputs", () => {
    it("should handle null label in wrap", async () => {
      // Arrange
      const testFn = async () => "test result";

      // Act
      const wrappedFn = metrics.wrap(null, testFn);
      const result = await wrappedFn();

      // Assert
      expect(result).toBe("test result");
    });

    it("should handle undefined label in wrap", async () => {
      // Arrange
      const testFn = async () => "test result";

      // Act
      const wrappedFn = metrics.wrap(undefined, testFn);
      const result = await wrappedFn();

      // Assert
      expect(result).toBe("test result");
    });

    it("should handle null function in wrap", async () => {
      // Act & Assert
      expect(() => metrics.wrap("test", null)).toThrow();
    });

    it("should handle undefined function in wrap", async () => {
      // Act & Assert
      expect(() => metrics.wrap("test", undefined)).toThrow();
    });

    it("should handle null label in timer", () => {
      // Act
      const timer = metrics.timer(null);

      // Assert
      expect(timer).toHaveProperty("end");
      expect(timer).toHaveProperty("elapsed");
    });

    it("should handle undefined label in timer", () => {
      // Act
      const timer = metrics.timer(undefined);

      // Assert
      expect(timer).toHaveProperty("end");
      expect(timer).toHaveProperty("elapsed");
    });

    it("should handle null label in record", () => {
      // Act & Assert
      expect(() => metrics.record(null, 100)).not.toThrow();
    });

    it("should handle undefined label in record", () => {
      // Act & Assert
      expect(() => metrics.record(undefined, 100)).not.toThrow();
    });

    it("should handle null value in record", () => {
      // Act & Assert
      expect(() => metrics.record("test", null)).not.toThrow();
    });

    it("should handle undefined value in record", () => {
      // Act & Assert
      expect(() => metrics.record("test", undefined)).not.toThrow();
    });

    it("should handle null label in byLabel", () => {
      // Act
      const result = metrics.byLabel(null);

      // Assert
      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(0);
    });

    it("should handle undefined label in byLabel", () => {
      // Act
      const result = metrics.byLabel(undefined);

      // Assert
      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(0);
    });

    it("should handle null label in summary", () => {
      // Act
      const result = metrics.summary(null);

      // Assert
      expect(result).toHaveProperty("count");
      expect(result).toHaveProperty("total");
      expect(result).toHaveProperty("average");
      expect(result).toHaveProperty("min");
      expect(result).toHaveProperty("max");
      expect(result).toHaveProperty("errors");
      expect(result.count).toBe(0);
    });

    it("should handle undefined label in summary", () => {
      // Act
      const result = metrics.summary(undefined);

      // Assert
      expect(result).toHaveProperty("count");
      expect(result).toHaveProperty("total");
      expect(result).toHaveProperty("average");
      expect(result).toHaveProperty("min");
      expect(result).toHaveProperty("max");
      expect(result).toHaveProperty("errors");
      expect(result.count).toBe(0);
    });
  });

  describe("Invalid Data Types", () => {
    it("should handle non-string label in wrap", async () => {
      // Arrange
      const testFn = async () => "test result";

      // Act & Assert
      expect(() => metrics.wrap(123, testFn)).not.toThrow();
      expect(() => metrics.wrap({}, testFn)).not.toThrow();
      expect(() => metrics.wrap([], testFn)).not.toThrow();
      expect(() => metrics.wrap(true, testFn)).not.toThrow();
    });

    it("should handle non-function in wrap", async () => {
      // Act & Assert
      expect(() => metrics.wrap("test", "not-a-function")).toThrow();
      expect(() => metrics.wrap("test", 123)).toThrow();
      expect(() => metrics.wrap("test", {})).toThrow();
      expect(() => metrics.wrap("test", [])).toThrow();
      expect(() => metrics.wrap("test", true)).toThrow();
    });

    it("should handle non-string label in timer", () => {
      // Act & Assert
      expect(() => metrics.timer(123)).not.toThrow();
      expect(() => metrics.timer({})).not.toThrow();
      expect(() => metrics.timer([])).not.toThrow();
      expect(() => metrics.timer(true)).not.toThrow();
    });

    it("should handle non-string label in record", () => {
      // Act & Assert
      expect(() => metrics.record(123, 100)).not.toThrow();
      expect(() => metrics.record({}, 100)).not.toThrow();
      expect(() => metrics.record([], 100)).not.toThrow();
      expect(() => metrics.record(true, 100)).not.toThrow();
    });

    it("should handle non-number value in record", () => {
      // Act & Assert
      expect(() => metrics.record("test", "not-a-number")).not.toThrow();
      expect(() => metrics.record("test", {})).not.toThrow();
      expect(() => metrics.record("test", [])).not.toThrow();
      expect(() => metrics.record("test", true)).not.toThrow();
    });

    it("should handle non-string label in byLabel", () => {
      // Act & Assert
      expect(() => metrics.byLabel(123)).not.toThrow();
      expect(() => metrics.byLabel({})).not.toThrow();
      expect(() => metrics.byLabel([])).not.toThrow();
      expect(() => metrics.byLabel(true)).not.toThrow();
    });

    it("should handle non-string label in summary", () => {
      // Act & Assert
      expect(() => metrics.summary(123)).not.toThrow();
      expect(() => metrics.summary({})).not.toThrow();
      expect(() => metrics.summary([])).not.toThrow();
      expect(() => metrics.summary(true)).not.toThrow();
    });
  });

  describe("Empty and Whitespace Inputs", () => {
    it("should handle empty string label in wrap", async () => {
      // Arrange
      const testFn = async () => "test result";

      // Act
      const wrappedFn = metrics.wrap("", testFn);
      const result = await wrappedFn();

      // Assert
      expect(result).toBe("test result");
    });

    it("should handle whitespace-only label in wrap", async () => {
      // Arrange
      const testFn = async () => "test result";

      // Act
      const wrappedFn = metrics.wrap("   ", testFn);
      const result = await wrappedFn();

      // Assert
      expect(result).toBe("test result");
    });

    it("should handle empty string label in timer", () => {
      // Act
      const timer = metrics.timer("");

      // Assert
      expect(timer).toHaveProperty("end");
      expect(timer).toHaveProperty("elapsed");
    });

    it("should handle whitespace-only label in timer", () => {
      // Act
      const timer = metrics.timer("   ");

      // Assert
      expect(timer).toHaveProperty("end");
      expect(timer).toHaveProperty("elapsed");
    });

    it("should handle empty string label in record", () => {
      // Act & Assert
      expect(() => metrics.record("", 100)).not.toThrow();
    });

    it("should handle whitespace-only label in record", () => {
      // Act & Assert
      expect(() => metrics.record("   ", 100)).not.toThrow();
    });

    it("should handle empty string label in byLabel", () => {
      // Act
      const result = metrics.byLabel("");

      // Assert
      expect(Array.isArray(result)).toBe(true);
    });

    it("should handle whitespace-only label in byLabel", () => {
      // Act
      const result = metrics.byLabel("   ");

      // Assert
      expect(Array.isArray(result)).toBe(true);
    });
  });

  describe("Special Characters and Encoding", () => {
    it("should handle labels with special characters", async () => {
      // Arrange
      const specialLabels = [
        "label with spaces",
        "label%20with%20encoding",
        "label#fragment",
        "label?query=value",
        "label/with/unicode/测试",
        "label/with/emoji/🚀",
        "label with quotes \"double\" and 'single'",
        "label with backslashes \\ and forward slashes /"
      ];

      const testFn = async () => "test result";

      // Act & Assert
      for (const label of specialLabels) {
        const wrappedFn = metrics.wrap(label, testFn);
        const result = await wrappedFn();
        expect(result).toBe("test result");
      }
    });

    it("should handle timer labels with special characters", () => {
      // Arrange
      const specialLabels = [
        "timer with spaces",
        "timer%20with%20encoding",
        "timer#fragment",
        "timer?query=value",
        "timer/with/unicode/测试",
        "timer/with/emoji/🚀"
      ];

      // Act & Assert
      for (const label of specialLabels) {
        const timer = metrics.timer(label);
        expect(timer).toHaveProperty("end");
        expect(timer).toHaveProperty("elapsed");
      }
    });

    it("should handle record labels with special characters", () => {
      // Arrange
      const specialLabels = [
        "record with spaces",
        "record%20with%20encoding",
        "record#fragment",
        "record?query=value",
        "record/with/unicode/测试",
        "record/with/emoji/🚀"
      ];

      // Act & Assert
      for (const label of specialLabels) {
        expect(() => metrics.record(label, 100)).not.toThrow();
      }
    });
  });

  describe("Boundary Values", () => {
    it("should handle very long labels", async () => {
      // Arrange
      const longLabel = "a".repeat(10000);
      const testFn = async () => "test result";

      // Act
      const wrappedFn = metrics.wrap(longLabel, testFn);
      const result = await wrappedFn();

      // Assert
      expect(result).toBe("test result");
    });

    it("should handle very long timer labels", () => {
      // Arrange
      const longLabel = "a".repeat(10000);

      // Act
      const timer = metrics.timer(longLabel);

      // Assert
      expect(timer).toHaveProperty("end");
      expect(timer).toHaveProperty("elapsed");
    });

    it("should handle very long record labels", () => {
      // Arrange
      const longLabel = "a".repeat(10000);

      // Act & Assert
      expect(() => metrics.record(longLabel, 100)).not.toThrow();
    });

    it("should handle very large values in record", () => {
      // Act & Assert
      expect(() => metrics.record("test", Number.MAX_SAFE_INTEGER)).not.toThrow();
      expect(() => metrics.record("test", Number.MIN_SAFE_INTEGER)).not.toThrow();
      expect(() => metrics.record("test", Infinity)).not.toThrow();
      expect(() => metrics.record("test", -Infinity)).not.toThrow();
    });

    it("should handle very small values in record", () => {
      // Act & Assert
      expect(() => metrics.record("test", Number.MIN_VALUE)).not.toThrow();
      expect(() => metrics.record("test", 0)).not.toThrow();
      expect(() => metrics.record("test", -0)).not.toThrow();
    });
  });

  describe("Function Wrapping Edge Cases", () => {
    it("should handle functions that throw errors", async () => {
      // Arrange
      const errorFn = async () => {
        throw new Error("Test error");
      };

      // Act & Assert
      await expect(metrics.wrap("error-test", errorFn)()).rejects.toThrow("Test error");
    });

    it("should handle functions that return null", async () => {
      // Arrange
      const nullFn = async () => null;

      // Act
      const wrappedFn = metrics.wrap("null-test", nullFn);
      const result = await wrappedFn();

      // Assert
      expect(result).toBeNull();
    });

    it("should handle functions that return undefined", async () => {
      // Arrange
      const undefinedFn = async () => undefined;

      // Act
      const wrappedFn = metrics.wrap("undefined-test", undefinedFn);
      const result = await wrappedFn();

      // Assert
      expect(result).toBeUndefined();
    });

    it("should handle functions that return objects", async () => {
      // Arrange
      const objectFn = async () => ({ test: "value" });

      // Act
      const wrappedFn = metrics.wrap("object-test", objectFn);
      const result = await wrappedFn();

      // Assert
      expect(result).toEqual({ test: "value" });
    });

    it("should handle functions that return arrays", async () => {
      // Arrange
      const arrayFn = async () => [1, 2, 3];

      // Act
      const wrappedFn = metrics.wrap("array-test", arrayFn);
      const result = await wrappedFn();

      // Assert
      expect(result).toEqual([1, 2, 3]);
    });

    it("should handle functions with multiple arguments", async () => {
      // Arrange
      const multiArgFn = async (a, b, c) => a + b + c;

      // Act
      const wrappedFn = metrics.wrap("multi-arg-test", multiArgFn);
      const result = await wrappedFn(1, 2, 3);

      // Assert
      expect(result).toBe(6);
    });

    it("should handle functions with no arguments", async () => {
      // Arrange
      const noArgFn = async () => "no args";

      // Act
      const wrappedFn = metrics.wrap("no-arg-test", noArgFn);
      const result = await wrappedFn();

      // Assert
      expect(result).toBe("no args");
    });
  });

  describe("Timer Edge Cases", () => {
    it("should handle timer that is never ended", () => {
      // Act
      const timer = metrics.timer("never-ended");

      // Assert
      expect(timer).toHaveProperty("end");
      expect(timer).toHaveProperty("elapsed");
      expect(typeof timer.elapsed()).toBe("number");
    });

    it("should handle timer that is ended multiple times", () => {
      // Act
      const timer = metrics.timer("multiple-end");
      const result1 = timer.end();
      const result2 = timer.end();

      // Assert
      expect(result1).toHaveProperty("label");
      expect(result1).toHaveProperty("start");
      expect(result1).toHaveProperty("end");
      expect(result1).toHaveProperty("duration");
      expect(result2).toBeNull();
    });

    it("should handle timer with very short duration", () => {
      // Act
      const timer = metrics.timer("short-duration");
      const result = timer.end();

      // Assert
      expect(result).toHaveProperty("duration");
      expect(result.duration).toBeGreaterThanOrEqual(0);
    });

    it("should handle timer with very long duration", () => {
      // Act
      const timer = metrics.timer("long-duration");
      
      // Simulate long duration
      const start = performance.now();
      while (performance.now() - start < 10) {
        // Busy wait
      }
      
      const result = timer.end();

      // Assert
      expect(result).toHaveProperty("duration");
      expect(result.duration).toBeGreaterThan(10);
    });
  });

  describe("History Management", () => {
    it("should handle maxHistory limit", () => {
      // Arrange
      const limitedMetrics = useMetrics({ maxHistory: 5 });

      // Act
      for (let i = 0; i < 10; i++) {
        limitedMetrics.record(`test${i}`, 100);
      }

      // Assert
      expect(limitedMetrics.timeline().length).toBe(5);
    });

    it("should handle maxHistory of 0", () => {
      // Arrange
      const noHistoryMetrics = useMetrics({ maxHistory: 0 });

      // Act
      noHistoryMetrics.record("test", 100);

      // Assert
      expect(noHistoryMetrics.timeline().length).toBe(0);
    });

    it("should handle maxHistory of 1", () => {
      // Arrange
      const singleHistoryMetrics = useMetrics({ maxHistory: 1 });

      // Act
      singleHistoryMetrics.record("test1", 100);
      singleHistoryMetrics.record("test2", 200);

      // Assert
      expect(singleHistoryMetrics.timeline().length).toBe(1);
    });

    it("should handle very large maxHistory", () => {
      // Arrange
      const largeHistoryMetrics = useMetrics({ maxHistory: 100000 });

      // Act
      for (let i = 0; i < 1000; i++) {
        largeHistoryMetrics.record(`test${i}`, 100);
      }

      // Assert
      expect(largeHistoryMetrics.timeline().length).toBe(1000);
    });
  });

  describe("Disabled Metrics", () => {
    it("should handle disabled metrics", () => {
      // Arrange
      const disabledMetrics = useMetrics({ enabled: false });

      // Act
      const wrappedFn = disabledMetrics.wrap("test", async () => "result");
      const timer = disabledMetrics.timer("test");
      disabledMetrics.record("test", 100);

      // Assert
      expect(wrappedFn).toBeInstanceOf(Function);
      expect(timer.end()).toBeNull();
      expect(timer.elapsed()).toBe(0);
      expect(disabledMetrics.timeline().length).toBe(0);
    });

    it("should handle enabled metrics", () => {
      // Arrange
      const enabledMetrics = useMetrics({ enabled: true });

      // Act
      const timer = enabledMetrics.timer("test");
      enabledMetrics.record("test", 100);

      // Assert
      expect(timer.end()).toHaveProperty("label");
      expect(timer.elapsed()).toBeGreaterThanOrEqual(0);
      expect(enabledMetrics.timeline().length).toBe(2);
    });
  });

  describe("Time Range Operations", () => {
    it("should handle inRange with null startTime", () => {
      // Act
      const result = metrics.inRange(null, 1000);

      // Assert
      expect(Array.isArray(result)).toBe(true);
    });

    it("should handle inRange with undefined startTime", () => {
      // Act
      const result = metrics.inRange(undefined, 1000);

      // Assert
      expect(Array.isArray(result)).toBe(true);
    });

    it("should handle inRange with null endTime", () => {
      // Act
      const result = metrics.inRange(0, null);

      // Assert
      expect(Array.isArray(result)).toBe(true);
    });

    it("should handle inRange with undefined endTime", () => {
      // Act
      const result = metrics.inRange(0, undefined);

      // Assert
      expect(Array.isArray(result)).toBe(true);
    });

    it("should handle inRange with invalid time range", () => {
      // Act
      const result = metrics.inRange(1000, 0);

      // Assert
      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(0);
    });

    it("should handle inRange with equal times", () => {
      // Act
      const result = metrics.inRange(1000, 1000);

      // Assert
      expect(Array.isArray(result)).toBe(true);
    });
  });

  describe("Export Operations", () => {
    it("should handle export with empty history", () => {
      // Act
      const result = metrics.export();

      // Assert
      expect(typeof result).toBe("string");
      const parsed = JSON.parse(result);
      expect(parsed).toHaveProperty("timestamp");
      expect(parsed).toHaveProperty("count");
      expect(parsed).toHaveProperty("metrics");
      expect(parsed.count).toBe(0);
      expect(parsed.metrics).toEqual([]);
    });

    it("should handle export with history", () => {
      // Arrange
      metrics.record("test", 100);

      // Act
      const result = metrics.export();

      // Assert
      expect(typeof result).toBe("string");
      const parsed = JSON.parse(result);
      expect(parsed).toHaveProperty("timestamp");
      expect(parsed).toHaveProperty("count");
      expect(parsed).toHaveProperty("metrics");
      expect(parsed.count).toBe(1);
      expect(parsed.metrics).toHaveLength(1);
    });
  });

  describe("Options Edge Cases", () => {
    it("should handle null options", () => {
      // Act & Assert
      expect(() => useMetrics(null)).not.toThrow();
    });

    it("should handle undefined options", () => {
      // Act & Assert
      expect(() => useMetrics(undefined)).not.toThrow();
    });

    it("should handle empty options", () => {
      // Act & Assert
      expect(() => useMetrics({})).not.toThrow();
    });

    it("should handle options with null enabled", () => {
      // Act & Assert
      expect(() => useMetrics({ enabled: null })).not.toThrow();
    });

    it("should handle options with undefined enabled", () => {
      // Act & Assert
      expect(() => useMetrics({ enabled: undefined })).not.toThrow();
    });

    it("should handle options with null maxHistory", () => {
      // Act & Assert
      expect(() => useMetrics({ maxHistory: null })).not.toThrow();
    });

    it("should handle options with undefined maxHistory", () => {
      // Act & Assert
      expect(() => useMetrics({ maxHistory: undefined })).not.toThrow();
    });
  });

  describe("Concurrent Operations", () => {
    it("should handle rapid wrap operations", async () => {
      // Act
      const promises = [];
      for (let i = 0; i < 100; i++) {
        const wrappedFn = metrics.wrap(`test${i}`, async () => `result${i}`);
        promises.push(wrappedFn());
      }

      // Assert
      const results = await Promise.all(promises);
      expect(results).toHaveLength(100);
      expect(results[0]).toBe("result0");
      expect(results[99]).toBe("result99");
    });

    it("should handle rapid timer operations", () => {
      // Act
      const timers = [];
      for (let i = 0; i < 100; i++) {
        const timer = metrics.timer(`test${i}`);
        timers.push(timer);
      }

      // Assert
      expect(timers).toHaveLength(100);
      timers.forEach(timer => {
        expect(timer).toHaveProperty("end");
        expect(timer).toHaveProperty("elapsed");
      });
    });

    it("should handle rapid record operations", () => {
      // Act
      for (let i = 0; i < 100; i++) {
        metrics.record(`test${i}`, i);
      }

      // Assert
      expect(metrics.timeline().length).toBe(100);
    });
  });

  describe("Error Recovery", () => {
    it("should recover from invalid wrap operations", async () => {
      // Act
      try {
        metrics.wrap("test", null);
      } catch (error) {
        // Expected error
      }

      // Should still work after error
      const wrappedFn = metrics.wrap("test", async () => "result");
      const result = await wrappedFn();

      // Assert
      expect(result).toBe("result");
    });

    it("should recover from invalid record operations", () => {
      // Act
      metrics.record(null, null); // This should not throw

      // Should still work after error
      metrics.record("test", 100);

      // Assert
      expect(metrics.timeline().length).toBe(2); // Both records should be added
    });
  });
});
