/**
 * Roundtrip SLA Tracking and Enforcement
 * 
 * Tracks JS→Erlang→JS roundtrip latency and error rates with strict SLA thresholds.
 * Implements poka-yoke to prevent SLA violations.
 * 
 * **SLA Requirements**:
 * - Latency: <10ms per roundtrip
 * - Error Rate: <0.1% (1 error per 1000 roundtrips)
 * 
 * **Poka-Yoke**: Prevents operations that would violate SLA thresholds.
 * 
 * @module roundtrip-sla
 */

// SLA Thresholds (strict)
const MAX_LATENCY_MS = 10;
const MAX_ERROR_RATE = 0.001; // 0.1%

// Operation types
const OPERATION_TYPES = {
  EMIT_EVENT: 'emit_event',
  REGISTER_HOOK: 'register_hook',
  PROCESS_INTENT: 'process_intent',
  EXECUTE_BEAM: 'execute_beam',
};

// In-memory storage for roundtrip tracking
const activeRoundtrips = new Map(); // Map<operationId, { startTime, operationType }>
const stats = new Map(); // Map<operationType, { count, totalLatency, errorCount, lastLatency, lastError }>

/**
 * Generate unique operation ID
 * @returns {string} Unique operation ID
 */
function generateOperationId() {
  return `rt-${Date.now()}-${Math.random().toString(36).substring(2, 9)}`;
}

/**
 * Validate non-empty string (poka-yoke: prevents invalid operation types)
 * @param {string} value - Value to validate
 * @param {string} name - Parameter name for error message
 * @throws {Error} If value is invalid
 */
function validateNonEmptyString(value, name) {
  if (typeof value !== 'string' || value.length === 0) {
    throw new Error(`${name} must be a non-empty string, got: ${typeof value}`);
  }
}

/**
 * Start tracking a roundtrip
 * 
 * **Poka-yoke**: Validates operation type and checks if roundtrip can start without violating SLA
 * 
 * @param {string} operationType - Operation type (emit_event, register_hook, process_intent, execute_beam)
 * @param {string} [operationId] - Optional operation ID (generated if not provided)
 * @returns {string} Operation ID for this roundtrip
 * @throws {Error} If operation type is invalid or SLA would be violated
 */
export function startRoundtrip(operationType, operationId = null) {
  // Poka-yoke: Validate input
  validateNonEmptyString(operationType, 'operationType');
  
  // Poka-yoke: Check if roundtrip can start without violating SLA
  canStartRoundtrip(operationType);
  
  const id = operationId || generateOperationId();
  const startTime = performance.now(); // High-resolution time
  
  activeRoundtrips.set(id, {
    startTime,
    operationType,
  });
  
  return id;
}

/**
 * End tracking a roundtrip
 * 
 * **Poka-yoke**: Validates operation ID and records metrics
 * 
 * @param {string} operationId - Operation ID from startRoundtrip
 * @param {boolean} success - Whether the roundtrip succeeded
 * @param {string} [errorMessage] - Error message if failed
 * @returns {Object} Roundtrip result with latency and SLA compliance
 * @throws {Error} If operation ID is invalid
 */
export function endRoundtrip(operationId, success, errorMessage = null) {
  // Poka-yoke: Validate input
  validateNonEmptyString(operationId, 'operationId');
  
  const roundtrip = activeRoundtrips.get(operationId);
  if (!roundtrip) {
    throw new Error(`Roundtrip not found: ${operationId}`);
  }
  
  const { startTime, operationType } = roundtrip;
  const endTime = performance.now();
  const latency = endTime - startTime; // Milliseconds
  
  // Update stats
  let operationStats = stats.get(operationType);
  if (!operationStats) {
    operationStats = {
      count: 0,
      totalLatency: 0,
      errorCount: 0,
      lastLatency: 0,
      lastError: null,
    };
    stats.set(operationType, operationStats);
  }
  
  operationStats.count++;
  operationStats.totalLatency += latency;
  operationStats.lastLatency = latency;
  
  if (!success) {
    operationStats.errorCount++;
    operationStats.lastError = errorMessage || 'Unknown error';
  }
  
  // Remove from active roundtrips
  activeRoundtrips.delete(operationId);
  
  // Validate latency meets SLA
  const slaMet = latency < MAX_LATENCY_MS && success;
  
  // Log warning if latency exceeds threshold (but allow - may be transient)
  if (latency >= MAX_LATENCY_MS) {
    console.warn(
      `[SLA] Roundtrip latency ${latency.toFixed(2)}ms exceeds threshold ${MAX_LATENCY_MS}ms for ${operationType}`
    );
  }
  
  return {
    operationId,
    operationType,
    latency,
    success,
    slaMet,
    errorMessage: errorMessage || null,
  };
}

/**
 * Get SLA statistics for an operation type
 * 
 * @param {string} operationType - Operation type
 * @returns {Object} SLA statistics
 */
export function getSLAStats(operationType) {
  validateNonEmptyString(operationType, 'operationType');
  
  const operationStats = stats.get(operationType);
  if (!operationStats || operationStats.count === 0) {
    return {
      count: 0,
      averageLatency: 0,
      errorRate: 0,
      lastLatency: 0,
      lastError: null,
      slaCompliant: true,
    };
  }
  
  const averageLatency = operationStats.totalLatency / operationStats.count;
  const errorRate = operationStats.errorCount / operationStats.count;
  const slaCompliant = averageLatency < MAX_LATENCY_MS && errorRate < MAX_ERROR_RATE;
  
  return {
    count: operationStats.count,
    averageLatency,
    errorRate,
    lastLatency: operationStats.lastLatency,
    lastError: operationStats.lastError,
    slaCompliant,
  };
}

/**
 * Check if a roundtrip can start without violating SLA (poka-yoke)
 * 
 * **Poka-yoke**: Prevents operations that would violate error rate threshold
 * 
 * @param {string} operationType - Operation type
 * @returns {boolean} True if roundtrip can start
 * @throws {Error} If error rate would exceed threshold
 */
export function canStartRoundtrip(operationType) {
  validateNonEmptyString(operationType, 'operationType');
  
  const stats = getSLAStats(operationType);
  
  // If no history, allow (first operation)
  if (stats.count === 0) {
    return true;
  }
  
  // Poka-yoke: Check error rate - reject if would exceed threshold
  if (stats.errorRate > MAX_ERROR_RATE) {
    const errorRatePercent = (stats.errorRate * 100).toFixed(2);
    throw new Error(
      `SLA violation prevented: Error rate ${errorRatePercent}% exceeds ${(MAX_ERROR_RATE * 100).toFixed(1)}% threshold for ${operationType}. ` +
      `Current stats: ${stats.errorCount} errors in ${stats.count} roundtrips.`
    );
  }
  
  return true;
}

/**
 * Validate roundtrip latency meets SLA
 * 
 * @param {number} latency - Latency in milliseconds
 * @returns {boolean} True if latency meets SLA
 */
export function validateRoundtripLatency(latency) {
  if (typeof latency !== 'number' || latency < 0) {
    throw new Error(`Latency must be a non-negative number, got: ${typeof latency}`);
  }
  return latency < MAX_LATENCY_MS;
}

/**
 * Validate error rate meets SLA
 * 
 * @param {number} errorRate - Error rate (0-1)
 * @returns {boolean} True if error rate meets SLA
 */
export function validateErrorRate(errorRate) {
  if (typeof errorRate !== 'number' || errorRate < 0 || errorRate > 1) {
    throw new Error(`Error rate must be a number between 0 and 1, got: ${typeof errorRate}`);
  }
  return errorRate < MAX_ERROR_RATE;
}

/**
 * Get SLA compliance report
 * 
 * @returns {Object} SLA compliance report
 */
export function getSLAReport() {
  const report = {
    thresholds: {
      maxLatency: MAX_LATENCY_MS,
      maxErrorRate: MAX_ERROR_RATE,
    },
    operations: {},
    overall: {
      totalRoundtrips: 0,
      totalErrors: 0,
      overallErrorRate: 0,
      overallCompliant: true,
    },
    violations: [],
  };
  
  let totalCount = 0;
  let totalErrors = 0;
  
  for (const [operationType, operationStats] of stats.entries()) {
    const stats = getSLAStats(operationType);
    report.operations[operationType] = stats;
    
    totalCount += stats.count;
    totalErrors += operationStats.errorCount;
    
    // Check for violations
    if (!stats.slaCompliant) {
      report.violations.push({
        operationType,
        reason: stats.averageLatency >= MAX_LATENCY_MS
          ? `Average latency ${stats.averageLatency.toFixed(2)}ms exceeds ${MAX_LATENCY_MS}ms`
          : `Error rate ${(stats.errorRate * 100).toFixed(2)}% exceeds ${(MAX_ERROR_RATE * 100).toFixed(1)}%`,
        stats,
      });
    }
  }
  
  report.overall.totalRoundtrips = totalCount;
  report.overall.totalErrors = totalErrors;
  report.overall.overallErrorRate = totalCount > 0 ? totalErrors / totalCount : 0;
  report.overall.overallCompliant = report.violations.length === 0 && report.overall.overallErrorRate < MAX_ERROR_RATE;
  
  return report;
}

/**
 * Get list of SLA violations
 * 
 * @returns {Array} List of violations
 */
export function getViolations() {
  const report = getSLAReport();
  return report.violations;
}

/**
 * Reset SLA statistics (for testing)
 * 
 * @param {string} [operationType] - Optional operation type to reset (resets all if not provided)
 */
export function resetSLAStats(operationType = null) {
  if (operationType) {
    validateNonEmptyString(operationType, 'operationType');
    stats.delete(operationType);
  } else {
    stats.clear();
  }
  activeRoundtrips.clear();
}

/**
 * Get active roundtrips count
 * 
 * @returns {number} Number of active roundtrips
 */
export function getActiveRoundtripsCount() {
  return activeRoundtrips.size;
}

/**
 * Export operation types for use in other modules
 */
export { OPERATION_TYPES };

