/**
 * @file Manufacturing Error Types
 * @module manufacturing/error
 */

/**
 *
 */
export class OperatorError extends Error {
  /**
   *
   */
  constructor(operator, code, message, context = {}) {
    super(message);
    this.name = 'OperatorError';
    this.operator = operator;
    this.code = code;
    this.context = context;
    this.timestamp = new Date().toISOString();
    this.retrySafe = RETRY_SAFE_OPERATORS.has(operator);
  }

  /**
   *
   */
  toJSON() {
    return {
      name: this.name,
      operator: this.operator,
      code: this.code,
      message: this.message,
      context: this.context,
      timestamp: this.timestamp,
      retrySafe: this.retrySafe,
    };
  }
}

/**
 *
 */
export class PipelineError extends Error {
  /**
   *
   */
  constructor(stage, message, cause = null) {
    super(message);
    this.name = 'PipelineError';
    this.stage = stage;
    this.cause = cause;
    this.timestamp = new Date().toISOString();
  }
}

/**
 *
 */
export class GateError extends Error {
  /**
   *
   */
  constructor(gate, message, severity = 'major') {
    super(message);
    this.name = 'GateError';
    this.gate = gate;
    this.severity = severity;
    this.timestamp = new Date().toISOString();
  }
}

const RETRY_SAFE_OPERATORS = new Set(['enrich', 'monitor', 'sandbox']);

export const OPERATOR_CODES = {
  VALIDATE_FORMAT: 'INVALID_FORMAT',
  VALIDATE_TYPE: 'INVALID_TYPE',
  TRANSFORM_CONVERSION: 'CONVERSION_ERROR',
  TRANSFORM_UNSUPPORTED: 'UNSUPPORTED_TRANSFORMATION',
  ENRICH_EXTERNAL: 'EXTERNAL_API_ERROR',
  ENRICH_NOT_FOUND: 'ENRICHMENT_NOT_FOUND',
  FILTER_LOGIC: 'FILTER_LOGIC_ERROR',
  AGGREGATE_GROUPING: 'GROUPING_ERROR',
  AGGREGATE_FUNCTION: 'UNKNOWN_AGGREGATION',
  DERIVE_RULE: 'RULE_EVALUATION_ERROR',
  DERIVE_CYCLE: 'DERIVATION_CYCLE',
  MONITOR_EMIT: 'TELEMETRY_EMIT_ERROR',
  SANDBOX_TRANSACTION: 'TRANSACTION_ERROR',
  SANDBOX_AUDIT: 'AUDIT_LOG_ERROR',
  TIMEOUT: 'OPERATION_TIMEOUT',
  UNKNOWN: 'UNKNOWN_ERROR',
};
