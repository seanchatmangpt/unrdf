/**
 * @file Error Types Tests
 */

import { describe, it, expect } from 'vitest';
import { OperatorError, PipelineError, GateError, OPERATOR_CODES } from '../src/error.mjs';

describe('OperatorError', () => {
  it('stores all fields', () => {
    const err = new OperatorError('validate', 'VALIDATE_FORMAT', 'bad format', { field: 'name' });
    expect(err.name).toBe('OperatorError');
    expect(err.operator).toBe('validate');
    expect(err.code).toBe('VALIDATE_FORMAT');
    expect(err.message).toBe('bad format');
    expect(err.context).toEqual({ field: 'name' });
    expect(err.timestamp).toBeDefined();
  });

  it('sets retrySafe based on operator name', () => {
    const retryErr = new OperatorError('enrich', 'ENRICH_EXTERNAL', 'API error');
    expect(retryErr.retrySafe).toBe(true);

    const nonRetryErr = new OperatorError('validate', 'VALIDATE_FORMAT', 'bad format');
    expect(nonRetryErr.retrySafe).toBe(false);
  });

  it('serializes to JSON', () => {
    const err = new OperatorError('sandbox', 'SANDBOX_TRANSACTION', 'tx failed');
    const json = err.toJSON();
    expect(json.name).toBe('OperatorError');
    expect(json.operator).toBe('sandbox');
    expect(json.code).toBe('SANDBOX_TRANSACTION');
    expect(json.retrySafe).toBe(true);
    expect(json.timestamp).toBeDefined();
  });

  it('is an instance of Error', () => {
    const err = new OperatorError('test', 'UNKNOWN', 'msg');
    expect(err).toBeInstanceOf(Error);
    expect(err).toBeInstanceOf(OperatorError);
  });
});

describe('PipelineError', () => {
  it('stores stage and message', () => {
    const err = new PipelineError('validated', 'Gate failed');
    expect(err.name).toBe('PipelineError');
    expect(err.stage).toBe('validated');
    expect(err.message).toBe('Gate failed');
    expect(err.timestamp).toBeDefined();
    expect(err.cause).toBeNull();
  });

  it('stores cause', () => {
    const cause = new Error('root cause');
    const err = new PipelineError('compiled', 'compilation error', cause);
    expect(err.cause).toBe(cause);
  });

  it('is an instance of Error', () => {
    const err = new PipelineError('s', 'm');
    expect(err).toBeInstanceOf(Error);
    expect(err).toBeInstanceOf(PipelineError);
  });
});

describe('GateError', () => {
  it('stores gate, message, and severity', () => {
    const err = new GateError('schema-valid', 'Validation failed', 'critical');
    expect(err.name).toBe('GateError');
    expect(err.gate).toBe('schema-valid');
    expect(err.message).toBe('Validation failed');
    expect(err.severity).toBe('critical');
    expect(err.timestamp).toBeDefined();
  });

  it('defaults severity to major', () => {
    const err = new GateError('gate', 'msg');
    expect(err.severity).toBe('major');
  });

  it('is an instance of Error', () => {
    const err = new GateError('g', 'm');
    expect(err).toBeInstanceOf(Error);
    expect(err).toBeInstanceOf(GateError);
  });
});

describe('OPERATOR_CODES', () => {
  it('defines all expected codes', () => {
    expect(OPERATOR_CODES.VALIDATE_FORMAT).toBe('INVALID_FORMAT');
    expect(OPERATOR_CODES.TRANSFORM_CONVERSION).toBe('CONVERSION_ERROR');
    expect(OPERATOR_CODES.ENRICH_EXTERNAL).toBe('EXTERNAL_API_ERROR');
    expect(OPERATOR_CODES.TIMEOUT).toBe('OPERATION_TIMEOUT');
    expect(OPERATOR_CODES.UNKNOWN).toBe('UNKNOWN_ERROR');
  });
});
