/**
 * @file Error Validation Tests — Chicago TDD
 * @description Verification for error types and retry logic
 *
 * Core Chicago TDD tests focused on process truth:
 * - OperatorError stores all fields correctly
 * - retrySafe flag set correctly
 * - PipelineError includes stage/message/cause
 * - GateError includes gate/message/severity
 * - All are Error instances (instanceof check)
 */

import { describe, it, expect } from 'vitest';
import {
  OperatorError,
  PipelineError,
  GateError,
  OPERATOR_CODES
} from '../../src/error.mjs';

describe('Error Types — Process Truth Verification', () => {

  describe('OperatorError', () => {
    it('stores operator name, code, and message', () => {
      const err = new OperatorError('validate-operator', OPERATOR_CODES.VALIDATE_FORMAT, 'Schema validation failed');
      expect(err.operator).toBe('validate-operator');
      expect(err.code).toBe(OPERATOR_CODES.VALIDATE_FORMAT);
      expect(err.message).toBe('Schema validation failed');
    });

    it('sets retrySafe=false for permanent failures', () => {
      const err = new OperatorError('compile-artifact', OPERATOR_CODES.UNKNOWN, 'Syntax error');
      expect(err.retrySafe).toBe(false);
    });

    it('sets retrySafe=true for transient failures', () => {
      const err = new OperatorError('enrich', OPERATOR_CODES.ENRICH_EXTERNAL, 'Network timeout');
      expect(err.retrySafe).toBe(true);
    });

    it('is instance of Error', () => {
      const err = new OperatorError('test', OPERATOR_CODES.UNKNOWN, 'test message');
      expect(err).toBeInstanceOf(Error);
    });

    it('includes stack trace', () => {
      const err = new OperatorError('test', OPERATOR_CODES.UNKNOWN, 'test message');
      expect(err.stack).toBeDefined();
    });
  });

  describe('PipelineError', () => {
    it('stores stage, message, and cause', () => {
      const cause = new Error('Underlying error');
      const err = new PipelineError('validated', 'Validation gate failed', cause);
      expect(err.stage).toBe('validated');
      expect(err.message).toBe('Validation gate failed');
      expect(err.cause).toBe(cause);
    });

    it('is instance of Error', () => {
      const err = new PipelineError('test', 'test');
      expect(err).toBeInstanceOf(Error);
    });

    it('includes stack trace', () => {
      const err = new PipelineError('test', 'test message');
      expect(err.stack).toBeDefined();
    });
  });

  describe('GateError', () => {
    it('stores gate name, message, and severity', () => {
      const err = new GateError('schema-valid', 'SHACL validation failed', 'critical');
      expect(err.gate).toBe('schema-valid');
      expect(err.message).toBe('SHACL validation failed');
      expect(err.severity).toBe('critical');
    });

    it('defaults severity to major if not specified', () => {
      const err = new GateError('custom-gate', 'Failed');
      expect(err.severity).toBe('major');
    });

    it('is instance of Error', () => {
      const err = new GateError('test', 'test');
      expect(err).toBeInstanceOf(Error);
    });

    it('includes stack trace', () => {
      const err = new GateError('test', 'test message');
      expect(err.stack).toBeDefined();
    });
  });

  describe('Error Classification', () => {
    it('distinguishes retryable from non-retryable errors', () => {
      const retryable = new OperatorError('enrich', OPERATOR_CODES.ENRICH_EXTERNAL, 'Timeout');
      const permanent = new OperatorError('compile', OPERATOR_CODES.UNKNOWN, 'Syntax error');

      expect(retryable.retrySafe).toBe(true);
      expect(permanent.retrySafe).toBe(false);
    });

    it('classifies gate errors by severity', () => {
      const critical = new GateError('schema-valid', 'Failed', 'critical');
      const warning = new GateError('performance-check', 'Slow but acceptable', 'warning');

      expect(critical.severity).toBe('critical');
      expect(warning.severity).toBe('warning');
    });

    it('preserves causal chain in pipeline errors', () => {
      const root = new Error('Database connection failed');
      const pipelineErr = new PipelineError('seeded', 'Failed to load ontology', root);

      expect(pipelineErr.cause).toBe(root);
      expect(pipelineErr.stack).toBeDefined();
      expect(pipelineErr.stack).toBeTruthy();
    });
  });
});
