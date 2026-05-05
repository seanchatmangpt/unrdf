/**
 * Analysis Functions Tests
 */

import { describe, it, expect } from 'vitest';
import * as wasm from '../../pkg/wasm4pm.js';

describe('Analysis - Available Functions List', () => {
  it('should list available analysis functions', () => {
    const functions = wasm.available_analysis_functions();
    expect(functions).toBeTruthy();
    expect(typeof functions).toBe('string');

    const functionsObj = JSON.parse(functions);
    expect(functionsObj.functions).toBeTruthy();
    expect(Array.isArray(functionsObj.functions)).toBe(true);
    expect(functionsObj.functions.length).toBeGreaterThan(0);
  });

  it('should include expected analysis functions', () => {
    const functions = wasm.available_analysis_functions();
    const functionsObj = JSON.parse(functions);

    const functionNames = functionsObj.functions.map((fn: any) => fn.name);
    expect(functionNames).toContain('event_statistics');
    expect(functionNames).toContain('case_duration');
  });
});
