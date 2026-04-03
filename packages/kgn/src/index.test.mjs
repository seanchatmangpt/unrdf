/**
 * @file KGN package basic tests
 */

import { describe, it, expect } from 'vitest';
import {
  createEngine,
  createInheritanceEngine,
  createEnhancedEngine,
  TemplateEngine,
  EnhancedTemplateEngine,
} from './index.mjs';

describe('@unrdf/kgn', () => {
  it('should export main engine classes', () => {
    expect(TemplateEngine).toBeDefined();
    expect(EnhancedTemplateEngine).toBeDefined();
  });

  it('should create basic engine', () => {
    const engine = createEngine({ useBasicEngine: true });
    expect(engine).toBeDefined();
    expect(engine).toBeInstanceOf(TemplateEngine);
  });

  it('should create enhanced engine by default', () => {
    const engine = createEngine();
    expect(engine).toBeDefined();
  });

  it('should create inheritance engine', () => {
    const engine = createInheritanceEngine();
    expect(engine).toBeDefined();
  });

  it('should create enhanced engine with options', () => {
    const engine = createEnhancedEngine({ enableCache: false });
    expect(engine).toBeDefined();
  });
});
