/**
 * KGEN Base Templates - Foundation template system
 *
 * Exports all base template classes and utilities for building
 * deterministic, reusable template components
 */

// Import classes first so they can be used in factory functions
import { KGenTemplateBase } from './template-base.js';
import { KGenMacroTemplates } from './macro-templates.js';
import { KGenFilterTemplates } from './filter-templates.js';
import { KGenSHACLTemplates } from './shacl-templates.js';
import { KGenInjectionTargets } from './injection-targets.js';

// Re-export for external use
export { KGenTemplateBase } from './template-base.js';
export { KGenMacroTemplates } from './macro-templates.js';
export { KGenFilterTemplates } from './filter-templates.js';
export { KGenSHACLTemplates } from './shacl-templates.js';
export { KGenInjectionTargets } from './injection-targets.js';

// Convenience factory function
/**
 *
 */
export function createBaseTemplate(options = {}) {
  return new KGenTemplateBase(options);
}

// Template system factory
/**
 *
 */
export function createTemplateSystem(options = {}) {
  const base = new KGenTemplateBase(options);
  const macros = new KGenMacroTemplates(options);
  const filters = new KGenFilterTemplates(options);
  const shacl = new KGenSHACLTemplates(options);
  const injection = new KGenInjectionTargets(options);
  
  return {
    base,
    macros,
    filters,
    shacl,
    injection,
    
    // Convenience methods
    generateTemplate(templateName, context = {}) {
      return base.generateFromTemplate(templateName, context);
    },
    
    generateMacro(macroName, context = {}) {
      const macro = macros.getMacro(macroName);
      if (!macro) {
        throw new Error(`Macro '${macroName}' not found`);
      }
      return base.engine.renderTemplate(macro.template, context);
    },
    
    generateFilter(filterName, context = {}) {
      return filters.generateFilter(filterName, context);
    },
    
    generateSHACLShape(shapeName, context = {}) {
      return shacl.generateShape(shapeName, context);
    },
    
    generateInjectionTarget(targetName, context = {}) {
      return injection.generateTarget(targetName, context);
    },
    
    // System information
    getStats() {
      return {
        base: base.getStats(),
        macros: macros.getStats(),
        filters: filters.getStats(),
        shacl: shacl.getStats(),
        injection: injection.getStats()
      };
    }
  };
}

// Default export object
const BaseTemplates = {
  get KGenTemplateBase() { return KGenTemplateBase; },
  get KGenMacroTemplates() { return KGenMacroTemplates; },
  get KGenFilterTemplates() { return KGenFilterTemplates; },
  get KGenSHACLTemplates() { return KGenSHACLTemplates; },
  get KGenInjectionTargets() { return KGenInjectionTargets; },
  createBaseTemplate,
  createTemplateSystem
};

export default BaseTemplates;