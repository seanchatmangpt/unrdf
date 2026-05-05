/**
 * Simple template renderer (Mustache-like syntax)
 * @module @unrdf/v6-core/docs/template-renderer
 */
/**
 * Render template with data (simple Mustache-like implementation)
 * @param {string} template - Template string with {{variable}} syntax
 * @param {Object} data - Data object
 * @returns {string} Rendered template
 */
export function render(template: string, data: any): string;
