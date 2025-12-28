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
export function render(template, data) {
  let result = template;

  // Handle {{#section}}...{{/section}} (conditional blocks)
  result = result.replace(/\{\{#(\w+)\}\}([\s\S]*?)\{\{\/\1\}\}/g, (match, key, content) => {
    const value = data[key];
    if (!value) return '';
    if (Array.isArray(value)) {
      return value.map((item, index) => {
        let itemContent = content;
        // Handle {{.}} for simple values
        if (typeof item === 'string' || typeof item === 'number') {
          itemContent = itemContent.replace(/\{\{\.\}\}/g, String(item));
        } else {
          // Handle nested object properties
          itemContent = renderSimpleVars(itemContent, { ...item, stepNumber: index + 1 });
        }
        return itemContent;
      }).join('');
    }
    return renderSimpleVars(content, data);
  });

  // Handle simple {{variable}} replacements
  result = renderSimpleVars(result, data);

  return result;
}

/**
 * Render simple variable replacements
 * @param {string} template - Template string
 * @param {Object} data - Data object
 * @returns {string} Rendered string
 */
function renderSimpleVars(template, data) {
  return template.replace(/\{\{(\w+)\}\}/g, (match, key) => {
    const value = data[key];
    if (value === undefined || value === null) return '';
    return String(value);
  });
}
