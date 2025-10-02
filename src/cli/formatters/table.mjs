/**
 * @file Table Formatter
 * @module cli-v2/formatters/table
 */

/**
 * Format output as ASCII table
 * @param {Array<Object>} data - Array of objects to format
 * @param {Object} options - Formatting options
 * @returns {string} Formatted table
 */
export function formatTable(data, options = {}) {
  if (!Array.isArray(data) || data.length === 0) {
    return 'No data';
  }

  const columns = options.columns || Object.keys(data[0]);
  const headers = options.headers || columns;

  // Calculate column widths
  const widths = headers.map((header, i) => {
    const col = columns[i];
    const values = data.map(row => String(row[col] || ''));
    return Math.max(header.length, ...values.map(v => v.length));
  });

  // Format header row
  const headerRow = headers.map((h, i) => h.padEnd(widths[i])).join(' | ');
  const separator = widths.map(w => '-'.repeat(w)).join('-+-');

  // Format data rows
  const rows = data.map(row =>
    columns.map((col, i) => String(row[col] || '').padEnd(widths[i])).join(' | ')
  );

  return [headerRow, separator, ...rows].join('\n');
}
