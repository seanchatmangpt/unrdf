/**
 * Table formatter
 * @module domain/formatters/table
 */

import { table, getBorderCharacters } from 'table';

/**
 * @typedef {Object} TableOptions
 * @property {string[]} [columns] - Column headers
 * @property {'honeywell'|'norc'|'ramac'|'void'} [border='honeywell'] - Border style
 * @property {boolean} [header=true] - Include header row
 * @property {Object<number, {alignment?: 'left'|'center'|'right', width?: number}>} [columnConfig] - Column configuration
 */

/**
 * Format data as ASCII table
 * @param {unknown} data - Data to format (array of objects or 2D array)
 * @param {TableOptions} [options] - Table options
 * @returns {string} ASCII table string
 */
export function tableFormatter(data, options = {}) {
  const { border = 'honeywell', header = true, columns, columnConfig } = options;

  // Convert data to 2D array
  const rows = dataToRows(data, columns);

  if (rows.length === 0) {
    return '(empty)';
  }

  // Build table config
  /** @type {import('table').TableUserConfig} */
  const config = {
    border: getBorderCharacters(border),
    drawHorizontalLine: (lineIndex, rowCount) => {
      return lineIndex === 0 || lineIndex === 1 || lineIndex === rowCount;
    },
  };

  // Add column config if provided
  if (columnConfig) {
    config.columns = columnConfig;
  }

  // Generate header row if needed
  if (header && Array.isArray(data) && data.length > 0 && typeof data[0] === 'object' && !Array.isArray(data[0])) {
    const headerRow = columns ?? Object.keys(data[0]);
    const tableData = [headerRow, ...rows];
    return table(tableData, config);
  }

  return table(rows, config);
}

/**
 * Convert data to 2D array for table
 * @param {unknown} data - Input data
 * @param {string[]} [columns] - Column order
 * @returns {string[][]} 2D array of strings
 */
export function dataToRows(data, columns) {
  // Handle null/undefined
  if (data == null) {
    return [];
  }

  // Handle 2D array
  if (Array.isArray(data) && data.every((row) => Array.isArray(row))) {
    return data.map((row) => row.map((cell) => formatCell(cell)));
  }

  // Handle array of objects
  if (Array.isArray(data) && data.length > 0 && typeof data[0] === 'object') {
    const keys = columns ?? Object.keys(data[0]);
    return data.map((obj) => keys.map((key) => formatCell(obj[key])));
  }

  // Handle single object
  if (typeof data === 'object' && !Array.isArray(data)) {
    const keys = columns ?? Object.keys(data);
    return keys.map((key) => [key, formatCell(data[key])]);
  }

  // Handle array of primitives
  if (Array.isArray(data)) {
    return data.map((item, index) => [String(index), formatCell(item)]);
  }

  // Handle primitive
  return [[formatCell(data)]];
}

/**
 * Format a cell value for display
 * @param {unknown} value - Cell value
 * @returns {string} Formatted string
 */
export function formatCell(value) {
  if (value === null) return 'null';
  if (value === undefined) return '';
  if (typeof value === 'boolean') return value ? 'true' : 'false';
  if (typeof value === 'number') return String(value);
  if (typeof value === 'string') return value;
  if (Array.isArray(value)) return `[${value.length} items]`;
  if (typeof value === 'object') {
    const keys = Object.keys(value);
    return `{${keys.length} keys}`;
  }
  return String(value);
}

/**
 * Create a simple key-value table
 * @param {Record<string, unknown>} data - Key-value pairs
 * @param {Object} [options] - Options
 * @param {string} [options.keyHeader='Key'] - Header for key column
 * @param {string} [options.valueHeader='Value'] - Header for value column
 * @returns {string} ASCII table string
 */
export function keyValueTable(data, options = {}) {
  const { keyHeader = 'Key', valueHeader = 'Value' } = options;
  const rows = [[keyHeader, valueHeader]];
  for (const [key, value] of Object.entries(data)) {
    rows.push([key, formatCell(value)]);
  }
  return table(rows, {
    border: getBorderCharacters('honeywell'),
    drawHorizontalLine: (lineIndex, rowCount) => {
      return lineIndex === 0 || lineIndex === 1 || lineIndex === rowCount;
    },
  });
}

/**
 * Create a list table (single column)
 * @param {string[]} items - List items
 * @param {string} [header='Items'] - Column header
 * @returns {string} ASCII table string
 */
export function listTable(items, header = 'Items') {
  const rows = [[header], ...items.map((item) => [item])];
  return table(rows, {
    border: getBorderCharacters('honeywell'),
    drawHorizontalLine: (lineIndex, rowCount) => {
      return lineIndex === 0 || lineIndex === 1 || lineIndex === rowCount;
    },
  });
}
