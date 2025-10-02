/**
 * @file Tree Formatter
 * @module cli-v2/formatters/tree
 */

/**
 * Format output as tree structure
 * @param {*} data - Data to format
 * @param {Object} options - Formatting options
 * @returns {string} Formatted tree
 */
export function formatTree(data, options = {}) {
  const lines = [];
  formatNode(data, '', true, lines, options);
  return lines.join('\n');
}

function formatNode(node, prefix, isLast, lines, options) {
  const connector = isLast ? '└── ' : '├── ';
  const extension = isLast ? '    ' : '│   ';

  if (typeof node !== 'object' || node === null) {
    lines.push(prefix + connector + String(node));
    return;
  }

  if (Array.isArray(node)) {
    node.forEach((item, i) => {
      const itemIsLast = i === node.length - 1;
      formatNode(item, prefix, itemIsLast, lines, options);
    });
    return;
  }

  const entries = Object.entries(node);
  entries.forEach(([key, value], i) => {
    const isLastEntry = i === entries.length - 1;
    lines.push(prefix + connector + key);

    if (typeof value === 'object' && value !== null) {
      formatNode(value, prefix + extension, isLastEntry, lines, options);
    } else {
      lines.push(prefix + extension + String(value));
    }
  });
}
