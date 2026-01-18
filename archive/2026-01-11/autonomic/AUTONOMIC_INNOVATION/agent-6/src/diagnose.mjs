/**
 * @typedef {import('./compiler.mjs').Violation} Violation
 */

/**
 * Generate human-readable diagnostic report
 * @param {Violation[]} violations - Sorted violations
 * @param {string} [profileId] - Profile identifier
 * @returns {string} Formatted diagnostic report
 */
export function diagnosticReport(violations, profileId) {
  if (violations.length === 0) {
    return `✅ All conventions validated (profile: ${profileId || 'unknown'})\n`;
  }

  // Sort violations deterministically:
  // 1. file (alphabetical)
  // 2. line (numerical)
  // 3. column (numerical)
  // 4. type (alphabetical)
  const sorted = [...violations].sort((a, b) => {
    if (a.file !== b.file) return (a.file || '').localeCompare(b.file || '');
    if (a.line !== b.line) return (a.line || 0) - (b.line || 0);
    if (a.column !== b.column) return (a.column || 0) - (b.column || 0);
    return a.type.localeCompare(b.type);
  });

  const lines = [
    `❌ Convention violations found (profile: ${profileId || 'unknown'})`,
    `Total violations: ${violations.length}`,
    '',
  ];

  // Group by file
  const byFile = new Map();
  for (const v of sorted) {
    const key = v.file || '(no file)';
    if (!byFile.has(key)) byFile.set(key, []);
    byFile.get(key).push(v);
  }

  for (const [file, fileViolations] of byFile) {
    lines.push(`📄 ${file}`);

    for (const v of fileViolations) {
      const location = v.line ? `:${v.line}${v.column ? `:${v.column}` : ''}` : '';
      lines.push(`  [${v.type}]${location} ${v.message}`);

      if (v.expected) {
        lines.push(`    Expected: ${v.expected}`);
      }
      if (v.actual) {
        lines.push(`    Actual: ${v.actual}`);
      }
      if (v.suggestion) {
        lines.push(`    💡 Suggestion: ${v.suggestion}`);
      }
      lines.push('');
    }
  }

  return lines.join('\n');
}
