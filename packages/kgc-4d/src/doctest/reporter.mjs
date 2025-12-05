/**
 * Doctest Reporter - Format test output with source context
 * Links failures back to original @example blocks in source code
 */

/**
 * Format doctest failure with source context
 * @param {Object} failure - Test failure object
 * @param {string} sourceFile - Original source file name
 * @param {number} lineNumber - Line number in source
 * @param {string} code - Original @example code
 * @returns {string} Formatted error message
 */
export function formatDoctestFailure(failure, sourceFile, lineNumber, code) {
  const header = `
${'='.repeat(70)}
DOCTEST FAILURE
${'='.repeat(70)}

Source: ${sourceFile}:${lineNumber}
Test: ${failure.name}

Original @example:
${code}

Error:
${failure.message}
${failure.stack ? '\nStack:\n' + failure.stack : ''}

${'='.repeat(70)}
`;

  return header;
}

/**
 * Format doctest summary
 * @param {Array} failures - Array of failure objects
 * @param {number} total - Total doctest count
 * @returns {string} Summary report
 */
export function formatDoctestSummary(failures, total) {
  const passed = total - failures.length;
  const percentage = total > 0 ? ((passed / total) * 100).toFixed(1) : 0;

  return `
${'='.repeat(70)}
DOCTEST SUMMARY
${'='.repeat(70)}

Total: ${total}
Passed: ${passed}
Failed: ${failures.length}
Success Rate: ${percentage}%

${failures.length > 0 ? `Failed Tests:\n${failures.map(f => `  - ${f.name}`).join('\n')}` : 'All doctests passed! ✅'}

${'='.repeat(70)}
`;
}

/**
 * Parse Vitest test result and extract doctest metadata
 * @param {Object} testResult - Vitest test result
 * @returns {Object} Extracted metadata {sourceFile, lineNumber, code, functionName}
 */
export function extractDoctestMetadata(testResult) {
  const match = testResult.name.match(/Doctests:\s+(\w+\.mjs).*line\s+(\d+)/);

  if (match) {
    return {
      sourceFile: match[1],
      lineNumber: parseInt(match[2], 10)
    };
  }

  return null;
}

/**
 * Generate HTML report for doctests
 * @param {Object} report - Test report object
 * @returns {string} HTML report
 */
export function generateHTMLReport(report) {
  const { totalExamples, passedExamples, failedExamples, failures } = report;

  return `<!DOCTYPE html>
<html>
<head>
  <title>Doctest Report</title>
  <style>
    body { font-family: monospace; margin: 20px; }
    h1 { color: #333; }
    .summary { margin: 20px 0; padding: 10px; background: #f5f5f5; border-radius: 5px; }
    .passed { color: green; }
    .failed { color: red; }
    .failure { margin: 10px 0; padding: 10px; background: #ffe0e0; border-left: 3px solid red; }
    .code { background: #f0f0f0; padding: 10px; font-size: 12px; overflow-x: auto; }
  </style>
</head>
<body>
  <h1>Doctest Report</h1>
  <div class="summary">
    <p>Total: ${totalExamples}</p>
    <p class="passed">Passed: ${passedExamples}</p>
    ${failedExamples > 0 ? `<p class="failed">Failed: ${failedExamples}</p>` : ''}
  </div>
  ${failures.map(f => `
    <div class="failure">
      <h3>${f.sourceFile}:${f.lineNumber}</h3>
      <p><strong>Test:</strong> ${f.name}</p>
      <p><strong>Error:</strong> ${f.error}</p>
      <div class="code">${f.code}</div>
    </div>
  `).join('')}
</body>
</html>
`;
}

export default {
  formatDoctestFailure,
  formatDoctestSummary,
  extractDoctestMetadata,
  generateHTMLReport
};
