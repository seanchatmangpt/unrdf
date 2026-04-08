/**
 * @file Human-Readable Output Formatter
 * @module cli/commands/doctor/formatters/human
 *
 * @description
 * Formats doctor check results in a human-readable format with
 * emoji icons, colors, and clear remediation steps.
 */

/**
 * Format results as human-readable text
 */
export function formatHuman(results) {
  let output = '';
  let totalChecks = 0;
  let passedChecks = 0;
  let warnings = 0;
  let failedChecks = 0;

  // Header
  output += '\n╔══════════════════════════════════════════════════════════╗\n';
  output += '║           UNRDF Doctor - Health Check Report           ║\n';
  output += '╚══════════════════════════════════════════════════════════╝\n';
  output += `📅 ${new Date().toISOString()}\n`;

  // Categories
  for (const category of results.categories) {
    output += `\n${category.category}\n`;
    output += `${'═'.repeat(category.category.length)}\n`;

    for (const check of category.checks) {
      totalChecks++;

      const icon =
        check.status === 'pass'
          ? '✅'
          : check.status === 'warn'
            ? '⚠️ '
            : check.status === 'error'
              ? '💥'
              : '❌';

      output += `${icon} ${check.name}\n`;

      if (check.status !== 'pass') {
        if (check.expected) {
          output += `   Expected: ${check.expected}\n`;
        }
        if (check.actual) {
          output += `   Actual: ${check.actual}\n`;
        }
        if (check.fix) {
          output += `   💡 ${check.fix}\n`;
        }

        // Show violations if present
        if (check.violations && check.violations.length > 0) {
          output += `   Details:\n`;
          for (const violation of check.violations.slice(0, 5)) {
            if (typeof violation === 'string') {
              output += `     • ${violation}\n`;
            } else if (violation.file) {
              output += `     • ${violation.file}${violation.count ? ` (${violation.count} issues)` : ''}\n`;
            } else if (violation.tool) {
              output += `     • ${violation.tool}: ${violation.present ? '✓' : '✗'}\n`;
            }
          }
          if (check.violations.length > 5) {
            output += `     ... and ${check.violations.length - 5} more\n`;
          }
        }
      }

      // Count results
      if (check.status === 'pass') {
        passedChecks++;
      } else if (check.status === 'warn') {
        warnings++;
      } else {
        failedChecks++;
      }
    }
  }

  // Summary
  output += `\n${'─'.repeat(50)}\n`;
  output += `📊 Summary\n`;
  output += `   Total Checks: ${totalChecks}\n`;
  output += `   ✅ Passed: ${passedChecks}\n`;
  if (warnings > 0) {
    output += `   ⚠️  Warnings: ${warnings}\n`;
  }
  if (failedChecks > 0) {
    output += `   ❌ Failed: ${failedChecks}\n`;
  }

  // Overall status
  if (failedChecks === 0 && warnings === 0) {
    output += `\n✅ All systems operational! Everything looks good.\n`;
  } else if (failedChecks === 0) {
    output += `\n⚠️  Doctor found ${warnings} warning(s). Review above for details.\n`;
  } else if (failedChecks <= 3) {
    output += `\n⚠️  Doctor found ${failedChecks} issue(s) and ${warnings} warning(s).\n`;
    output += `   Run with --fix to attempt auto-remediation.\n`;
  } else {
    output += `\n❌ Doctor found ${failedChecks} issue(s) and ${warnings} warning(s).\n`;
    output += `   Some issues require manual intervention. Review above for details.\n`;
  }

  output += `\n${'═'.repeat(50)}\n`;

  return output;
}
