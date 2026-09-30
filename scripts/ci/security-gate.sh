#!/usr/bin/env bash
# Security gate shared by the PR workflow (ci.yml) and the scheduled deep scan (security.yml).
#   secrets:   zero tolerance for critical findings
#   injection: ratchet against .github/baselines/injection-critical.json (only NEW file|rule findings fail)
# Both scans are a few seconds; the expensive `full-check` / `dashboard` reports are report-only and live in
# security.yml's scheduled job, not here. Update the injection baseline deliberately with:
#   node scripts/ci/findings-ratchet.mjs --report injection-report.json \
#     --baseline .github/baselines/injection-critical.json --update
set -uo pipefail

scan() { # scan <command> <report-basename>
  # The CLI prints a progress banner before the JSON document and exits non-zero when it finds issues;
  # write to a file first (piping stdout truncates at 64KB because the CLI calls process.exit).
  node src/security/cli.mjs "$1" . --json > "$2.json.raw" || true
  sed -n '/^{/,$p' "$2.json.raw" > "$2.json"
}

echo "Running custom secret detection..."
scan secrets secrets-report
SECRETS_CRITICAL=$(jq '.summary.bySeverity.critical // 0' secrets-report.json)
if [ "$SECRETS_CRITICAL" -gt 0 ]; then
  echo "❌ Critical secrets found!"
  jq '.findings[] | select(.severity == "critical")' secrets-report.json
fi

echo "Running injection vulnerability analysis..."
scan injection injection-report
INJECTION_CRITICAL=$(jq '.summary.bySeverity.critical // 0' injection-report.json)
if [ "$INJECTION_CRITICAL" -gt 0 ]; then
  echo "Critical injection findings present (accepted baseline debt is allowed; new debt is not)."
fi

INJECTION_RATCHET=0
node scripts/ci/findings-ratchet.mjs --report injection-report.json \
  --baseline .github/baselines/injection-critical.json || INJECTION_RATCHET=$?

if [ "$SECRETS_CRITICAL" -gt 0 ] || [ "$INJECTION_RATCHET" -ne 0 ]; then
  echo "❌ Security gate FAILED"
  echo "Secrets critical: $SECRETS_CRITICAL (must be 0)"
  echo "Injection ratchet exit code: $INJECTION_RATCHET (0 = nothing new, 1 = new/increased findings, 2 = report unreadable)"
  echo "Injection critical total: $INJECTION_CRITICAL"
  exit 1
fi
echo "✅ Security gate PASSED"
