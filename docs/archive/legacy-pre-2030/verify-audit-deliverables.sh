#!/bin/bash
# Verify Receipt Audit Deliverables
# Run: bash verify-audit-deliverables.sh

set -e

echo "=== Receipt Audit Deliverables Verification ==="
echo ""

# Check documentation files
echo "1. Checking documentation files..."
files=(
  "/home/user/unrdf/RECEIPTS-AUDIT-SUMMARY.md"
  "/home/user/unrdf/docs/receipts-audit/receipts-architecture.md"
  "/home/user/unrdf/docs/receipts-audit/quick-reference.md"
  "/home/user/unrdf/docs/receipts-audit/README.md"
  "/home/user/unrdf/docs/receipts-audit/INDEX.md"
)

for file in "${files[@]}"; do
  if [ -f "$file" ]; then
    echo "  ✅ $file"
  else
    echo "  ❌ MISSING: $file"
    exit 1
  fi
done
echo ""

# Check proof files
echo "2. Checking proof files..."
proofs=(
  "/home/user/unrdf/proofs/receipt-tamper-detection.mjs"
  "/home/user/unrdf/proofs/audit-trail-reconstruction.mjs"
)

for proof in "${proofs[@]}"; do
  if [ -f "$proof" ] && [ -x "$proof" ]; then
    echo "  ✅ $proof (executable)"
  else
    echo "  ❌ MISSING or not executable: $proof"
    exit 1
  fi
done
echo ""

# Run proof 1
echo "3. Running Proof 1: Receipt Tamper Detection..."
if node /home/user/unrdf/proofs/receipt-tamper-detection.mjs > /tmp/proof1.log 2>&1; then
  if grep -q "PROOF SUCCESSFUL" /tmp/proof1.log; then
    echo "  ✅ Proof 1 PASSED (tamper detection working)"
  else
    echo "  ❌ Proof 1 FAILED (no success message)"
    cat /tmp/proof1.log
    exit 1
  fi
else
  echo "  ❌ Proof 1 FAILED (execution error)"
  cat /tmp/proof1.log
  exit 1
fi
echo ""

# Run proof 2
echo "4. Running Proof 2: Audit Trail Reconstruction..."
if node /home/user/unrdf/proofs/audit-trail-reconstruction.mjs > /tmp/proof2.log 2>&1; then
  if grep -q "PROOF SUCCESSFUL" /tmp/proof2.log; then
    echo "  ✅ Proof 2 PASSED (audit trail reconstruction working)"
  else
    echo "  ❌ Proof 2 FAILED (no success message)"
    cat /tmp/proof2.log
    exit 1
  fi
else
  echo "  ❌ Proof 2 FAILED (execution error)"
  cat /tmp/proof2.log
  exit 1
fi
echo ""

# Count lines
echo "5. Counting documentation lines..."
total_doc_lines=$(wc -l /home/user/unrdf/docs/receipts-audit/*.md | tail -1 | awk '{print $1}')
total_proof_lines=$(wc -l /home/user/unrdf/proofs/receipt-tamper-detection.mjs /home/user/unrdf/proofs/audit-trail-reconstruction.mjs | tail -1 | awk '{print $1}')
total_summary_lines=$(wc -l /home/user/unrdf/RECEIPTS-AUDIT-SUMMARY.md | awk '{print $1}')

echo "  - Documentation: $total_doc_lines lines"
echo "  - Proofs: $total_proof_lines lines"
echo "  - Summary: $total_summary_lines lines"
echo "  - Total: $((total_doc_lines + total_proof_lines + total_summary_lines)) lines"
echo ""

# Summary
echo "=== Verification Summary ==="
echo ""
echo "✅ All documentation files present (5 files)"
echo "✅ All proof files present and executable (2 files)"
echo "✅ Proof 1 (tamper detection) VERIFIED"
echo "✅ Proof 2 (audit trail) VERIFIED"
echo "✅ Total deliverable lines: $((total_doc_lines + total_proof_lines + total_summary_lines))"
echo ""
echo "Status: ALL CHECKS PASSED ✅"
echo ""

# Cleanup
rm -f /tmp/proof1.log /tmp/proof2.log

exit 0
