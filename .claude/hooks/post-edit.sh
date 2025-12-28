#!/bin/bash
# Post-edit hook for UNRDF development
# Validates file after editing

set -e

FILE_PATH="$1"

# Skip if no file path provided
if [ -z "$FILE_PATH" ]; then
  exit 0
fi

echo "Post-edit: Validating $FILE_PATH"

# Check file exists after edit
if [ ! -f "$FILE_PATH" ]; then
  echo "WARNING: File does not exist after edit"
  exit 0
fi

# Validate .mjs files
if [[ "$FILE_PATH" == *.mjs ]]; then
  # Check line count
  LINE_COUNT=$(wc -l < "$FILE_PATH")

  if [ "$LINE_COUNT" -gt 500 ]; then
    echo "ERROR: File exceeds 500 line limit ($LINE_COUNT lines)"
    echo "Split into modules before committing"
  fi

  # Check for forbidden patterns
  if grep -q "from 'n3'" "$FILE_PATH" 2>/dev/null; then
    echo "ERROR: Direct n3 import detected. Use @unrdf/oxigraph"
  fi

  # Quick syntax check
  if command -v node &> /dev/null; then
    if ! node --check "$FILE_PATH" 2>/dev/null; then
      echo "ERROR: JavaScript syntax error in $FILE_PATH"
    fi
  fi
fi

# Validate test files
if [[ "$FILE_PATH" == *.test.mjs ]]; then
  # Check for empty tests
  EMPTY_TESTS=$(grep -c "it('.*', () => {})" "$FILE_PATH" 2>/dev/null || echo "0")
  if [ "$EMPTY_TESTS" -gt 0 ]; then
    echo "WARNING: Found $EMPTY_TESTS empty test bodies"
  fi

  # Check for skipped tests
  SKIP_COUNT=$(grep -c "it.skip\|describe.skip" "$FILE_PATH" 2>/dev/null || echo "0")
  if [ "$SKIP_COUNT" -gt 0 ]; then
    echo "ERROR: Found $SKIP_COUNT skipped tests - not allowed"
  fi
fi

# Validate JSON files
if [[ "$FILE_PATH" == *.json ]]; then
  if command -v jq &> /dev/null; then
    if ! jq empty "$FILE_PATH" 2>/dev/null; then
      echo "ERROR: Invalid JSON in $FILE_PATH"
    fi
  fi
fi

echo "Post-edit: Validation complete"
exit 0
