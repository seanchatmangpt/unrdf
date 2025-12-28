#!/bin/bash
# Pre-edit hook for UNRDF development
# Validates file before editing

set -e

FILE_PATH="$1"

# Skip if no file path provided
if [ -z "$FILE_PATH" ]; then
  exit 0
fi

# Check file extension
if [[ "$FILE_PATH" == *.mjs ]]; then
  echo "Pre-edit: Validating $FILE_PATH"

  # Check if file exists and get line count
  if [ -f "$FILE_PATH" ]; then
    LINE_COUNT=$(wc -l < "$FILE_PATH")

    # Warn if approaching 500 line limit
    if [ "$LINE_COUNT" -gt 400 ]; then
      echo "WARNING: File has $LINE_COUNT lines (limit: 500)"
      echo "Consider splitting into modules before adding more code"
    fi
  fi

  # Check for common issues in the edit context
  if [ -f "$FILE_PATH" ]; then
    # Check for direct n3 imports
    if grep -q "from 'n3'" "$FILE_PATH" 2>/dev/null; then
      echo "WARNING: Found direct n3 import. Use @unrdf/oxigraph instead"
    fi

    # Check for TODOs
    TODO_COUNT=$(grep -c "TODO" "$FILE_PATH" 2>/dev/null || echo "0")
    if [ "$TODO_COUNT" -gt 0 ]; then
      echo "NOTE: File contains $TODO_COUNT TODOs to address"
    fi
  fi
fi

# Check for test files
if [[ "$FILE_PATH" == *.test.mjs ]]; then
  echo "Pre-edit: Test file detected"

  if [ -f "$FILE_PATH" ]; then
    # Check for skipped tests
    SKIP_COUNT=$(grep -c "it.skip\|describe.skip" "$FILE_PATH" 2>/dev/null || echo "0")
    if [ "$SKIP_COUNT" -gt 0 ]; then
      echo "WARNING: File contains $SKIP_COUNT skipped tests"
    fi
  fi
fi

exit 0
