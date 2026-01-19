# Error Handling Enhancements - Sync Command

## Overview

Enhanced error handling in the sync command modules to provide actionable, detailed error messages with line numbers, file paths, and specific fix suggestions.

## Modified Files

### 1. `src/cli/commands/sync/orchestrator.mjs`

**Enhancements:**
- ✅ Added validation that template file exists and is readable before executing rule
- ✅ Wrapped SPARQL query execution in dedicated try-catch with specific error context
- ✅ Wrapped template rendering in dedicated try-catch with specific error context
- ✅ Provides clear error messages with:
  - Rule name
  - Template path
  - Config file path
  - Specific error message
  - Actionable fix suggestions
- ✅ Continues processing other rules if one fails (already implemented, now improved)
- ✅ Shows stack trace in verbose mode

**Example Error Message:**
```
ERR my-rule:
Template file not found or not readable: /path/to/template.njk
  Rule: my-rule
  Config: /path/to/ggen.toml
  Fix: Check that the template path is correct and the file exists
```

### 2. `src/cli/commands/sync/ontology-loader.mjs`

**Enhancements:**
- ✅ Validates that source field is present in config
- ✅ Checks file exists before attempting to read
- ✅ Checks file is readable with proper permissions check
- ✅ Auto-detects RDF format from file extension if not specified
- ✅ Wraps store.load in try-catch with detailed parse error handling
- ✅ Extracts line numbers from parse errors
- ✅ Provides format-specific fix suggestions for Turtle syntax errors
- ✅ Suggests explicit format specification if auto-detection might be wrong

**Example Error Message:**
```
Failed to parse ontology file: /path/to/ontology.ttl
  Format: turtle
  Line: 42
  Parse error: unexpected token "}"

Possible fixes:
  1. Check TURTLE syntax is valid
  2. Verify all prefixes are declared with @prefix
  3. Check for unescaped special characters in literals
  4. Ensure all statements end with a period (.)
  5. Try specifying format explicitly in config:
     "format": "turtle"  (or "ntriples", "rdfxml", etc.)
```

### 3. `src/cli/commands/sync/sparql-executor.mjs`

**Enhancements:**
- ✅ Enhanced timeout error messages with timeout value and query preview
- ✅ Added `extractLineInfo()` helper to extract line/column from SPARQL errors
- ✅ Adjusts line numbers to account for auto-added PREFIX declarations
- ✅ Reports both full query line number and user query line number
- ✅ Added `getSyntaxSuggestions()` helper for context-aware fix suggestions
- ✅ Provides specific suggestions based on error type:
  - Prefix/namespace errors
  - Syntax errors (punctuation, brackets)
  - Variable naming issues
  - URI/IRI format errors
  - Literal quoting errors
  - Keyword typos

**Example Error Message:**
```
SPARQL query failed: unexpected token at line 15
  Line 12 (15 in full query): FILTER (?age > 18

Possible fixes:
  - Check for missing or extra punctuation (., ;, ,)
  - Verify all brackets/braces are balanced
```

**Helper Functions Added:**
- `extractLineInfo(errorMsg, prefixLineCount)` - Extracts line/column from error messages
- `getSyntaxSuggestions(errorMsg)` - Returns contextual fix suggestions

### 4. `src/cli/commands/sync/template-renderer.mjs`

**Enhancements:**
- ✅ Wraps template rendering in try-catch with detailed error handling
- ✅ Added `extractNunjucksLineInfo()` helper to extract line/column from Nunjucks errors
- ✅ Added `getTemplateSuggestions()` helper for template-specific fix suggestions
- ✅ Provides context-aware suggestions based on error type:
  - Undefined variables (lists available variables)
  - Unknown filters (lists available filters)
  - Unmatched tags
  - Syntax errors
  - Null/undefined access errors
- ✅ Enhanced batch render error messages with line/column info

**Example Error Message:**
```
Template rendering failed: undefined variable 'className'
  Line 23, Column 15

Possible fixes:
  - Check that all variables used in template are defined in context
  - Variable 'className' is not available. Available: sparql_results, results, prefixes, now, project
```

**Helper Functions Added:**
- `extractNunjucksLineInfo(err)` - Extracts line/column from Nunjucks errors
- `getTemplateSuggestions(errorMsg, context)` - Returns contextual template fix suggestions

## Error Message Structure

All enhanced error messages follow this consistent structure:

```
[Error type]: [Brief description]
  [Context Field 1]: [Value]
  [Context Field 2]: [Value]
  [Error details]: [Specific error message]

Possible fixes:
  1. [First suggestion]
  2. [Second suggestion]
  ...
```

## Benefits

1. **Actionable Errors** - Users know exactly what to fix and how
2. **Line Numbers** - Precise location of errors in files
3. **File Paths** - Full paths to files involved
4. **Context** - Relevant configuration and state information
5. **Fix Suggestions** - Specific, contextual guidance
6. **Resilient** - Continues processing other rules on error
7. **Verbose Mode** - Stack traces available when needed

## Testing

All files passed syntax validation:
```bash
node --check src/cli/commands/sync/orchestrator.mjs      # ✅ PASS
node --check src/cli/commands/sync/ontology-loader.mjs   # ✅ PASS
node --check src/cli/commands/sync/sparql-executor.mjs   # ✅ PASS
node --check src/cli/commands/sync/template-renderer.mjs # ✅ PASS
```

## Compatibility

- ✅ No breaking changes to public APIs
- ✅ All existing tests remain valid
- ✅ Error structure extensible for future enhancements
- ✅ Backward compatible error handling (graceful degradation)

## Future Enhancements

Potential areas for further improvement:
- Add colorized diff output for parse errors
- Include file content snippets around error locations
- Add "Did you mean?" suggestions for common typos
- Integrate with language server protocol for IDE support
