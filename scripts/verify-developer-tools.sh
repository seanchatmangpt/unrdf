#!/usr/bin/env bash
# UNRDF Developer Tools Verification Script
# Tests all developer experience improvements

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
PASSED=0
FAILED=0

# Test function
test_item() {
  local description="$1"
  local command="$2"

  echo -n "Testing: $description ... "

  if eval "$command" &>/dev/null; then
    echo -e "${GREEN}✓ PASS${NC}"
    ((PASSED++))
  else
    echo -e "${RED}✗ FAIL${NC}"
    ((FAILED++))
  fi
}

# Header
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}UNRDF Developer Tools Verification${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# 1. Shell Completions
echo -e "${YELLOW}1. Shell Completions${NC}"
test_item "Bash completion file exists" "test -f completions/bash-completion.sh"
test_item "Zsh completion file exists" "test -f completions/zsh-completion.zsh"
test_item "Fish completion file exists" "test -f completions/fish-completion.fish"
test_item "Installer script exists and executable" "test -x completions/install.sh"
test_item "Completions README exists" "test -f completions/README.md"
test_item "Installation guide exists" "test -f completions/INSTALLATION.md"
echo ""

# 2. VS Code Extension
echo -e "${YELLOW}2. VS Code Extension${NC}"
test_item "Extension manifest exists" "test -f vscode-extension/package.json"
test_item "Extension code exists" "test -f vscode-extension/extension.js"
test_item "Language config exists" "test -f vscode-extension/language-configuration.json"
test_item "Hook syntax file exists" "test -f vscode-extension/syntaxes/hook.tmLanguage.json"
test_item "Policy syntax file exists" "test -f vscode-extension/syntaxes/policy.tmLanguage.json"
test_item "Hook snippets exist" "test -f vscode-extension/snippets/hooks.json"
test_item "Policy snippets exist" "test -f vscode-extension/snippets/policies.json"
test_item "Extension README exists" "test -f vscode-extension/README.md"
echo ""

# 3. REPL/Interactive Mode
echo -e "${YELLOW}3. REPL/Interactive Mode${NC}"
test_item "REPL command file exists" "test -f src/cli-v2/commands/repl.mjs"
test_item "REPL has REPLSession class" "grep -q 'class REPLSession' src/cli-v2/commands/repl.mjs"
test_item "REPL has tab completion" "grep -q 'class TabCompletion' src/cli-v2/commands/repl.mjs"
test_item "REPL has syntax highlighting" "grep -q 'class SyntaxHighlighter' src/cli-v2/commands/repl.mjs"
test_item "REPL has command history" "grep -q 'class REPLHistory' src/cli-v2/commands/repl.mjs"
test_item "REPL registered in CLI" "grep -q 'repl:' src/cli-v2/index.mjs"
echo ""

# 4. Core CLI Integration
echo -e "${YELLOW}4. Core CLI Integration${NC}"
test_item "Completion module exists" "test -f src/cli-v2/core/completion.mjs"
test_item "Completion reads from files" "grep -q 'readFileSync' src/cli-v2/core/completion.mjs"
test_item "Completion supports bash" "grep -q 'bash' src/cli-v2/core/completion.mjs"
test_item "Completion supports zsh" "grep -q 'zsh' src/cli-v2/core/completion.mjs"
test_item "Completion supports fish" "grep -q 'fish' src/cli-v2/core/completion.mjs"
echo ""

# 5. Documentation
echo -e "${YELLOW}5. Documentation${NC}"
test_item "Developer guide exists" "test -f docs/developer-guide.md"
test_item "Developer guide has Getting Started" "grep -q '## Getting Started' docs/developer-guide.md"
test_item "Developer guide has Testing Guide" "grep -q '## Testing Guide' docs/developer-guide.md"
test_item "Developer guide has CLI Development" "grep -q '## CLI Development' docs/developer-guide.md"
test_item "Developer guide has Hooks Development" "grep -q '## Knowledge Hooks Development' docs/developer-guide.md"
test_item "Developer guide has Policy Development" "grep -q '## Policy Pack Development' docs/developer-guide.md"
test_item "Developer tools summary exists" "test -f docs/developer-tools-summary.md"
echo ""

# 6. Completions Content Verification
echo -e "${YELLOW}6. Completions Content${NC}"
test_item "Bash completion includes graph" "grep -q 'graph' completions/bash-completion.sh"
test_item "Bash completion includes hook" "grep -q 'hook' completions/bash-completion.sh"
test_item "Bash completion includes policy" "grep -q 'policy' completions/bash-completion.sh"
test_item "Bash completion includes repl" "grep -q 'repl' completions/bash-completion.sh"
test_item "Zsh completion includes graph" "grep -q 'graph' completions/zsh-completion.zsh"
test_item "Zsh completion includes repl" "grep -q 'repl' completions/zsh-completion.zsh"
test_item "Fish completion includes graph" "grep -q 'graph' completions/fish-completion.fish"
test_item "Fish completion includes repl" "grep -q 'repl' completions/fish-completion.fish"
echo ""

# 7. VS Code Extension Content
echo -e "${YELLOW}7. VS Code Extension Content${NC}"
test_item "Extension has validate command" "grep -q 'validateHook' vscode-extension/package.json"
test_item "Extension has evaluate command" "grep -q 'evaluateHook' vscode-extension/package.json"
test_item "Extension has policy command" "grep -q 'applyPolicy' vscode-extension/package.json"
test_item "Hook snippets have defhook" "grep -q 'defhook' vscode-extension/snippets/hooks.json"
test_item "Hook snippets have sparqlhook" "grep -q 'sparqlhook' vscode-extension/snippets/hooks.json"
test_item "Policy snippets have defpolicy" "grep -q 'defpolicy' vscode-extension/snippets/policies.json"
test_item "Policy snippets have shaclpolicy" "grep -q 'shaclpolicy' vscode-extension/snippets/policies.json"
echo ""

# Summary
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Verification Summary${NC}"
echo -e "${BLUE}========================================${NC}"
echo -e "${GREEN}Passed: $PASSED${NC}"
if [ $FAILED -gt 0 ]; then
  echo -e "${RED}Failed: $FAILED${NC}"
else
  echo -e "${GREEN}Failed: $FAILED${NC}"
fi
echo ""

if [ $FAILED -eq 0 ]; then
  echo -e "${GREEN}✓ All developer tools verified successfully!${NC}"
  echo ""
  echo "Next steps:"
  echo "  1. Install shell completions: cd completions && ./install.sh"
  echo "  2. Try REPL: unrdf repl"
  echo "  3. Install VS Code extension: cd vscode-extension && npm run package"
  echo "  4. Read developer guide: cat docs/developer-guide.md"
  exit 0
else
  echo -e "${RED}✗ Some verifications failed. Please review.${NC}"
  exit 1
fi
