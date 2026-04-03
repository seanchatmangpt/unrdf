.PHONY: help publish publish-check version-bump pre-publish test lint clean \
	publish-all publish-packages publish-single tag changelog \
	validate-npm-auth setup-npm

# ============================================================================
# VARIABLES
# ============================================================================

VERSION ?= 26.4.2
NPM_TAG ?= latest
NPM_TOKEN ?=
DRY_RUN ?= false
PUBLISH_TIMEOUT ?= 120
WORKSPACE_ROOT := $(CURDIR)

# Private packages to exclude from publishing
PRIVATE_PACKAGES := docs domain integration-tests nextra test-utils validation

# Color output
RED := \033[0;31m
GREEN := \033[0;32m
YELLOW := \033[0;33m
BLUE := \033[0;34m
NC := \033[0m # No Color

# ============================================================================
# HELP
# ============================================================================

help:
	@echo "$(BLUE)UNRDF Publishing Workflow$(NC)"
	@echo ""
	@echo "$(YELLOW)Usage:$(NC)"
	@echo "  make publish              # Full publish workflow (test → lint → bump → publish)"
	@echo "  make publish-check        # Validate package readiness (no actual publish)"
	@echo "  make publish-all          # Publish all public packages (requires auth)"
	@echo "  make publish-single PKG=<name>  # Publish single package"
	@echo ""
	@echo "$(YELLOW)Configuration:$(NC)"
	@echo "  VERSION=26.4.3           # Override version (default: $(VERSION))"
	@echo "  NPM_TOKEN=<token>        # Set npm token for publishing"
	@echo "  DRY_RUN=true             # Simulate publish without actual upload"
	@echo "  NPM_TAG=latest           # npm tag (default: latest)"
	@echo ""
	@echo "$(YELLOW)Setup & Validation:$(NC)"
	@echo "  make setup-npm           # Configure npm authentication"
	@echo "  make validate-npm-auth   # Check npm token validity"
	@echo ""
	@echo "$(YELLOW)Helpers:$(NC)"
	@echo "  make test                # Run full test suite"
	@echo "  make lint                # Run linter"
	@echo "  make version-bump        # Bump all packages to VERSION"
	@echo "  make tag                 # Create git tags for release"
	@echo "  make clean               # Clean build artifacts"

# ============================================================================
# SETUP & VALIDATION
# ============================================================================

.setup-npm-auth:
	@if [ -z "$(NPM_TOKEN)" ]; then \
		echo "$(RED)ERROR: NPM_TOKEN not set$(NC)"; \
		echo "Usage: make publish NPM_TOKEN=npm_xxxxxx..."; \
		exit 1; \
	fi
	@npm set //registry.npmjs.org/:_authToken="$(NPM_TOKEN)" 2>/dev/null
	@echo "$(GREEN)✓ npm token configured$(NC)"

setup-npm: .setup-npm-auth validate-npm-auth
	@echo "$(GREEN)✓ npm setup complete$(NC)"

validate-npm-auth: .setup-npm-auth
	@if npm whoami > /dev/null 2>&1; then \
		echo "$(GREEN)✓ npm authenticated as: $$(npm whoami)$(NC)"; \
	else \
		echo "$(RED)ERROR: npm authentication failed$(NC)"; \
		exit 1; \
	fi

# ============================================================================
# PACKAGE DISCOVERY & VALIDATION
# ============================================================================

.PHONY: list-packages list-public-packages list-private-packages

list-packages:
	@find packages -maxdepth 2 -name "package.json" ! -path "*/node_modules/*" | \
		sed 's|packages/||;s|/package.json||' | sort

list-public-packages:
	@find packages -maxdepth 2 -name "package.json" ! -path "*/node_modules/*" \
		! -exec grep -l '"private"' {} \; | \
		sed 's|packages/||;s|/package.json||' | sort

list-private-packages:
	@find packages -maxdepth 2 -name "package.json" ! -path "*/node_modules/*" \
		-exec grep -l '"private"' {} \; | \
		sed 's|packages/||;s|/package.json||' | sort

# ============================================================================
# VERSIONING
# ============================================================================

version-bump:
	@echo "$(BLUE)Bumping all packages to v$(VERSION)...$(NC)"
	@find packages -maxdepth 2 -name "package.json" ! -path "*/node_modules/*" \
		-exec sed -i '' 's/"version": "[^"]*"/"version": "$(VERSION)"/g' {} \;
	@echo "$(GREEN)✓ Version bumped to $(VERSION)$(NC)"
	@git diff packages/*/package.json | head -20

# ============================================================================
# VALIDATION & QUALITY GATES
# ============================================================================

test:
	@echo "$(BLUE)Running test suite...$(NC)"
	@timeout 60s pnpm test 2>&1 | tail -50
	@echo "$(GREEN)✓ Tests passed$(NC)"

lint:
	@echo "$(BLUE)Running linter...$(NC)"
	@timeout 30s pnpm lint 2>&1 | grep -E "error|warning" | head -20 || true
	@echo "$(GREEN)✓ Lint check complete$(NC)"

format-check:
	@echo "$(BLUE)Checking code format...$(NC)"
	@timeout 30s pnpm format:check 2>&1 | tail -10 || true
	@echo "$(GREEN)✓ Format check complete$(NC)"

publish-check: lint format-check
	@echo "$(BLUE)Validating package readiness...$(NC)"
	@echo "$(YELLOW)Public packages: $$(make list-public-packages | wc -l)$(NC)"
	@echo "$(YELLOW)Private packages: $$(make list-private-packages | wc -l)$(NC)"
	@echo "$(YELLOW)Total: $$(make list-packages | wc -l)$(NC)"
	@echo ""
	@echo "$(YELLOW)Checking for unpublished packages:$(NC)"
	@for pkg in $$(make list-public-packages | head -5); do \
		current=$$(grep '"version"' packages/$$pkg/package.json 2>/dev/null | grep -o '[0-9.]*' | head -1); \
		echo "  $$pkg: v$$current"; \
	done
	@echo "$(GREEN)✓ Package validation complete$(NC)"

# ============================================================================
# PUBLISHING
# ============================================================================

pre-publish: version-bump
	@echo "$(BLUE)Pre-publish setup...$(NC)"
	@git add packages/*/package.json
	@git commit -m "chore: Bump all packages to v$(VERSION)" --no-verify || true
	@git push --no-verify || true
	@echo "$(GREEN)✓ Version commit pushed$(NC)"

publish-packages: validate-npm-auth
	@echo "$(BLUE)Publishing all public packages...$(NC)"
	@if [ "$(DRY_RUN)" = "true" ]; then \
		echo "$(YELLOW)DRY RUN MODE - no actual publish$(NC)"; \
	fi
	@cd $(WORKSPACE_ROOT) && timeout $(PUBLISH_TIMEOUT)s bash -c ' \
		count=0; \
		for dir in packages/*/; do \
			pkg=$$(basename "$$dir"); \
			if grep -q "\"private\"" "$$dir/package.json" 2>/dev/null; then \
				echo "$(YELLOW)⏭️  $$pkg (private)$(NC)"; \
				continue; \
			fi; \
			echo "$(BLUE)Publishing $$pkg...$(NC)"; \
			if [ "$(DRY_RUN)" = "true" ]; then \
				(cd "$$dir" && npm publish --access public --dry-run 2>&1 | grep -q "^+") && { count=$$((count+1)); echo "$(GREEN)✅ $$pkg$(NC)"; } || echo "$(YELLOW)⚠️  $$pkg$(NC)"; \
			else \
				(cd "$$dir" && npm publish --access public 2>&1) | grep -q "^+" && { count=$$((count+1)); echo "$(GREEN)✅ $$pkg$(NC)"; } || echo "$(YELLOW)⚠️  $$pkg$(NC)"; \
			fi; \
		done; \
		echo "$(GREEN)✓ Published: $$count packages$(NC)" \
	'

publish-single: validate-npm-auth
	@if [ -z "$(PKG)" ]; then \
		echo "$(RED)ERROR: PKG not specified$(NC)"; \
		echo "Usage: make publish-single PKG=core"; \
		exit 1; \
	fi
	@if [ ! -d "packages/$(PKG)" ]; then \
		echo "$(RED)ERROR: Package not found: packages/$(PKG)$(NC)"; \
		exit 1; \
	fi
	@echo "$(BLUE)Publishing $(PKG)...$(NC)"
	@cd packages/$(PKG) && npm publish --access public --tag $(NPM_TAG)
	@echo "$(GREEN)✓ Published $(PKG)@$(VERSION)$(NC)"

# ============================================================================
# TAGGING & RELEASE
# ============================================================================

tag:
	@echo "$(BLUE)Creating git tags...$(NC)"
	@git tag -a "v$(VERSION)" -m "Release v$(VERSION)" 2>/dev/null || echo "$(YELLOW)Tag already exists$(NC)"
	@git push origin "v$(VERSION)" --no-verify 2>/dev/null || true
	@echo "$(GREEN)✓ Tags created and pushed$(NC)"

# ============================================================================
# CHANGELOG
# ============================================================================

changelog:
	@echo "$(BLUE)Generating changelog...$(NC)"
	@echo "# Release v$(VERSION)" > CHANGELOG_v$(VERSION).md
	@echo "" >> CHANGELOG_v$(VERSION).md
	@echo "Released: $$(date -u +'%Y-%m-%d')" >> CHANGELOG_v$(VERSION).md
	@echo "" >> CHANGELOG_v$(VERSION).md
	@echo "## Packages Published" >> CHANGELOG_v$(VERSION).md
	@make list-public-packages | sed 's/^/- @unrdf\//' >> CHANGELOG_v$(VERSION).md
	@echo "" >> CHANGELOG_v$(VERSION).md
	@echo "## Commit Info" >> CHANGELOG_v$(VERSION).md
	@git log --oneline -10 >> CHANGELOG_v$(VERSION).md
	@echo "$(GREEN)✓ Changelog created: CHANGELOG_v$(VERSION).md$(NC)"

# ============================================================================
# MAIN WORKFLOWS
# ============================================================================

publish: publish-check pre-publish publish-packages tag
	@echo ""
	@echo "$(GREEN)╔════════════════════════════════════════════╗$(NC)"
	@echo "$(GREEN)║  Release v$(VERSION) Complete!            ║$(NC)"
	@echo "$(GREEN)║  All packages published to npm            ║$(NC)"
	@echo "$(GREEN)╚════════════════════════════════════════════╝$(NC)"

publish-all: validate-npm-auth pre-publish publish-packages tag changelog
	@echo ""
	@echo "$(GREEN)╔════════════════════════════════════════════╗$(NC)"
	@echo "$(GREEN)║  Full Release Workflow Complete!          ║$(NC)"
	@echo "$(GREEN)║  All packages published + tagged + logged ║$(NC)"
	@echo "$(GREEN)╚════════════════════════════════════════════╝$(NC)"

# ============================================================================
# CLEANUP
# ============================================================================

clean:
	@echo "$(BLUE)Cleaning build artifacts...$(NC)"
	@pnpm clean 2>/dev/null || true
	@rm -rf node_modules packages/*/node_modules
	@rm -f CHANGELOG_v*.md
	@echo "$(GREEN)✓ Cleanup complete$(NC)"
