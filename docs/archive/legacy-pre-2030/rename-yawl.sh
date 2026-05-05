#!/bin/bash
# =============================================================================
# YAWL to HWFL Rename Script
# =============================================================================
# Purpose: Rename @unrdf/yawl package to @unrdf/hwfl to avoid prior art
#          collision with van der Aalst's YAWL (2005)
#
# Usage: ./rename-yawl.sh [--dry-run]
#
# Options:
#   --dry-run    Show what would be done without making changes
# =============================================================================

set -euo pipefail

# Configuration
DRY_RUN=false
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PACKAGES_DIR="${SCRIPT_DIR}/packages"
OLD_NAME="yawl"
NEW_NAME="hwfl"
OLD_NAME_UPPER="YAWL"
NEW_NAME_UPPER="HWFL"
OLD_NAME_CAMEL="Yawl"
NEW_NAME_CAMEL="Hwfl"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Parse arguments
if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=true
    echo -e "${YELLOW}DRY RUN MODE - No changes will be made${NC}"
fi

log() {
    echo -e "${GREEN}[RENAME]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
    exit 1
}

run_cmd() {
    if [[ "$DRY_RUN" == "true" ]]; then
        echo -e "${BLUE}[DRY-RUN]${NC} $1"
    else
        eval "$1"
    fi
}

# =============================================================================
# Pre-flight checks
# =============================================================================
log "Running pre-flight checks..."

# Check we're in the right directory
if [[ ! -d "${PACKAGES_DIR}/${OLD_NAME}" ]]; then
    error "Package directory not found: ${PACKAGES_DIR}/${OLD_NAME}"
fi

# Check git is clean (optional warning)
if [[ -n "$(git status --porcelain)" ]]; then
    warn "Git working directory is not clean. Consider committing changes first."
fi

# Create backup branch
log "Creating backup branch..."
run_cmd "git branch -f backup-before-hwfl-rename 2>/dev/null || true"

# =============================================================================
# Step 1: Rename main package directory (preserves git history)
# =============================================================================
log "Step 1: Renaming package directory..."

if [[ -d "${PACKAGES_DIR}/${OLD_NAME}" && ! -d "${PACKAGES_DIR}/${NEW_NAME}" ]]; then
    run_cmd "git mv ${PACKAGES_DIR}/${OLD_NAME} ${PACKAGES_DIR}/${NEW_NAME}"
else
    if [[ -d "${PACKAGES_DIR}/${NEW_NAME}" ]]; then
        warn "Directory ${PACKAGES_DIR}/${NEW_NAME} already exists, skipping directory rename"
    fi
fi

# =============================================================================
# Step 2: Rename source files with yawl in the name
# =============================================================================
log "Step 2: Renaming source files..."

# Source files
SOURCE_FILES=(
    "src/cancellation/yawl-cancellation.mjs:src/cancellation/hwfl-cancellation.mjs"
    "src/events/yawl-events.mjs:src/events/hwfl-events.mjs"
    "src/hooks/yawl-hooks.mjs:src/hooks/hwfl-hooks.mjs"
    "src/ontology/yawl-ontology.mjs:src/ontology/hwfl-ontology.mjs"
    "src/resources/yawl-resources.mjs:src/resources/hwfl-resources.mjs"
    "src/store/yawl-store.mjs:src/store/hwfl-store.mjs"
    "src/types/yawl-schemas.mjs:src/types/hwfl-schemas.mjs"
    "src/types/yawl-types.mjs:src/types/hwfl-types.mjs"
)

for file_pair in "${SOURCE_FILES[@]}"; do
    OLD_FILE="${PACKAGES_DIR}/${NEW_NAME}/${file_pair%%:*}"
    NEW_FILE="${PACKAGES_DIR}/${NEW_NAME}/${file_pair##*:}"
    if [[ -f "$OLD_FILE" ]]; then
        run_cmd "git mv \"$OLD_FILE\" \"$NEW_FILE\""
    else
        warn "File not found: $OLD_FILE"
    fi
done

# Test files
TEST_FILES=(
    "test/yawl.test.mjs:test/hwfl.test.mjs"
    "test/yawl-events.test.mjs:test/hwfl-events.test.mjs"
    "test/yawl-hooks.test.mjs:test/hwfl-hooks.test.mjs"
    "test/yawl-patterns.test.mjs:test/hwfl-patterns.test.mjs"
    "test/yawl-resources.test.mjs:test/hwfl-resources.test.mjs"
)

for file_pair in "${TEST_FILES[@]}"; do
    OLD_FILE="${PACKAGES_DIR}/${NEW_NAME}/${file_pair%%:*}"
    NEW_FILE="${PACKAGES_DIR}/${NEW_NAME}/${file_pair##*:}"
    if [[ -f "$OLD_FILE" ]]; then
        run_cmd "git mv \"$OLD_FILE\" \"$NEW_FILE\""
    else
        warn "File not found: $OLD_FILE"
    fi
done

# =============================================================================
# Step 3: Text replacements in package files
# =============================================================================
log "Step 3: Performing text replacements in package files..."

# Function to perform sed replacement
replace_in_file() {
    local file="$1"
    local old="$2"
    local new="$3"
    if [[ -f "$file" ]]; then
        if [[ "$DRY_RUN" == "true" ]]; then
            echo -e "${BLUE}[DRY-RUN]${NC} sed -i 's/${old}/${new}/g' $file"
        else
            sed -i "s/${old}/${new}/g" "$file"
        fi
    fi
}

# Get all files to process in the package
if [[ "$DRY_RUN" == "false" ]]; then
    PACKAGE_FILES=$(find "${PACKAGES_DIR}/${NEW_NAME}" -type f \( -name "*.mjs" -o -name "*.json" -o -name "*.md" \) 2>/dev/null)
else
    PACKAGE_FILES=$(find "${PACKAGES_DIR}/${OLD_NAME}" -type f \( -name "*.mjs" -o -name "*.json" -o -name "*.md" \) 2>/dev/null || find "${PACKAGES_DIR}/${NEW_NAME}" -type f \( -name "*.mjs" -o -name "*.json" -o -name "*.md" \) 2>/dev/null)
fi

# Replacements (order matters - longer patterns first)
REPLACEMENTS=(
    # Package references
    "@unrdf/yawl:@unrdf/hwfl"

    # URIs
    "http:\/\/yawl\.io\/:http:\/\/hwfl.io\/"
    "http:\/\/unrdf\.org\/yawl:http:\/\/unrdf.org\/hwfl"

    # Constants (uppercase)
    "YAWL_RESOURCE_NS:HWFL_RESOURCE_NS"
    "YAWL_EVENT_TYPES:HWFL_EVENT_TYPES"
    "YAWL_PREDICATES:HWFL_PREDICATES"
    "YAWL_PREFIXES:HWFL_PREFIXES"
    "YAWL_GRAPHS:HWFL_GRAPHS"
    "YAWL_CASE:HWFL_CASE"
    "YAWL_TASK:HWFL_TASK"
    "YAWL_WORK:HWFL_WORK"
    "YAWL_NS:HWFL_NS"

    # Schemas
    "YAWLHooksWorkflowSchema:HWFLHooksWorkflowSchema"
    "YAWLHooksTaskSchema:HWFLHooksTaskSchema"
    "YAWLWorkflowSchema:HWFLWorkflowSchema"
    "YAWLTaskSchema:HWFLTaskSchema"
    "YawlNetSpecSchema:HwflNetSpecSchema"

    # Classes (CamelCase)
    "YawlResourceManager:HwflResourceManager"
    "YawlResourcePool:HwflResourcePool"
    "YawlWorkflow:HwflWorkflow"
    "YawlReceipt:HwflReceipt"
    "YawlEngine:HwflEngine"
    "YawlCase:HwflCase"
    "YawlTask:HwflTask"

    # Factory functions
    "createYAWLPolicyPack:createHWFLPolicyPack"
    "createYawlStore:createHwflStore"

    # SPARQL prefixes
    "PREFIX yawl-case::PREFIX hwfl-case:"
    "PREFIX yawl-task::PREFIX hwfl-task:"
    "PREFIX yawl-work::PREFIX hwfl-work:"
    "PREFIX yawl::PREFIX hwfl:"

    # File references in imports/exports
    "yawl-cancellation:hwfl-cancellation"
    "yawl-resources:hwfl-resources"
    "yawl-ontology:hwfl-ontology"
    "yawl-schemas:hwfl-schemas"
    "yawl-events:hwfl-events"
    "yawl-hooks:hwfl-hooks"
    "yawl-store:hwfl-store"
    "yawl-types:hwfl-types"
    "yawl\.test:hwfl.test"

    # General namespace constant (must come after more specific replacements)
    "'YAWL':'HWFL'"
    "\"YAWL\":\"HWFL\""
)

for file in $PACKAGE_FILES; do
    for replacement in "${REPLACEMENTS[@]}"; do
        OLD_PATTERN="${replacement%%:*}"
        NEW_PATTERN="${replacement##*:}"
        replace_in_file "$file" "$OLD_PATTERN" "$NEW_PATTERN"
    done
done

# =============================================================================
# Step 4: Update package.json specifically
# =============================================================================
log "Step 4: Updating package.json..."

PACKAGE_JSON="${PACKAGES_DIR}/${NEW_NAME}/package.json"
if [[ -f "$PACKAGE_JSON" ]]; then
    if [[ "$DRY_RUN" == "false" ]]; then
        # Update name
        sed -i 's/"name": "@unrdf\/yawl"/"name": "@unrdf\/hwfl"/g' "$PACKAGE_JSON"

        # Update description
        sed -i 's/YAWL (Yet Another Workflow Language)/HWFL (Hook Workflow Language)/g' "$PACKAGE_JSON"

        # Update keywords - remove van-der-aalst, change yawl to hwfl
        sed -i 's/"yawl"/"hwfl"/g' "$PACKAGE_JSON"
        sed -i '/"van-der-aalst"/d' "$PACKAGE_JSON"

        # Update exports paths
        sed -i 's/yawl-ontology/hwfl-ontology/g' "$PACKAGE_JSON"
        sed -i 's/yawl-store/hwfl-store/g' "$PACKAGE_JSON"
        sed -i 's/yawl-types/hwfl-types/g' "$PACKAGE_JSON"
        sed -i 's/yawl-schemas/hwfl-schemas/g' "$PACKAGE_JSON"
        sed -i 's/yawl-hooks/hwfl-hooks/g' "$PACKAGE_JSON"
        sed -i 's/yawl-resources/hwfl-resources/g' "$PACKAGE_JSON"
    else
        echo -e "${BLUE}[DRY-RUN]${NC} Would update package.json with new name, description, keywords, and export paths"
    fi
fi

# =============================================================================
# Step 5: Update documentation files outside package
# =============================================================================
log "Step 5: Updating documentation files..."

DOC_FILES=(
    "${SCRIPT_DIR}/BENCHMARK-SUITE.md"
    "${SCRIPT_DIR}/CODE-LISTINGS.md"
    "${SCRIPT_DIR}/CORRECTED-THESIS-EXCERPTS.md"
    "${SCRIPT_DIR}/DIAGRAMS.md"
    "${SCRIPT_DIR}/FINAL-ADVERSARIAL-REVIEW.md"
    "${SCRIPT_DIR}/METRICS-CORRECTIONS.md"
    "${SCRIPT_DIR}/PERFORMANCE-ANALYSIS.md"
    "${SCRIPT_DIR}/PERFORMANCE-MODEL.md"
    "${SCRIPT_DIR}/POLISHED-EXCERPTS.md"
    "${SCRIPT_DIR}/SUPPLEMENTARY-MATERIALS.md"
    "${SCRIPT_DIR}/TABLES.md"
    "${SCRIPT_DIR}/TEST-RESULTS.md"
    "${SCRIPT_DIR}/max-combo-10-mega-framework.mjs"
    "${SCRIPT_DIR}/max-combo-10-mega-framework-standalone.mjs"
    "${SCRIPT_DIR}/docs/PHD-THESIS-UNRDF-2028-REVOLUTION-FINAL.md"
    "${SCRIPT_DIR}/docs/PHD-THESIS-UNRDF-2028-REVOLUTION-UPGRADE.md"
    "${SCRIPT_DIR}/docs/THESIS-BEYOND-HUMAN-PERCEPTION-FINAL.md"
    "${SCRIPT_DIR}/docs/THESIS-BEYOND-HUMAN-PERCEPTION-UPGRADE.md"
    "${SCRIPT_DIR}/docs/THESIS-BIGBANG-80-20-FINAL.md"
    "${SCRIPT_DIR}/docs/THESIS-BIGBANG-80-20-UPGRADE.md"
    "${SCRIPT_DIR}/docs/THESIS-COMPLETION-EXECUTIVE-SUMMARY.md"
    "${SCRIPT_DIR}/docs/THESIS-UPGRADE-SYNTHESIS-2025.md"
    "${SCRIPT_DIR}/docs/MERGE-CHANGELOG.md"
    "${SCRIPT_DIR}/docs/POSITIONING-ANALYSIS.md"
    "${SCRIPT_DIR}/docs/UNIFIED-ARCHITECTURE-CHAPTER.md"
    "${SCRIPT_DIR}/docs/ARCHITECTURE-COHERENCE-REPORT.md"
)

for file in "${DOC_FILES[@]}"; do
    if [[ -f "$file" ]]; then
        for replacement in "${REPLACEMENTS[@]}"; do
            OLD_PATTERN="${replacement%%:*}"
            NEW_PATTERN="${replacement##*:}"
            replace_in_file "$file" "$OLD_PATTERN" "$NEW_PATTERN"
        done

        # Additional documentation-specific replacements
        if [[ "$DRY_RUN" == "false" ]]; then
            # Update package references in documentation
            sed -i 's/packages\/yawl/packages\/hwfl/g' "$file"
            # Update full name references (but preserve academic citations)
            sed -i 's/YAWL (Yet Another Workflow Language) engine/HWFL (Hook Workflow Language) engine/g' "$file"
            sed -i 's/YAWL engine/HWFL engine/g' "$file"
        fi
    fi
done

# =============================================================================
# Step 6: Update pnpm workspace references
# =============================================================================
log "Step 6: Checking pnpm workspace configuration..."

# pnpm-workspace.yaml typically doesn't need changes if it uses wildcards
# But if there are explicit references, update them
WORKSPACE_FILE="${SCRIPT_DIR}/pnpm-workspace.yaml"
if [[ -f "$WORKSPACE_FILE" ]]; then
    if grep -q "yawl" "$WORKSPACE_FILE"; then
        if [[ "$DRY_RUN" == "false" ]]; then
            sed -i 's/yawl/hwfl/g' "$WORKSPACE_FILE"
        else
            echo -e "${BLUE}[DRY-RUN]${NC} Would update pnpm-workspace.yaml"
        fi
    fi
fi

# =============================================================================
# Step 7: Run pnpm install to update lockfile
# =============================================================================
log "Step 7: Updating pnpm lockfile..."

if [[ "$DRY_RUN" == "false" ]]; then
    log "Running pnpm install to regenerate lockfile..."
    pnpm install --no-frozen-lockfile || warn "pnpm install had warnings, check manually"
else
    echo -e "${BLUE}[DRY-RUN]${NC} Would run: pnpm install --no-frozen-lockfile"
fi

# =============================================================================
# Step 8: Verification
# =============================================================================
log "Step 8: Running verification checks..."

if [[ "$DRY_RUN" == "false" ]]; then
    # Check for remaining yawl references (excluding this script and plan)
    REMAINING=$(grep -r "yawl" "${PACKAGES_DIR}/${NEW_NAME}" --include="*.mjs" --include="*.json" 2>/dev/null | grep -v "van der Aalst" | grep -v "YAWL (2005)" | head -20 || true)

    if [[ -n "$REMAINING" ]]; then
        warn "Found remaining 'yawl' references that may need manual review:"
        echo "$REMAINING"
    else
        log "No unexpected 'yawl' references found in package files"
    fi

    # Check for broken imports
    log "Checking for import consistency..."
    BROKEN_IMPORTS=$(grep -r "from '@unrdf/yawl" "${SCRIPT_DIR}" --include="*.mjs" 2>/dev/null | head -10 || true)
    if [[ -n "$BROKEN_IMPORTS" ]]; then
        warn "Found old import references that need updating:"
        echo "$BROKEN_IMPORTS"
    fi
else
    echo -e "${BLUE}[DRY-RUN]${NC} Would verify: No remaining yawl references, imports consistent"
fi

# =============================================================================
# Summary
# =============================================================================
echo ""
echo "============================================================================="
log "RENAME COMPLETE"
echo "============================================================================="
echo ""
echo "Summary of changes:"
echo "  - Package directory: packages/yawl -> packages/hwfl"
echo "  - Package name: @unrdf/yawl -> @unrdf/hwfl"
echo "  - Source files renamed: 8 files"
echo "  - Test files renamed: 5 files"
echo "  - Namespace URIs updated: 5"
echo "  - Class names updated: 10+"
echo "  - Documentation files updated: 20+"
echo ""
echo "Next steps:"
echo "  1. Review changes: git diff --stat"
echo "  2. Run tests: pnpm --filter @unrdf/hwfl test"
echo "  3. Check for manual review items in YAWL-RENAME-PLAN.md"
echo "  4. Commit changes: git add -A && git commit -m 'refactor: rename YAWL to HWFL to avoid prior art collision'"
echo ""

if [[ "$DRY_RUN" == "true" ]]; then
    echo -e "${YELLOW}This was a DRY RUN. No changes were made.${NC}"
    echo "Run without --dry-run to apply changes."
fi
