---
description: Generate and maintain changelogs following Keep a Changelog format with RDF provenance tracking
---

# Changelog Writer

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Generate and maintain changelogs for UNRDF packages following the Keep a Changelog format, with optional RDF provenance for semantic versioning.

## Changelog Format

Following [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) with Semantic Versioning:

```markdown
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- New feature description

### Changed

- Change description

### Deprecated

- Deprecation notice

### Removed

- Removal description

### Fixed

- Bug fix description

### Security

- Security fix description

## [1.0.0] - 2025-01-15

### Added

- Initial release
```

## Change Categories

| Category   | When to Use                    | Semver Impact |
| ---------- | ------------------------------ | ------------- |
| Added      | New features                   | MINOR         |
| Changed    | Existing functionality changes | MINOR/PATCH   |
| Deprecated | Soon-to-be removed features    | MINOR         |
| Removed    | Removed features               | MAJOR         |
| Fixed      | Bug fixes                      | PATCH         |
| Security   | Security vulnerabilities       | PATCH         |

## Execution Steps

### 1. Analyze Git History

```bash
# Get commits since last tag
LAST_TAG=$(git describe --tags --abbrev=0 2>/dev/null || echo "")
if [ -n "$LAST_TAG" ]; then
  git log "$LAST_TAG"..HEAD --oneline
else
  git log --oneline -20
fi
```

### 2. Categorize Changes

```bash
# Group by conventional commit type
git log "$LAST_TAG"..HEAD --pretty=format:"%s" | while read msg; do
  case "$msg" in
    feat:*) echo "Added: ${msg#feat: }" ;;
    fix:*) echo "Fixed: ${msg#fix: }" ;;
    docs:*) echo "Changed: ${msg#docs: }" ;;
    refactor:*) echo "Changed: ${msg#refactor: }" ;;
    perf:*) echo "Changed: ${msg#perf: }" ;;
    test:*) echo "Changed: ${msg#test: }" ;;
    chore:*) echo "Changed: ${msg#chore: }" ;;
    BREAKING*) echo "Removed/Changed: $msg" ;;
    *) echo "Other: $msg" ;;
  esac
done | sort
```

### 3. Generate Changelog Entry

```javascript
// generate-changelog.mjs
import { execSync } from 'child_process';

function generateChangelog(sinceTag) {
  const commits = execSync(`git log ${sinceTag}..HEAD --pretty=format:"%H|%s|%b" --no-merges`, {
    encoding: 'utf8',
  })
    .split('\n')
    .filter(Boolean);

  const changes = {
    Added: [],
    Changed: [],
    Deprecated: [],
    Removed: [],
    Fixed: [],
    Security: [],
  };

  for (const commit of commits) {
    const [hash, subject, body] = commit.split('|');

    if (subject.startsWith('feat:')) {
      changes.Added.push(subject.slice(5).trim());
    } else if (subject.startsWith('fix:')) {
      changes.Fixed.push(subject.slice(4).trim());
    } else if (subject.includes('BREAKING')) {
      changes.Changed.push(`**BREAKING:** ${subject}`);
    } else if (subject.startsWith('security:')) {
      changes.Security.push(subject.slice(9).trim());
    } else if (subject.startsWith('deprecate:')) {
      changes.Deprecated.push(subject.slice(10).trim());
    }
  }

  return formatChangelog(changes);
}

function formatChangelog(changes) {
  let md = `## [Unreleased]\n\n`;

  for (const [category, items] of Object.entries(changes)) {
    if (items.length > 0) {
      md += `### ${category}\n\n`;
      for (const item of items) {
        md += `- ${item}\n`;
      }
      md += '\n';
    }
  }

  return md;
}
```

### 4. Update CHANGELOG.md

```bash
# Backup existing
cp CHANGELOG.md CHANGELOG.md.bak

# Insert new entries after [Unreleased] header
# (Manual or scripted insertion)
```

## RDF Provenance (Optional)

Track changes as RDF for semantic queries:

```turtle
@prefix changelog: <https://unrdf.org/ontology/changelog#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

changelog:release-1.0.0 a changelog:Release ;
    changelog:version "1.0.0" ;
    changelog:date "2025-01-15"^^xsd:date ;
    changelog:hasChange [
        a changelog:Addition ;
        changelog:description "Added maturity assessment CLI" ;
        changelog:commit "abc123"
    ] .
```

## Package-Specific Changelogs

For monorepo, maintain per-package changelogs:

```bash
# Generate for specific package
PACKAGE="${ARGUMENTS:-core}"
cd "packages/$PACKAGE"

# Get package-specific changes
git log --oneline -- . | head -20
```

## Output Format

````markdown
## Changelog Generation Report

**Date**: [timestamp]
**Scope**: [package or monorepo]
**Since**: [last tag]

### Changes Detected

| Category | Count | Items         |
| -------- | ----- | ------------- |
| Added    | X     | feat: ...     |
| Fixed    | Y     | fix: ...      |
| Changed  | Z     | refactor: ... |

### Suggested Version Bump

- Current: X.Y.Z
- Suggested: [MAJOR.MINOR.PATCH]
- Reason: [breaking change / new feature / bug fix]

### Generated Entry

```markdown
## [X.Y.Z] - YYYY-MM-DD

### Added

- [items]

### Fixed

- [items]
```
````

### Next Steps

1. Review generated changelog
2. Update version in package.json
3. Commit: `git commit -m "chore: release vX.Y.Z"`
4. Tag: `git tag vX.Y.Z`

````

## Best Practices

1. **Write for humans** - Clear, concise descriptions
2. **Group logically** - Related changes together
3. **Link issues** - Reference GitHub issues (#123)
4. **Be consistent** - Same format across packages
5. **Date format** - ISO 8601 (YYYY-MM-DD)
6. **Yanked releases** - Mark with [YANKED]

## Integration with Release

```bash
# Standard release workflow
pnpm changeset          # Generate changeset
pnpm changeset version  # Update versions
pnpm changeset publish  # Publish packages

# Or manual
pnpm version patch      # Bump version
git tag v$(jq -r .version package.json)
git push --tags
````

End Command ---
