# UNRDF Documentation Archive

Historical documentation organized by archive date.

## Archives

### [2026-01-11](./2026-01-11/)
**Files**: 265+ markdown files + code artifacts
**Categories**: Root reports, autonomic innovation, enterprise migration, research, proofs, historical snapshots
**Reason**: Consolidate completed work and clean up repository root

#### Contents Summary
- **Root reports** (189 files) - Agent execution reports, validation summaries, implementation plans
- **AUTONOMIC_INNOVATION** - Autonomic computing innovation experiments (10 agents)
- **ENTERPRISE_MIGRATION** - Enterprise migration tooling and contracts (10 agents)
- **research-output** - MCP research, IDE research, capability analysis
- **proofs** - Receipt tamper detection, audit trails, poka-yoke patterns
- **historical** - Rollback snapshots, capability analysis, permutation tests

## Purpose

This archive preserves completed work, execution reports, and historical documentation
while keeping the repository root clean and focused on active development.

## Archive Organization

```
archive/
├── README.md (this file)
└── YYYY-MM-DD/
    ├── README.md (detailed manifest)
    ├── root-reports/
    ├── autonomic/
    ├── enterprise/
    ├── research/
    ├── proofs/
    └── historical/
```

## Accessing Archives

```bash
# List all archives
ls -la archive/

# View archive manifest
cat archive/2026-01-11/README.md

# Search across all archives
grep -r "search term" archive/

# Find specific file type
find archive/ -name "*.md" -o -name "*.mjs"
```

## Retention Policy

Archives are retained indefinitely in git history. Physical files may be removed from
working directory after 6 months, but remain accessible via git history.

## Questions?

See individual archive README files for detailed manifests and restoration instructions.
