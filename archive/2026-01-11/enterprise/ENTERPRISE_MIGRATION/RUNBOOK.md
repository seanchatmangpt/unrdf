# ENTERPRISE_MIGRATION RUNBOOK

Operational guide for executing safe, verifiable enterprise migration from legacy systems to UNRDF substrate platform.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Setup](#setup)
3. [Migration Execution](#migration-execution)
4. [Verification](#verification)
5. [Rollback Procedures](#rollback-procedures)
6. [Troubleshooting](#troubleshooting)
7. [Agent Responsibilities](#agent-responsibilities)

---

## Prerequisites

### System Requirements

- Node.js >= 18.0.0
- pnpm >= 7.0.0
- Access to legacy system database (read-only recommended for initial phases)
- Access to UNRDF substrate instance
- Minimum 4GB RAM for migration process
- Disk space: 2x size of legacy database

### Access Requirements

- Legacy system API credentials
- Substrate admin credentials
- Backup storage access
- Monitoring/observability platform access

### Pre-Migration Checklist

- [ ] Legacy system backed up
- [ ] Substrate instance deployed and healthy
- [ ] Network connectivity verified between systems
- [ ] Read-only replica of legacy DB available (recommended)
- [ ] Rollback plan reviewed and approved
- [ ] Stakeholders notified of migration window

---

## Setup

### 1. Environment Configuration

```bash
# Navigate to migration directory
cd /home/user/unrdf/ENTERPRISE_MIGRATION

# Verify all agent modules present
ls -1 agent-*/

# Expected output:
# agent-1/   (Control Plane)
# agent-2/   (Discovery)
# agent-3/   (Schema Mapping)
# agent-4/   (Validation)
# agent-5/   (Migration Execution)
# agent-6/   (Impact Analysis)
# agent-7/   (Routing Modes)
# agent-8/   (Shadow Testing)
# agent-9/   (Performance Benchmarking)
# agent-10/  (Rollback Preparation)
```

### 2. Configuration Files

Create `.env` file (DO NOT commit):

```bash
# Legacy System
LEGACY_DB_HOST=legacy.example.com
LEGACY_DB_PORT=5432
LEGACY_DB_NAME=production
LEGACY_DB_USER=readonly_user
LEGACY_DB_PASSWORD=<secret>

# UNRDF Substrate
SUBSTRATE_URL=https://substrate.example.com
SUBSTRATE_API_KEY=<secret>

# Migration Settings
MIGRATION_BATCH_SIZE=1000
MIGRATION_TIMEOUT=300000
MIGRATION_DRY_RUN=true
```

### 3. Dependency Verification

```bash
# Run health check
node -e "import('./src/index.mjs').then(m => m.getHealth().then(console.log))"

# Expected output:
# {
#   healthy: true,
#   components: { ... },
#   timestamp: ...
# }
```

---

## Migration Execution

### Phase-by-Phase Execution

#### Full Migration (All Phases)

```bash
# Dry run first (ALWAYS)
node -e "import('./src/index.mjs').then(m => m.runMigration({ dryRun: true }).then(console.log))"

# Review dry run results
# If successful, proceed with actual migration

# Execute full migration
node -e "import('./src/index.mjs').then(m => m.runMigration().then(r => { console.log(JSON.stringify(r, null, 2)); process.exit(r.success ? 0 : 1); }))"
```

#### Partial Migration (Specific Phases)

```bash
# Run only discovery and impact analysis
node -e "import('./src/index.mjs').then(m => m.runMigration({ phases: ['discovery', 'impact-analysis'] }).then(console.log))"
```

### Migration Phases

1. **Discovery** (Agent 2)
   - Scans legacy system entities and relationships
   - Output: Entity count, relationship map
   - Duration: ~5-10 minutes

2. **Impact Analysis** (Agent 6)
   - Computes read/write/delete impacts for all operations
   - Detects overlapping impacts
   - Duration: ~2-5 minutes

3. **Routing Setup** (Agent 7)
   - Configures shadow routing modes
   - Initial mode: LEGACY_ONLY (safe default)
   - Duration: <1 minute

4. **Schema Mapping** (Agent 3)
   - Maps legacy schemas to RDF ontology
   - Generates property mappings
   - Duration: ~3-7 minutes

5. **Validation** (Agent 4)
   - Validates data integrity and constraints
   - CRITICAL: Migration stops if validation fails
   - Duration: ~5-15 minutes

6. **Migration** (Agent 5)
   - Executes actual data migration
   - Batch processing with progress tracking
   - Duration: ~30-120 minutes (depends on data volume)

7. **Shadow Testing** (Agent 8)
   - Runs shadow traffic tests (dual writes/reads)
   - Compares legacy vs substrate results
   - Duration: ~10-30 minutes

8. **Benchmarking** (Agent 9)
   - Performance comparison (latency, throughput)
   - Acceptable threshold: -10% or better
   - Duration: ~5-10 minutes

9. **Rollback Preparation** (Agent 10)
   - Creates backups
   - Generates rollback scripts
   - Tests rollback procedure
   - Duration: ~10-20 minutes

10. **Cutover** (Agent 1)
    - Updates routing to SUBSTRATE_ONLY
    - Final health check
    - Duration: <1 minute

### Monitoring During Migration

```bash
# Watch migration state in real-time
watch -n 5 "node -e \"import('./src/index.mjs').then(m => m.getState().then(console.log))\""

# Check health continuously
watch -n 10 "node -e \"import('./src/index.mjs').then(m => m.getHealth().then(console.log))\""
```

---

## Verification

### Pre-Migration Verification

```bash
# Verify all components healthy
node -e "import('./src/index.mjs').then(m => m.verifyAll().then(r => { console.log(r); process.exit(r.allHealthy ? 0 : 1); }))"
```

### Post-Phase Verification

After each phase completes, verify:

```bash
# Get current state
node -e "import('./src/index.mjs').then(m => m.getState().then(console.log))"

# Check phase results
node -e "import('./src/index.mjs').then(m => m.runMigration({ phases: ['discovery'] }).then(r => console.log(JSON.stringify(r.phaseResults, null, 2))))"
```

### Post-Migration Verification

```bash
# 1. Verify data count matches
# Compare entity counts from discovery phase with substrate counts

# 2. Spot check sample records
# Manually verify 10-20 random records match between systems

# 3. Run functional tests
# Execute key business flows against substrate

# 4. Check routing modes
node -e "import('./agent-7/routing-modes.mjs').then(m => console.log(m.listModes()))"

# 5. Performance validation
# Verify benchmarks meet SLAs
```

### Success Criteria

Migration is considered successful when:

- [ ] All 10 phases completed without errors
- [ ] Validation phase: `allValid === true`
- [ ] Shadow testing: `successRate >= 0.95`
- [ ] Benchmarking: latency improvement >= -10%, throughput >= -10%
- [ ] All routing modes set to `SUBSTRATE_ONLY`
- [ ] Manual spot checks pass (100% of sample)
- [ ] Functional tests pass (100%)
- [ ] No errors in logs for 1 hour post-cutover

---

## Rollback Procedures

### When to Rollback

Rollback immediately if:

- Any phase fails validation
- Shadow testing success rate < 95%
- Performance degradation > 10%
- Critical functional test failures
- Data integrity issues detected
- Unrecoverable errors in migration phase

### Rollback Steps

```bash
# 1. Stop all traffic to substrate
node -e "import('./agent-7/routing-modes.mjs').then(m => { const modes = m.listModes(); for (const mode of modes) { m.setMode(mode.operation, m.ROUTING_MODES.LEGACY_ONLY); } })"

# 2. Verify legacy system operational
# Check legacy system health endpoint

# 3. Restore from backup (if data was modified)
# Use agent-10 rollback scripts

# 4. Notify stakeholders

# 5. Post-mortem analysis
# Review logs, errors, performance data
```

### Automated Rollback

```bash
# Quick rollback to legacy-only mode
node -e "import('./src/index.mjs').then(async m => { const modes = await m.listModes(); for (const mode of modes) { await m.setMode(mode.operation, 'LEGACY_ONLY'); } console.log('Rollback complete'); })"
```

### Post-Rollback Verification

```bash
# Verify all modes back to LEGACY_ONLY
node -e "import('./agent-7/routing-modes.mjs').then(m => { const stats = m.getModeStats(); console.log(stats); })"

# Expected: All modes in LEGACY_ONLY
```

---

## Troubleshooting

### Common Issues

#### Issue: Discovery phase fails

**Symptoms:**
- Error: "Cannot connect to legacy database"
- Phase status: failed

**Resolution:**
1. Verify database credentials in `.env`
2. Check network connectivity: `ping $LEGACY_DB_HOST`
3. Verify firewall rules allow connection
4. Check database user permissions

#### Issue: Validation phase fails

**Symptoms:**
- `allValid === false`
- Errors in validation results

**Resolution:**
1. Review validation errors: Check `phaseResults.validation.validationResults`
2. Fix data issues in legacy system
3. Re-run from validation phase: `runMigration({ phases: ['validation', 'migration', ...] })`

#### Issue: Shadow testing low success rate

**Symptoms:**
- `successRate < 0.95`
- Acceptable: false

**Resolution:**
1. Review failed test cases
2. Check for schema mapping issues
3. Verify data consistency
4. Adjust batch size if timeout-related
5. Re-run shadow testing phase

#### Issue: Performance degradation

**Symptoms:**
- Latency increase > 10%
- Throughput decrease > 10%

**Resolution:**
1. Check substrate resource utilization
2. Review query patterns and indexes
3. Enable substrate query caching
4. Consider scaling substrate horizontally
5. Re-run benchmarks after optimization

#### Issue: Migration hangs/times out

**Symptoms:**
- Phase stuck in "running" state
- No progress for > 10 minutes

**Resolution:**
1. Check process status: `ps aux | grep node`
2. Review logs for errors
3. Increase timeout: `MIGRATION_TIMEOUT=600000`
4. Reduce batch size: `MIGRATION_BATCH_SIZE=500`
5. Kill process and restart with smaller scope

### Debug Mode

```bash
# Enable verbose logging
NODE_ENV=development node -e "import('./src/index.mjs').then(m => m.runMigration({ dryRun: true }).then(console.log))"

# Check individual agent health
node -e "import('./agent-6/impact-set.mjs').then(m => console.log(m.computeImpactSet({ type: 'TEST', path: '/test' })))"
```

### Getting Help

1. Review logs in `./logs/` directory
2. Check agent-specific documentation in `agent-*/README.md`
3. Consult CLAUDE.md for development guidelines
4. File issue with:
   - Migration state output
   - Error messages and stack traces
   - Phase results JSON
   - System configuration

---

## Agent Responsibilities

### Agent 1 - Control Plane (This System)
- Orchestrates all 10 phases
- Manages migration state
- Provides health checks
- Coordinates agent interactions

### Agent 2 - Discovery
- Scans legacy system
- Identifies entities and relationships
- Outputs: entity counts, schema structure

### Agent 3 - Schema Mapping
- Maps legacy schemas to RDF
- Generates property mappings
- Creates ontology definitions

### Agent 4 - Validation
- Validates data integrity
- Checks constraints
- Verifies mappings

### Agent 5 - Migration Execution
- Executes batch data migration
- Tracks progress
- Handles errors and retries

### Agent 6 - Impact Analysis
- Computes read/write/delete impacts
- Detects operation overlaps
- Optimizes execution order

### Agent 7 - Routing Modes
- Manages shadow routing configuration
- Controls traffic routing (LEGACY_ONLY → SUBSTRATE_ONLY)
- Supports gradual migration

### Agent 8 - Shadow Testing
- Runs dual writes/reads
- Compares results
- Computes success rates

### Agent 9 - Performance Benchmarking
- Measures latency and throughput
- Compares legacy vs substrate
- Validates performance requirements

### Agent 10 - Rollback Preparation
- Creates backups
- Generates rollback scripts
- Tests rollback procedures

---

## Appendix

### Command Reference

```bash
# Full migration
node -e "import('./src/index.mjs').then(m => m.runMigration().then(console.log))"

# Dry run
node -e "import('./src/index.mjs').then(m => m.runMigration({ dryRun: true }).then(console.log))"

# Specific phases
node -e "import('./src/index.mjs').then(m => m.runMigration({ phases: ['discovery', 'impact-analysis'] }).then(console.log))"

# Health check
node -e "import('./src/index.mjs').then(m => m.getHealth().then(console.log))"

# Verify all
node -e "import('./src/index.mjs').then(m => m.verifyAll().then(console.log))"

# Get state
node -e "import('./src/index.mjs').then(m => m.getState().then(console.log))"

# List phases
node -e "import('./src/index.mjs').then(m => console.log(m.getPhases()))"

# Reset (testing only)
node -e "import('./src/index.mjs').then(m => m.reset().then(console.log))"
```

### Environment Variables

| Variable | Description | Default | Required |
|----------|-------------|---------|----------|
| `LEGACY_DB_HOST` | Legacy database host | - | Yes |
| `LEGACY_DB_PORT` | Legacy database port | 5432 | No |
| `LEGACY_DB_NAME` | Legacy database name | - | Yes |
| `LEGACY_DB_USER` | Database user | - | Yes |
| `LEGACY_DB_PASSWORD` | Database password | - | Yes |
| `SUBSTRATE_URL` | Substrate API URL | - | Yes |
| `SUBSTRATE_API_KEY` | Substrate API key | - | Yes |
| `MIGRATION_BATCH_SIZE` | Records per batch | 1000 | No |
| `MIGRATION_TIMEOUT` | Phase timeout (ms) | 300000 | No |
| `MIGRATION_DRY_RUN` | Dry run mode | false | No |

### File Structure

```
ENTERPRISE_MIGRATION/
├── agent-1/
│   └── control-plane.mjs       # Main orchestration (this file)
├── agent-2/
│   └── discovery.mjs           # Legacy system discovery (placeholder)
├── agent-3/
│   └── schema-mapping.mjs      # Schema to RDF mapping (placeholder)
├── agent-4/
│   └── validation.mjs          # Data validation (placeholder)
├── agent-5/
│   └── migration.mjs           # Migration execution (placeholder)
├── agent-6/
│   └── impact-set.mjs          # Impact analysis (implemented)
├── agent-7/
│   └── routing-modes.mjs       # Routing configuration (implemented)
├── agent-8/
│   └── shadow-testing.mjs      # Shadow tests (placeholder)
├── agent-9/
│   └── benchmarking.mjs        # Performance benchmarks (placeholder)
├── agent-10/
│   └── rollback.mjs            # Rollback procedures (placeholder)
├── src/
│   └── index.mjs               # Main entry point
├── test/
│   └── (test files)
├── package.json                # Package configuration
└── RUNBOOK.md                  # This file
```

---

## Change Log

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-12-26 | 1.0.0 | Initial runbook creation | Agent-1 |

---

## License

Internal use only. Do not distribute.
