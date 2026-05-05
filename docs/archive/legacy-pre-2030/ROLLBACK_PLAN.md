# UNRDF v6.0.0 Production Rollback Plan

**Version:** 6.0.0  
**Last Updated:** 2026-04-03  
**Status:** Production-Ready  
**Incident Response SLA:** 15 minutes decision → 45 minutes full rollback

---

## Executive Summary

This document defines the complete incident response and rollback procedure for UNRDF v6.0.0 production deployments. It addresses decision criteria, technical procedures, data compatibility, and communication protocols for rapid recovery to v5.x if critical issues occur.

**Key Contacts:**

- Incident Commander: On-call engineering lead
- Data Integrity Owner: Database/RDF team lead
- Communication Lead: Product/support manager

---

## 1. Rollback Decision Tree

### 1.1 Detection Criteria (Auto-Trigger Alerts)

```
CRITICAL (Immediate Rollback):
├─ P0: Complete service unavailability (>5 min)
├─ P0: Data corruption detected (OTEL span anomalies ≥1000/s)
├─ P0: Security breach confirmed (unauth query access)
├─ P0: RDF store read/write failures (>50% of requests)
└─ P0: ΔGate receipt validation failure (core contract broken)

HIGH (Escalate to Incident Commander):
├─ P1: 50%+ API error rate (>2 min sustained)
├─ P1: Receipt chain integrity compromised
├─ P1: Oxigraph SPARQL engine memory leak (>2GB/min growth)
├─ P1: v6-compat bridge silent data loss detected
└─ P1: Consensus quorum lost (federation >30s offline)

MEDIUM (Monitor, Consider Rollback):
├─ P2: 20-50% error rate (localized to feature)
├─ P2: Performance degradation >50% (p95 latency)
├─ P2: Configuration migration failures (partial)
└─ P2: CLI command regression (backward compatibility)
```

### 1.2 Decision Authority & Timeline

**Incident Commander Decision Matrix:**

| Severity | Authority                       | Decision Time | Action                       |
| -------- | ------------------------------- | ------------- | ---------------------------- |
| P0       | Incident Commander              | ≤5 minutes    | AUTO-TRIGGER rollback        |
| P1       | Incident Commander + Data Owner | ≤10 minutes   | EXEC rollback if confirmed   |
| P2       | Engineering Lead + Data Owner   | ≤30 minutes   | EVALUATE rollback vs. hotfix |
| P3       | Product Manager                 | As needed     | Consider for next release    |

**Decision Framework:**

```
if (criticalityScore >= 8/10 AND timeToMitigation > 30min) {
  ROLLBACK = true;
} else if (dataCorruptionDetected) {
  ROLLBACK = true;
} else if (consensusLost && federatedDeploy) {
  ROLLBACK = true;
} else {
  ESCALATE_TO_INCIDENT_COMMANDER;
}
```

### 1.3 Time Estimates (SLA)

| Phase                      | Duration   | Target                  | Notes                   |
| -------------------------- | ---------- | ----------------------- | ----------------------- |
| Detection → Alert          | 2 min      | Auto-triggered          | OTEL threshold breach   |
| Alert → Decision           | 5 min      | IC decision             | P0: auto, P1-P2: manual |
| Decision → Execution Start | 2 min      | Begin rollback commands | Comms + validation      |
| Rollback Execution         | 20-30 min  | Single datacenter       | Includes config revert  |
| Post-Rollback Validation   | 10 min     | Health checks pass      | Services stable         |
| **Total Recovery SLA**     | **45 min** | **Restore service**     | From P0 detection       |

### 1.4 Data Loss Assessment

**v6 → v5 Rollback Data Safety:**

```
ZERO DATA LOSS guarantee if:
✅ Rollback within 2 hours of issue detection
✅ RDF store backups taken (hourly automated)
✅ Streaming changesets persisted (30-day retention)
✅ Receipt chain backups exist (immutable log)

POTENTIAL DATA LOSS:
⚠️ In-flight mutations NOT flushed to store (≤100 ops)
⚠️ Unverified receipts NOT in chain (≤50 receipts)
⚠️ Consensus votes NOT applied (federation edge case)
⚠️ Cache inconsistencies (re-sync needed)

RECOVERY PROCEDURES:
→ Restore from hourly RDF backup (< 1 hour data loss)
→ Replay changeset log from v6 → v5 format
→ Validate receipt chain integrity (cryptographic)
→ Rebuild consensus state from Raft logs
```

---

## 2. Quick Revert Procedure

### 2.1 Pre-Rollback Validation (5 minutes)

**BEFORE executing rollback, verify:**

```bash
# 1. Confirm service status
curl -s https://api.unrdf.example/health | jq '.status'
# Expected: "unhealthy" or timeout (confirms need for rollback)

# 2. Verify backups exist and are recent
aws s3 ls s3://unrdf-backups/rdf-store/hourly/ | tail -5
# Expected: 4-5 backups from last 4-5 hours (recent)

# 3. Check Raft consensus state
curl -s http://localhost:3000/consensus/status | jq '.cluster.healthy'
# Expected: false or error (confirms breakdown)

# 4. Verify v5.x container images are in registry
docker pull unrdf-registry/unrdf-core:5.5.0
docker pull unrdf-registry/unrdf-gateway:5.5.0
# Expected: Pull succeeds (images available for immediate deployment)

# 5. Backup v6 state for post-incident analysis
aws s3 cp /var/lib/unrdf/rdf-store s3://unrdf-backups/incident/ --recursive
# Preserve state for debugging (< 2 min)
```

### 2.2 Step-by-Step Revert (20-30 minutes)

**Phase 1: Notification & Lock (2 minutes)**

```bash
# 1. Notify all services of rollback intent
# Send to #incident-response channel
echo "🚨 ROLLBACK INITIATED: v6.0.0 → v5.5.0
  Issue: [INCIDENT_DESCRIPTION]
  ETA: 30 min to full recovery
  Maintenance window: 15:30-16:30 UTC" | send_slack_notification

# 2. Acquire rollback lock (prevent concurrent changes)
touch /var/lib/unrdf/.rollback-in-progress
chmod 444 /var/lib/unrdf/.rollback-in-progress  # Read-only lock
```

**Phase 2: Container Downgrade (5 minutes)**

```bash
# 1. Stop v6 services gracefully (30s drain period)
kubectl scale deployment unrdf-core --replicas=0 -n production
kubectl scale deployment unrdf-gateway --replicas=0 -n production
sleep 30  # Allow graceful shutdown

# 2. Update image versions in deployment manifests
# File: k8s/production/deployment.yaml
sed -i 's|unrdf-core:6.0.0|unrdf-core:5.5.0|g' k8s/production/deployment.yaml
sed -i 's|unrdf-gateway:6.0.0|unrdf-gateway:5.5.0|g' k8s/production/deployment.yaml

# 3. Scale up v5 services (3-5 replicas)
kubectl scale deployment unrdf-core --replicas=3 -n production
kubectl scale deployment unrdf-gateway --replicas=2 -n production

# 4. Wait for readiness (services report healthy)
kubectl rollout status deployment/unrdf-core -n production --timeout=5m
kubectl rollout status deployment/unrdf-gateway -n production --timeout=5m
```

**Phase 3: Configuration Revert (3 minutes)**

```bash
# 1. Restore v5 configuration
cp /etc/unrdf/config.v5.backup.toml /etc/unrdf/config.toml

# 2. Reload environment variables
export UNRDF_VERSION=5.5.0
export UNRDF_RDF_ENGINE=n3  # v5 default (not oxigraph)
export UNRDF_FEATURE_V6_COMPAT=false  # Disable v6 features

# 3. Restart services with new config
kubectl rollout restart deployment/unrdf-core -n production
kubectl rollout restart deployment/unrdf-gateway -n production
sleep 10  # Config propagation
```

**Phase 4: Data Restoration (10-15 minutes)**

```bash
# 1. Stop all mutations (read-only mode)
export UNRDF_READ_ONLY=true
kubectl set env deployment/unrdf-core READ_ONLY=true -n production

# 2. Verify latest backup exists and is valid
BACKUP_DATE=$(date -d '1 hour ago' +%Y%m%d_%H%M%S)
BACKUP_PATH="s3://unrdf-backups/rdf-store/hourly/${BACKUP_DATE}"
aws s3 ls "$BACKUP_PATH"
# Expected: Lists backup files

# 3. Restore RDF store from backup
# Download backup to local disk
aws s3 sync "$BACKUP_PATH" /mnt/backup/restore/ --no-progress

# 4. Replace current store with backup
# (v5 store format - compatible with v5 engine)
rm -rf /var/lib/unrdf/rdf-store
mv /mnt/backup/restore /var/lib/unrdf/rdf-store

# 5. Re-enable mutations (if data validates)
export UNRDF_READ_ONLY=false
kubectl set env deployment/unrdf-core READ_ONLY=false -n production
```

**Phase 5: Validation & Re-enable (5 minutes)**

```bash
# 1. Verify services are healthy
curl -s https://api.unrdf.example/health | jq '.'
# Expected: {"status": "healthy", "version": "5.5.0"}

# 2. Check RDF store connectivity
curl -s https://api.unrdf.example/query -d '{query: "SELECT * WHERE { ?s ?p ?o } LIMIT 1"}' | jq '.results.bindings | length'
# Expected: ≥ 0 (queries working)

# 3. Verify receipt chain (if applicable)
curl -s https://api.unrdf.example/receipts/status | jq '.chainValid'
# Expected: true (chain intact)

# 4. Enable traffic to services (remove maintenance mode)
kubectl patch service unrdf-gateway -n production -p '{"spec":{"selector":{"version":"5.5.0"}}}'

# 5. Remove rollback lock
rm /var/lib/unrdf/.rollback-in-progress

# 6. Notify completion
echo "✅ ROLLBACK COMPLETE: Service restored to v5.5.0
  Completed: 16:00 UTC
  Data restored from: 15:00 UTC backup
  Next steps: Post-incident review" | send_slack_notification
```

### 2.3 Service Restart Procedure

**If container-level restart is insufficient:**

```bash
# 1. Full service restart (nuclear option, 3-5 min downtime)
systemctl stop unrdf-core.service
systemctl stop unrdf-gateway.service
sleep 5

# 2. Verify processes terminated
ps aux | grep unrdf | grep -v grep
# Expected: No processes running

# 3. Clear in-memory caches
rm -rf /var/lib/unrdf/cache/*
rm -rf /var/cache/unrdf/*

# 4. Restart services
systemctl start unrdf-core.service
systemctl start unrdf-gateway.service
systemctl status unrdf-core.service
systemctl status unrdf-gateway.service

# 5. Verify startup
sleep 10
curl -s http://localhost:3000/health
# Expected: 200 OK response
```

---

## 3. unrdf.toml Downgrade Path

### 3.1 Configuration Schema Changes (v6 → v5)

**Breaking changes in unrdf.toml:**

```toml
# v6.0.0 Configuration (REMOVE/MODIFY for v5)
[rdf]
engine = "oxigraph"  # v6 default (v5: "n3")
store_format = "binary"  # v6 (v5: "ntriples")

[v6]
deltagate_enabled = true  # v6 ONLY (remove for v5)
receipts_enabled = true  # v6 ONLY (remove for v5)
consensus_mode = "raft"  # v6 (v5: "simple")

[federation]
enabled = true  # v6 feature (v5: false)
cluster_mode = "distributed"  # v6 (v5: "standalone")
```

### 3.2 v6 Config → v5 Config Conversion Script

**Automated downgrade script (60 seconds):**

```bash
#!/bin/bash
# File: scripts/downgrade-config.sh
# Usage: ./scripts/downgrade-config.sh /etc/unrdf/config.toml

CONFIG_FILE=unrdf.toml}"
BACKUP_FILE="${CONFIG_FILE}.v6.backup"

echo "🔄 Converting config from v6 to v5..."

# 1. Backup v6 config
cp "$CONFIG_FILE" "$BACKUP_FILE"
echo "✓ Backed up v6 config to $BACKUP_FILE"

# 2. Remove v6-only sections
grep -v '^\[v6\]' "$CONFIG_FILE" | \
  grep -v '^\[federation\]' | \
  grep -v '^enabled' > "$CONFIG_FILE.tmp"

# 3. Modify RDF engine settings
sed -i 's/engine = "oxigraph"/engine = "n3"/g' "$CONFIG_FILE.tmp"
sed -i 's/store_format = "binary"/store_format = "ntriples"/g' "$CONFIG_FILE.tmp"

# 4. Restore v5 consensus defaults
sed -i 's/consensus_mode = "raft"/# consensus_mode = "simple" (v5 default)/g' "$CONFIG_FILE.tmp"

# 5. Disable federation features
sed -i 's/\[federation\]/#[federation] (v6 only)\n#/g' "$CONFIG_FILE.tmp"

# 6. Replace original config
mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"

echo "✓ Config downgraded to v5 format"
echo "✓ Backup saved: $BACKUP_FILE"
echo ""
echo "Changes made:"
echo "  - RDF engine: oxigraph → n3"
echo "  - Store format: binary → ntriples"
echo "  - Consensus: raft → simple"
echo "  - Federation: disabled"
```

### 3.3 Field-by-Field Downgrade Mapping

| v6 Field                  | v5 Equivalent | Action                   | Notes                      |
| ------------------------- | ------------- | ------------------------ | -------------------------- |
| `rdf.engine`              | "oxigraph"    | Change to "n3"           | Core engine change         |
| `rdf.store_format`        | "binary"      | Change to "ntriples"     | Format compatibility       |
| `v6.deltagate_enabled`    | (remove)      | Delete entire section    | v6-only feature            |
| `v6.receipts_enabled`     | (remove)      | Delete entire section    | v6-only feature            |
| `federation.enabled`      | "false"       | Comment out or set false | Disable distributed mode   |
| `federation.cluster_mode` | (remove)      | Delete if federation off | v6-only                    |
| `consensus.mode`          | (default)     | Remove, use v5 simple    | Revert to simple consensus |
| `query.timeout_ms`        | (unchanged)   | Keep as-is               | Backward compatible        |
| `security.tls_enabled`    | (unchanged)   | Keep as-is               | Backward compatible        |

### 3.4 Example: v6 Config → v5 Config

**Before (v6.0.0):**

```toml
[app]
version = "6.0.0"
name = "UNRDF"

[rdf]
engine = "oxigraph"
store_format = "binary"
cache_size_mb = 512
sparql_timeout_ms = 30000

[v6]
deltagate_enabled = true
receipts_enabled = true
consensus_mode = "raft"

[federation]
enabled = true
cluster_mode = "distributed"
node_id = "node-001"
discovery_service = "consul"

[query]
timeout_ms = 30000
max_results = 100000

[security]
tls_enabled = true
tls_cert_path = "/etc/unrdf/cert.pem"
```

**After (v5.5.0):**

```toml
[app]
version = "5.5.0"
name = "UNRDF"

[rdf]
engine = "n3"
store_format = "ntriples"
cache_size_mb = 512
sparql_timeout_ms = 30000

# [v6] section removed (v6-only features)
# [federation] section disabled (use standalone mode)

[query]
timeout_ms = 30000
max_results = 100000

[security]
tls_enabled = true
tls_cert_path = "/etc/unrdf/cert.pem"
```

---

## 4. Data Format Compatibility

### 4.1 RDF Store Format Compatibility Matrix

| Aspect                 | v6 (Oxigraph)              | v5 (N3)          | Compatibility                 | Action                    |
| ---------------------- | -------------------------- | ---------------- | ----------------------------- | ------------------------- |
| **Store Format**       | Binary (SPARQL-optimized)  | N-Triples (text) | Convert needed                | Restore from v5 backup    |
| **Triple Encoding**    | Binary (compressed)        | UTF-8 text       | One-way conversion            | Rebuild from text         |
| **Namespace Handling** | Prefixed (compact)         | Explicit URIs    | Lossless (prefixes preserved) | Auto-convert              |
| **Blank Node IDs**     | Internal (anonymized)      | Text identifiers | May differ                    | Rebuild blank nodes       |
| **Query Indices**      | SPARQL indices (optimized) | Sequential scan  | No indices in v5              | Query slower, but correct |
| **Streaming Support**  | Native changesets          | N-Triples append | Streaming format differs      | Convert to N-Triples      |

### 4.2 Triple Format Compatibility

**v6 to v5 format conversion (example):**

```
# v6 Oxigraph internal format (binary):
Subject: <node:s1>
Predicate: <http://example.org/name>
Object: "Alice" (string literal)
Graph: <default>

# v5 N3 format (text):
<http://example.org/s1> <http://example.org/name> "Alice" .
```

**Conversion procedure:**

```bash
# 1. Export all triples from v6 store (SPARQL query)
curl -X POST https://api.unrdf.example/query \
  -H "Content-Type: application/sparql-query" \
  -d "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" \
  > /tmp/triples.ntriples

# 2. Validate N-Triples format
head -n 5 /tmp/triples.ntriples
# Expected: Valid N-Triples lines (subject predicate object .)

# 3. Reload into v5 N3 engine
# v5 accepts N-Triples format natively
unrdf-cli load --format ntriples --file /tmp/triples.ntriples
```

### 4.3 Query Result Format Changes

**v6 query results:**

```json
{
  "results": {
    "bindings": [
      { "s": { "type": "uri", "value": "http://example.org/s1" } },
      { "p": { "type": "uri", "value": "http://example.org/name" } }
    ]
  },
  "_metadata": {
    "execution_time_ms": 1.2,
    "receipt_id": "rcpt-uuid",
    "verified": true
  }
}
```

**v5 query results:**

```json
{
  "results": {
    "bindings": [
      { "s": { "type": "uri", "value": "http://example.org/s1" } },
      { "p": { "type": "uri", "value": "http://example.org/name" } }
    ]
  }
}
```

**Compatibility:** v6 results are SUPERSET of v5 (v5 queries work in v6, but v6 features missing in v5).

### 4.4 Data Corruption Recovery Procedures

**If data becomes corrupted after rollback:**

```bash
# 1. Detect corruption (checksums fail)
unrdf-cli validate --checksum
# Output:
#   ✗ Triple s1:p1:o1 - checksum mismatch
#   ✗ Triple s2:p2:o2 - invalid format
#   Corruption detected in 234 triples (0.02%)

# 2. Backup corrupted store
mv /var/lib/unrdf/rdf-store /var/lib/unrdf/rdf-store.corrupted

# 3. Restore from backup (choose most recent valid backup)
LATEST_BACKUP="s3://unrdf-backups/rdf-store/hourly/20260403_120000"
aws s3 sync "$LATEST_BACKUP" /var/lib/unrdf/rdf-store/ --no-progress

# 4. Restart service
systemctl restart unrdf-core.service

# 5. Validate new store
unrdf-cli validate --checksum
# Expected: All checksums valid

# 6. Repair script (if backup unavailable - last resort)
# Rebuild store from N-Triples log (30-min process)
unrdf-cli repair --source /var/log/unrdf/mutations.ntriples --output /var/lib/unrdf/rdf-store
```

### 4.5 Backup & Restore Procedure

**Automated hourly backups (v6 compatible):**

```bash
#!/bin/bash
# File: scripts/backup-rdf-store.sh
# Runs hourly via cron

BACKUP_DIR="/mnt/backup"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_PATH="${BACKUP_DIR}/rdf-store/hourly/${TIMESTAMP}"

# 1. Export all triples in N-Triples format (v5 compatible)
echo "📦 Exporting RDF store..."
mkdir -p "$BACKUP_PATH"
unrdf-cli export --format ntriples --output "$BACKUP_PATH/triples.ntriples"

# 2. Backup receipt chain (immutable log)
echo "🔗 Backing up receipt chain..."
cp -r /var/lib/unrdf/receipts "$BACKUP_PATH/receipts"

# 3. Backup configuration
echo "⚙️  Backing up configuration..."
cp /etc/unrdf/config.toml "$BACKUP_PATH/config.toml"

# 4. Create backup manifest
cat > "$BACKUP_PATH/MANIFEST.json" <<EOF
{
  "timestamp": "$(date -Iseconds)",
  "version": "$(unrdf --version)",
  "files": {
    "triples": "triples.ntriples",
    "receipts": "receipts/",
    "config": "config.toml"
  },
  "checksum": "$(sha256sum "$BACKUP_PATH/triples.ntriples" | cut -d' ' -f1)"
}
EOF

# 5. Upload to S3
echo "☁️  Uploading to S3..."
aws s3 sync "$BACKUP_PATH" "s3://unrdf-backups/rdf-store/hourly/${TIMESTAMP}/" --no-progress

# 6. Cleanup local backups (keep last 30 days)
find "${BACKUP_DIR}/rdf-store/hourly" -mtime +30 -delete

echo "✅ Backup complete: $BACKUP_PATH"
```

**Restore from backup:**

```bash
#!/bin/bash
# File: scripts/restore-rdf-store.sh
# Usage: ./scripts/restore-rdf-store.sh <timestamp>

TIMESTAMP="${1:-$(ls /mnt/backup/rdf-store/hourly | sort -r | head -1)}"
BACKUP_PATH="/mnt/backup/rdf-store/hourly/${TIMESTAMP}"

echo "🔄 Restoring RDF store from backup: $TIMESTAMP"

# 1. Stop services (read-only first)
kubectl set env deployment/unrdf-core READ_ONLY=true -n production
sleep 5

# 2. Restore store
echo "📥 Loading triples..."
unrdf-cli load --format ntriples --file "$BACKUP_PATH/triples.ntriples"

# 3. Restore receipt chain
echo "🔗 Restoring receipt chain..."
cp -r "$BACKUP_PATH/receipts" /var/lib/unrdf/receipts

# 4. Restart services
echo "🚀 Restarting services..."
kubectl set env deployment/unrdf-core READ_ONLY=false -n production
systemctl restart unrdf-core.service

# 5. Validate
echo "✓ Validation..."
unrdf-cli validate --checksum

echo "✅ Restore complete"
```

---

## 5. CLI Command Differences (v5 vs v6)

### 5.1 Command Changes Summary

| Command    | v5 Syntax            | v6 Syntax                     | Status     | Migration               |
| ---------- | -------------------- | ----------------------------- | ---------- | ----------------------- |
| Load store | `unrdf load FILE`    | `unrdf store load FILE`       | BREAKING   | Add `store` subcommand  |
| Query      | `unrdf query SPARQL` | `unrdf query --sparql SPARQL` | BREAKING   | Add `--sparql` flag     |
| Export     | `unrdf export`       | `unrdf store export`          | BREAKING   | Add `store` subcommand  |
| Validate   | `unrdf validate`     | `unrdf validate --store`      | COMPATIBLE | Optional `--store` flag |
| Federation | (N/A)                | `unrdf federation status`     | NEW        | v6-only                 |
| Receipts   | (N/A)                | `unrdf receipts list`         | NEW        | v6-only                 |
| Health     | `unrdf health`       | `unrdf health`                | COMPATIBLE | Same in both            |

### 5.2 Before/After Command Examples

**Loading an RDF file:**

```bash
# v5 (old)
unrdf load /path/to/data.ntriples --format ntriples

# v6 (new)
unrdf store load /path/to/data.ntriples --format ntriples

# Migration: Add 'store' subcommand
```

**Running a SPARQL query:**

```bash
# v5 (old)
unrdf query "SELECT * WHERE { ?s ?p ?o }"

# v6 (new)
unrdf query --sparql "SELECT * WHERE { ?s ?p ?o }"

# Migration: Add --sparql flag
```

**Exporting data:**

```bash
# v5 (old)
unrdf export --format ntriples --output data.nt

# v6 (new)
unrdf store export --format ntriples --output data.nt

# Migration: Add 'store' subcommand
```

**Health check (no change):**

```bash
# v5 & v6 (identical)
unrdf health
# Output: {"status": "healthy", "version": "..."}
```

### 5.3 Argument/Option Changes

```bash
# v5 → v6 breaking changes

# Format option
v5: unrdf load FILE --format ntriples
v6: unrdf store load FILE --format ntriples

# Output option
v5: unrdf export --output data.nt
v6: unrdf store export --output data.nt

# Timeout option (UNCHANGED)
v5: unrdf query --timeout 5000
v6: unrdf query --timeout 5000  # v5 compatible

# Engine option (NEW in v6, N/A in v5)
v6: unrdf query --engine oxigraph
v5: (N/A - always uses n3)
```

### 5.4 Output Format Differences

**Health endpoint output:**

```bash
# v5 health
curl http://localhost:3000/health
# {"status": "healthy", "version": "5.5.0"}

# v6 health
curl http://localhost:3000/health
# {
#   "status": "healthy",
#   "version": "6.0.0",
#   "deltagate": {"enabled": true, "receipts": 1234},
#   "consensus": {"healthy": true, "nodes": 3},
#   "timestamp": "2026-04-03T15:30:00Z"
# }

# Migration: Parse v5 format only (ignore v6 extras)
```

### 5.5 Script Update Examples

**Update shell scripts for v5 compatibility:**

```bash
#!/bin/bash
# Before (v6)
unrdf store load data.ntriples --format ntriples
unrdf store export --format ntriples --output backup.nt

# After (v5 compatible)
if [[ $(unrdf --version) == "5"* ]]; then
  # v5 commands
  unrdf load data.ntriples --format ntriples
  unrdf export --format ntriples --output backup.nt
else
  # v6 commands
  unrdf store load data.ntriples --format ntriples
  unrdf store export --format ntriples --output backup.nt
fi
```

**Python/Node.js client updates:**

```javascript
// Before (v6-specific)
const result = await unrdf.store.load(data, { format: 'ntriples' });

// After (v5 compatible)
const version = await unrdf.getVersion();
if (version.startsWith('5')) {
  const result = await unrdf.load(data, { format: 'ntriples' });
} else {
  const result = await unrdf.store.load(data, { format: 'ntriples' });
}
```

---

## 6. Communication Plan

### 6.1 Incident Notification (Immediate)

**Slack notification template (send to #incident-response):**

```
🚨 PRODUCTION INCIDENT: UNRDF v6.0.0 Rollback

Issue: [SHORT DESCRIPTION - 1 sentence]
Severity: [P0/P1/P2]
Detected: [TIME UTC]
Incident Commander: @[NAME]

Status: 🔄 ROLLBACK IN PROGRESS
  → Estimated recovery: [X] minutes
  → Last update: [TIME UTC]
  → Next update: [TIME UTC+5min]

Affected Services:
  - UNRDF API: ⚠️ DEGRADED
  - Query Engine: ⚠️ DEGRADED
  - RDF Store: ⚠️ DEGRADED

Impact:
  - [Specific impact 1]
  - [Specific impact 2]
  - Data integrity: [SAFE/AT RISK]

Action: Downgrading from v6.0.0 → v5.5.0

More: [LINK TO INCIDENT LOG]
```

### 6.2 External Customer Communication

**Email template (send to customers if >30 min impact):**

```
Subject: RESOLVED: UNRDF Service Incident - April 3, 2026 15:30 UTC

Dear UNRDF Customers,

We experienced an incident affecting the UNRDF service on April 3, 2026,
starting at approximately 15:30 UTC.

INCIDENT SUMMARY:
  Duration: 45 minutes (15:30 - 16:15 UTC)
  Status: ✅ RESOLVED
  Root Cause: [BRIEF DESCRIPTION - under 2 sentences]

WHAT HAPPENED:
  [Technical summary for non-technical audience, 1-2 paragraphs]

RESOLUTION:
  We rolled back from v6.0.0 to v5.5.0, restoring full service at 16:15 UTC.
  Data integrity verified ✅
  All queries functioning normally ✅

CUSTOMER IMPACT:
  Query failures: [TIME PERIOD]
  Data loss: None
  Service availability: Now 100%

NEXT STEPS:
  - Post-incident review will complete by [DATE]
  - Root cause analysis available at [LINK]
  - No action required from you

We apologize for the disruption and appreciate your patience.

Best regards,
UNRDF Operations Team
status.unrdf.io | support@unrdf.io
```

### 6.3 Internal Incident Report Format

**Incident report (file to /incidents/[TIMESTAMP].md):**

```markdown
# Incident Report: UNRDF v6.0.0 Production Rollback

**Date:** April 3, 2026
**Duration:** 45 minutes (15:30 - 16:15 UTC)
**Status:** Resolved

## Executive Summary

[1-2 paragraph summary]

## Timeline

| Time UTC | Event                      | Owner      |
| -------- | -------------------------- | ---------- |
| 15:30    | Issue detected (alert)     | Monitoring |
| 15:35    | Incident commander engaged | On-call    |
| 15:40    | Rollback decision made     | IC         |
| 15:42    | Rollback started           | SRE        |
| 16:15    | Service restored           | SRE        |

## Root Cause

[Detailed root cause analysis]

## Impact Assessment

- Queries failed: [COUNT]
- Data loss: None
- Users affected: [COUNT]

## Resolution

Rolled back from v6.0.0 → v5.5.0 successfully.

## Preventive Actions

1. [Action 1]
2. [Action 2]
3. [Action 3]

## Owner: [NAME]

## Reviewers: [NAMES]
```

---

## 7. Validation After Rollback

### 7.1 Immediate Post-Rollback Checks (5 minutes)

**Run these checks in sequence:**

```bash
# 1. Service health (API endpoint)
echo "1. Checking service health..."
curl -s http://localhost:3000/health | jq '.'
# Expected: {"status": "healthy", "version": "5.5.0"}

# 2. RDF store connectivity
echo "2. Checking RDF store..."
curl -s -X POST http://localhost:3000/query \
  -H "Content-Type: application/sparql-query" \
  -d "SELECT COUNT(*) WHERE { ?s ?p ?o }" | jq '.results.bindings[0]'
# Expected: Triple count ≥ 0

# 3. CLI health
echo "3. Checking CLI..."
unrdf health
# Expected: "OK" or similar

# 4. Config validation
echo "4. Validating config..."
unrdf config validate
# Expected: No errors

# 5. Version confirmation
echo "5. Checking version..."
unrdf --version
# Expected: 5.5.0 or 5.x.x
```

### 7.2 Data Integrity Validation (10 minutes)

**Verify no data loss occurred:**

```bash
# 1. Checksum validation
echo "Validating data integrity..."
unrdf-cli validate --checksum
# Expected: ✓ All checksums valid

# 2. Triple count verification
CURRENT_COUNT=$(curl -s -X POST http://localhost:3000/query \
  -H "Content-Type: application/sparql-query" \
  -d "SELECT COUNT(*) AS ?count WHERE { ?s ?p ?o }" | \
  jq -r '.results.bindings[0].count.value')

EXPECTED_COUNT=$(cat /var/lib/unrdf/triple-count.baseline)
# baseline recorded before rollback

if [[ $CURRENT_COUNT -eq $EXPECTED_COUNT ]]; then
  echo "✓ Triple count matches baseline: $CURRENT_COUNT"
else
  echo "⚠️  Triple count mismatch: $CURRENT_COUNT vs $EXPECTED_COUNT"
fi

# 3. Namespace validation (sample 10 random triples)
unrdf-cli export --format ntriples --limit 10 | \
  while read triple; do
    # Verify valid N-Triples format
    if [[ $triple =~ ^'<'.*'>'.*'>'.*\. ]]; then
      echo "✓ Valid triple: ${triple:0:80}..."
    else
      echo "✗ Invalid triple: $triple"
    fi
  done
```

### 7.3 Performance Baseline Verification

**Ensure performance meets v5 standards:**

```bash
# 1. Simple query latency (should be <50ms)
TIME_START=$(date +%s%N)
curl -s -X POST http://localhost:3000/query \
  -H "Content-Type: application/sparql-query" \
  -d "SELECT * WHERE { ?s ?p ?o } LIMIT 10" > /dev/null
TIME_END=$(date +%s%N)
LATENCY_MS=$(( (TIME_END - TIME_START) / 1000000 ))

echo "Query latency: ${LATENCY_MS}ms"
if [[ $LATENCY_MS -lt 50 ]]; then
  echo "✓ Performance OK (< 50ms)"
else
  echo "⚠️  Performance degraded (> 50ms)"
fi

# 2. Throughput test (100 sequential queries)
echo "Running throughput test..."
time for i in {1..100}; do
  curl -s -X POST http://localhost:3000/query \
    -H "Content-Type: application/sparql-query" \
    -d "SELECT * WHERE { ?s ?p ?o } LIMIT 1" > /dev/null
done
# Expected: Completes in <10 seconds

# 3. Memory footprint check
MEMORY_USAGE=$(ps aux | grep unrdf-core | grep -v grep | awk '{print $6}')
echo "Memory usage: ${MEMORY_USAGE}MB"
if [[ $MEMORY_USAGE -lt 1000 ]]; then
  echo "✓ Memory OK (< 1GB)"
else
  echo "⚠️  Memory elevated (> 1GB)"
fi
```

### 7.4 Functional Testing Suite (15 minutes)

**Run automated test suite to verify functionality:**

```bash
#!/bin/bash
# File: scripts/post-rollback-tests.sh

echo "Running post-rollback functional tests..."

TEST_PASSED=0
TEST_FAILED=0

# Test 1: Insert triple
echo -n "Test 1: Insert triple... "
RESULT=$(curl -s -X POST http://localhost:3000/insert \
  -H "Content-Type: application/json" \
  -d '{"s": "http://example.org/s1", "p": "http://example.org/p1", "o": "http://example.org/o1"}')
if echo "$RESULT" | jq -e '.success' > /dev/null; then
  echo "✓ PASS"
  ((TEST_PASSED++))
else
  echo "✗ FAIL"
  ((TEST_FAILED++))
fi

# Test 2: Query inserted triple
echo -n "Test 2: Query inserted triple... "
RESULT=$(curl -s -X POST http://localhost:3000/query \
  -H "Content-Type: application/sparql-query" \
  -d 'SELECT * WHERE { ?s ?p ?o } FILTER(?s = <http://example.org/s1>)')
if echo "$RESULT" | jq -e '.results.bindings | length > 0' > /dev/null; then
  echo "✓ PASS"
  ((TEST_PASSED++))
else
  echo "✗ FAIL"
  ((TEST_FAILED++))
fi

# Test 3: Export data
echo -n "Test 3: Export data... "
RESULT=$(curl -s -X GET http://localhost:3000/export?format=ntriples)
if echo "$RESULT" | grep -q '^<' ; then
  echo "✓ PASS"
  ((TEST_PASSED++))
else
  echo "✗ FAIL"
  ((TEST_FAILED++))
fi

# Test 4: Delete triple
echo -n "Test 4: Delete triple... "
RESULT=$(curl -s -X POST http://localhost:3000/delete \
  -H "Content-Type: application/json" \
  -d '{"s": "http://example.org/s1", "p": "http://example.org/p1", "o": "http://example.org/o1"}')
if echo "$RESULT" | jq -e '.success' > /dev/null; then
  echo "✓ PASS"
  ((TEST_PASSED++))
else
  echo "✗ FAIL"
  ((TEST_FAILED++))
fi

echo ""
echo "Results: $TEST_PASSED passed, $TEST_FAILED failed"

if [[ $TEST_FAILED -eq 0 ]]; then
  echo "✅ All tests passed"
  exit 0
else
  echo "❌ Some tests failed"
  exit 1
fi
```

### 7.5 Health Dashboard Verification

**Check monitoring dashboards:**

```bash
# 1. Prometheus metrics (5-min window)
curl -s "http://prometheus:9090/api/v1/query_range?query=up{job='unrdf'}&start=$(date -d '5 min ago' +%s)&end=$(date +%s)&step=60" | jq '.data.result'
# Expected: up == 1 for all instances

# 2. Grafana dashboard checks
# Visit: https://grafana.example.com/d/unrdf-health
# Verify:
#   ✓ Service availability: 100%
#   ✓ Error rate: 0%
#   ✓ Latency (p95): <100ms
#   ✓ Memory: <1GB

# 3. Log verification (check for errors)
kubectl logs -n production deployment/unrdf-core --tail=100 | grep -i error
# Expected: No ERROR messages
```

---

## Appendix: Emergency Contact & Escalation

### Incident Commander Escalation

1. **Primary:** [ON-CALL ENGINEER]
2. **Secondary:** [BACKUP ENGINEER]
3. **VP Engineering:** [NAME]

### Slack Channels

- `#incident-response` - Incident coordination
- `#unrdf-alerts` - Automated alerts
- `#operations` - Operations team

### Post-Rollback Review Schedule

- **+1 hour:** Incident report filed
- **+24 hours:** Root cause analysis completed
- **+1 week:** Preventive actions implemented

---

**Document Version:** 1.0  
**Last Updated:** 2026-04-03  
**Next Review:** 2026-06-03 (quarterly)  
**Owner:** Infrastructure / SRE Team
