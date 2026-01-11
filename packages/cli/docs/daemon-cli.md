# Daemon CLI Reference

> Comprehensive guide for UNRDF daemon command-line interface for background operation management, scheduling, and cluster monitoring.

## Overview

The `unrdf daemon` command provides a powerful interface for managing background operations, scheduling tasks, monitoring daemon health, and inspecting Raft cluster status. All commands support both human-readable and JSON output formats.

## Quick Start

```bash
# List all configured operations
unrdf daemon list

# Run an operation immediately
unrdf daemon run backup-graphs

# Schedule an operation with a cron trigger
unrdf daemon schedule sync-federation cron

# Check daemon health status
unrdf daemon status

# View operation logs
unrdf daemon logs --follow

# Display daemon configuration
unrdf daemon config

# Show Raft cluster status
unrdf daemon cluster
```

## Commands

### daemon list

List all configured operations with their current status.

**Usage:**
```bash
unrdf daemon list [--json] [--include-metadata]
```

**Options:**
- `--json` - Output results in JSON format instead of human-readable table
- `--include-metadata` - Include operation metadata in the output

**Examples:**

```bash
# List operations in table format
$ unrdf daemon list

📋 Configured Operations
══════════════════════════════════════════════════════════════════════
ID                       Name                           Status
──────────────────────────────────────────────────────────────────────
backup-graphs            Backup RDF Graphs              scheduled
cleanup-temp             Cleanup Temporary Files        scheduled
sync-federation          Synchronize Federation Nodes   scheduled
compact-storage          Compact Storage Engine         scheduled
validate-integrity       Validate Data Integrity        scheduled
──────────────────────────────────────────────────────────────────────
Total Operations: 5
══════════════════════════════════════════════════════════════════════
```

```bash
# List operations with metadata
$ unrdf daemon list --include-metadata

# List operations as JSON
$ unrdf daemon list --json
{
  "operations": [
    {
      "id": "backup-graphs",
      "name": "Backup RDF Graphs",
      "status": "scheduled",
      "createdAt": "2026-01-11T10:30:00.000Z",
      "metadata": {
        "category": "maintenance",
        "priority": "high"
      }
    },
    ...
  ]
}
```

### daemon run

Execute a configured operation immediately with optional payload.

**Usage:**
```bash
unrdf daemon run <operation> [payload] [--json] [--timeout ms]
```

**Arguments:**
- `<operation>` - Operation ID to execute (required)
- `[payload]` - JSON payload for the operation (optional)

**Options:**
- `--json` - Output results in JSON format
- `--timeout ms` - Execution timeout in milliseconds (default: 30000)

**Examples:**

```bash
# Run operation without payload
$ unrdf daemon run backup-graphs

✅ Operation executed successfully
══════════════════════════════════════════════
Operation ID: backup-graphs
Status: success
Duration: 542ms
Processed: 47 items
Affected: 23 items
══════════════════════════════════════════════
```

```bash
# Run operation with JSON payload
$ unrdf daemon run backup-graphs '{"targetPath": "/mnt/backups"}' --json
{
  "operationId": "backup-graphs",
  "status": "success",
  "duration": 542,
  "result": {
    "processed": 47,
    "affected": 23
  }
}
```

```bash
# Run operation with custom timeout
$ unrdf daemon run cleanup-temp --timeout 60000
```

### daemon schedule

Schedule an operation with a trigger (cron, interval, reactive, event, or idle).

**Usage:**
```bash
unrdf daemon schedule <operation> <trigger> [payload] [--json]
```

**Arguments:**
- `<operation>` - Operation ID to schedule (required)
- `<trigger>` - Trigger type: `cron`, `interval`, `reactive`, `event`, `idle` (required)
- `[payload]` - Trigger configuration payload (optional)

**Options:**
- `--json` - Output results in JSON format

**Trigger Types:**

| Type | Description | Example |
|------|-------------|---------|
| `cron` | POSIX cron expression with timezone support | `0 2 * * *` (daily at 2 AM) |
| `interval` | Fixed interval in milliseconds | `3600000` (hourly) |
| `reactive` | Trigger on entity mutations | `create`, `update`, `delete` |
| `event` | Custom event-based triggering | `data-changed`, `user-action` |
| `idle` | Trigger when daemon is idle beyond threshold | `60000` (1 minute idle) |

**Examples:**

```bash
# Schedule daily backup with cron
$ unrdf daemon schedule backup-graphs cron

📅 Trigger scheduled successfully
══════════════════════════════════════════════
Operation ID: backup-graphs
Trigger ID: trigger-1673355000000
Trigger Type: cron
Description: 0 2 * * *
Status: scheduled
Enabled: true
══════════════════════════════════════════════
```

```bash
# Schedule synchronization on interval (hourly)
$ unrdf daemon schedule sync-federation interval --json
{
  "operationId": "sync-federation",
  "triggerId": "trigger-1673355000000",
  "triggerType": "interval",
  "triggerDescription": "3600000",
  "status": "scheduled",
  "enabled": true
}
```

```bash
# Schedule validation on reactive trigger (mutations)
$ unrdf daemon schedule validate-integrity reactive

# Schedule compaction on custom event
$ unrdf daemon schedule compact-storage event
```

### daemon status

Display comprehensive daemon health and performance metrics.

**Usage:**
```bash
unrdf daemon status [--json] [--include-metrics]
```

**Options:**
- `--json` - Output results in JSON format
- `--include-metrics` - Include detailed performance metrics

**Examples:**

```bash
# Show daemon status
$ unrdf daemon status

⚡ Daemon Status
══════════════════════════════════════════════
Node ID: node-default
Cluster ID: default-cluster
Running: ✅ Yes
Leader: 👑 Yes
Uptime: 2h 34m
Active Operations: 2
Queued Operations: 5
Completed Operations: 127
══════════════════════════════════════════════
```

```bash
# Show status with metrics
$ unrdf daemon status --include-metrics

⚡ Daemon Status
...
📊 Metrics
──────────────────────────────────────────────
Total Operations: 127
Successful: 121
Failed: 6
Success Rate: 95.28%
Average Duration: 1.23s
Total Duration: 2m 34s
══════════════════════════════════════════════
```

```bash
# Show status as JSON
$ unrdf daemon status --json --include-metrics
{
  "health": {
    "nodeId": "node-default",
    "clusterId": "default-cluster",
    "isRunning": true,
    "isLeader": true,
    "uptime": 9240000,
    "activeOperations": 2,
    "queuedOperations": 5,
    "completedOperations": 127,
    "timestamp": "2026-01-11T12:30:00.000Z"
  },
  "metrics": {
    "totalOperations": 127,
    "successfulOperations": 121,
    "failedOperations": 6,
    "successRate": 95.28,
    "averageDuration": 1230,
    "totalDuration": 154320,
    "timestamp": "2026-01-11T12:30:00.000Z"
  }
}
```

### daemon logs

View and filter operation logs with optional streaming.

**Usage:**
```bash
unrdf daemon logs [--follow] [--filter pattern] [--max-lines N] [--json]
```

**Options:**
- `--follow` - Stream new log entries (tail -f mode)
- `--filter pattern` - Filter logs by regex pattern
- `--max-lines N` - Maximum lines to display (default: 100)
- `--json` - Output results in JSON format

**Examples:**

```bash
# View recent operation logs
$ unrdf daemon logs

📝 Operation Logs
════════════════════════════════════════════════════════════════════════════════════════════════
Timestamp                Type                 Operation ID         Duration
──────────────────────────────────────────────────────────────────────────────────────────────────
2026-01-11T12:29:45.123Z operation:started    backup-graphs        -
2026-01-11T12:29:46.234Z operation:success    backup-graphs        1.111s
2026-01-11T12:30:00.456Z trigger:scheduled    sync-federation      -
2026-01-11T12:30:15.789Z operation:started    cleanup-temp         -
2026-01-11T12:30:18.890Z operation:success    cleanup-temp         3.101s
────────────────────────────────────────────────────────────────────────────────────────────────
Total Entries: 5
════════════════════════════════════════════════════════════════════════════════════════════════
```

```bash
# Follow logs in real-time
$ unrdf daemon logs --follow
📡 Following logs (Ctrl+C to stop)...

# Filter logs for specific operation
$ unrdf daemon logs --filter backup-graphs

# Show last 50 lines with JSON output
$ unrdf daemon logs --max-lines 50 --json
{
  "logs": [
    {
      "type": "operation:started",
      "operationId": "backup-graphs",
      "timestamp": "2026-01-11T12:29:45.123Z"
    },
    {
      "type": "operation:success",
      "operationId": "backup-graphs",
      "duration": 1111,
      "timestamp": "2026-01-11T12:29:46.234Z"
    },
    ...
  ]
}
```

### daemon config

Display current daemon configuration and feature status.

**Usage:**
```bash
unrdf daemon config [--json]
```

**Options:**
- `--json` - Output configuration in JSON format

**Examples:**

```bash
# Show daemon configuration
$ unrdf daemon config

⚙️  Daemon Configuration
══════════════════════════════════════════════
Daemon ID: default-daemon
Port: 8080
Node ID: node-default
Cluster ID: default-cluster
Max Concurrent: 10
Health Check Interval: 30s
══════════════════════════════════════════════

🔧 Features
──────────────────────────────────────────────
clustering          ✅ Enabled
federation          ✅ Enabled
streaming           ✅ Enabled
observability       ✅ Enabled

⚙️  Supported Operation Types
──────────────────────────────────────────────
  • cron
  • interval
  • reactive
  • event
  • idle

🔄 Retry Policy
──────────────────────────────────────────────
Max Attempts: 3
Initial Backoff: 1s
Multiplier: 2x
Max Backoff: 30s
Jitter Factor: 10%

📊 Logging
──────────────────────────────────────────────
Level: info
Format: json
Destination: stdout
══════════════════════════════════════════════
```

```bash
# Show configuration as JSON
$ unrdf daemon config --json
{
  "config": {
    "daemonId": "default-daemon",
    "port": 8080,
    "nodeId": "node-default",
    "clusterId": "default-cluster",
    "maxConcurrent": 10,
    "healthCheckInterval": 30000,
    "features": {
      "clustering": true,
      "federation": true,
      "streaming": true,
      "observability": true
    },
    "operationTypes": ["cron", "interval", "reactive", "event", "idle"],
    "retryPolicy": {
      "maxAttempts": 3,
      "backoffMs": 1000,
      "backoffMultiplier": 2,
      "maxBackoffMs": 30000,
      "jitterFactor": 0.1
    },
    "logging": {
      "level": "info",
      "format": "json",
      "destination": "stdout"
    }
  }
}
```

### daemon cluster

Display Raft cluster status and member information.

**Usage:**
```bash
unrdf daemon cluster [--json] [--include-metrics]
```

**Options:**
- `--json` - Output results in JSON format
- `--include-metrics` - Include detailed replication metrics per member

**Examples:**

```bash
# Show cluster status
$ unrdf daemon cluster

👥 Cluster Status
════════════════════════════════════════════════════════════════════════════════════════
Cluster ID: default-cluster
Leader: node-0
Current Term: 5
Quorum Size: 2
Status: ✅ Healthy
════════════════════════════════════════════════════════════════════════════════════════

📋 Members
────────────────────────────────────────────────────────────────────────────────────────────────
Node ID        Address                  Role       Status       Last HB
─────────────────────────────────────────────────────────────────────────────────────────────────
node-0         localhost:8080           leader     healthy      1s ago
node-1         localhost:8081           follower   healthy      500ms ago
node-2         localhost:8082           follower   healthy      800ms ago
─────────────────────────────────────────────────────────────────────────────────────────────────
Total Members: 3
════════════════════════════════════════════════════════════════════════════════════════
```

```bash
# Show cluster with replication metrics
$ unrdf daemon cluster --include-metrics

# ... (status section as above) ...

📊 Replication Metrics
────────────────────────────────────────────────────────────────────────────────────────────
node-0:
  Commit Index: 1523
  Log Index: 1525
  Match Index: 1523
node-1:
  Commit Index: 1523
  Log Index: 1525
  Match Index: 1523
node-2:
  Commit Index: 1523
  Log Index: 1525
  Match Index: 1523
════════════════════════════════════════════════════════════════════════════════════════
```

```bash
# Show cluster as JSON
$ unrdf daemon cluster --json
{
  "cluster": {
    "clusterId": "default-cluster",
    "term": 5,
    "leader": "node-0",
    "members": [
      {
        "nodeId": "node-0",
        "address": "localhost:8080",
        "role": "leader",
        "status": "healthy",
        "lastHeartbeat": "2026-01-11T12:30:00.000Z"
      },
      ...
    ],
    "quorumSize": 2,
    "isHealthy": true,
    "timestamp": "2026-01-11T12:30:00.000Z"
  }
}
```

## JSON Output Schema

All commands support `--json` flag for structured output. The following schemas define the output format for each command:

### List Command
```json
{
  "operations": [
    {
      "id": "string",
      "name": "string",
      "status": "string",
      "createdAt": "ISO8601 timestamp",
      "metadata": {}
    }
  ]
}
```

### Run Command
```json
{
  "operationId": "string",
  "status": "success | failure",
  "duration": "number (milliseconds)",
  "result": {
    "processed": "number",
    "affected": "number"
  }
}
```

### Schedule Command
```json
{
  "operationId": "string",
  "triggerId": "string",
  "triggerType": "cron | interval | reactive | event | idle",
  "triggerDescription": "string",
  "status": "scheduled",
  "enabled": "boolean"
}
```

### Status Command
```json
{
  "health": {
    "nodeId": "string",
    "clusterId": "string",
    "isRunning": "boolean",
    "isLeader": "boolean",
    "uptime": "number (milliseconds)",
    "activeOperations": "number",
    "queuedOperations": "number",
    "completedOperations": "number",
    "timestamp": "ISO8601 timestamp"
  },
  "metrics": {
    "nodeId": "string",
    "totalOperations": "number",
    "successfulOperations": "number",
    "failedOperations": "number",
    "successRate": "number (0-100)",
    "averageDuration": "number (milliseconds)",
    "totalDuration": "number (milliseconds)",
    "timestamp": "ISO8601 timestamp"
  }
}
```

## Troubleshooting

### Operation not found
**Error:** `Operation not found: <operation-id>`

**Solution:** Use `unrdf daemon list` to see all available operations.

### Invalid payload
**Error:** `Invalid arguments: Invalid payload`

**Solution:** Ensure the payload is valid JSON: `'{"key": "value"}'`

### Timeout exceeded
**Error:** `Operation timed out after <timeout>ms`

**Solution:** Increase the timeout with `--timeout` option or check daemon health with `unrdf daemon status`.

### Cluster unhealthy
**Error:** Shows unhealthy members in cluster

**Solution:** Check member heartbeats with `unrdf daemon cluster --include-metrics`. Restart unhealthy nodes.

### No logs available
**Error:** `Total Entries: 0`

**Solution:** Run an operation first with `unrdf daemon run <operation>` to generate logs.

## Integration Examples

### Monitor daemon health continuously
```bash
while true; do
  unrdf daemon status --json | jq '.health.activeOperations'
  sleep 5
done
```

### Export logs to file
```bash
unrdf daemon logs --json > daemon-logs.json
```

### Check if leader
```bash
unrdf daemon cluster --json | jq '.cluster.leader' -r
```

### Auto-run operations based on success rate
```bash
rate=$(unrdf daemon status --json | jq '.metrics.successRate')
if (( $(echo "$rate < 90" | bc -l) )); then
  unrdf daemon run validate-integrity
fi
```

## Performance Considerations

- **Timeout:** Default is 30 seconds. Adjust for long-running operations.
- **Max Lines:** Default 100 for logs. Reduce for large logs or high-frequency operations.
- **Metrics:** Including metrics adds overhead. Only use when needed.
- **JSON Output:** Slightly faster than formatted table output for automated processing.

## Best Practices

1. Use `--json` flag for programmatic consumption
2. Check `daemon status` regularly for cluster health
3. Review logs with filters for specific operations
4. Schedule operations during low-traffic periods
5. Monitor success rate in metrics
6. Set appropriate timeouts based on operation complexity
7. Keep Raft cluster healthy with proper quorum

