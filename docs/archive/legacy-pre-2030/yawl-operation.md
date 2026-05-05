# YAWL Operation Runbook

**Operational procedures for daemon+YAWL workflow deployment and monitoring**

---

## Checklist: Deploy New Workflow

### Pre-Deployment

- [ ] **Workflow definition validated** (Run: `WorkflowSpecSchema.parse(workflowDef)`)
- [ ] **All task handlers implemented** (No placeholder functions)
- [ ] **Control flow validated** (No unreachable tasks, all joins satisfied)
- [ ] **Timeout thresholds set** (Based on P95 latency + 50% buffer)
- [ ] **Retry policy configured** (Max attempts, backoff, jitter)
- [ ] **Test workflow in staging** (Verify happy path + error paths)
- [ ] **Monitoring dashboards created** (Grafana/Datadog panels)
- [ ] **Alerts configured** (Timeout, retry exhaustion, case failure)
- [ ] **Runbook documented** (This document updated)

### Deployment Steps

#### 1. Deploy Workflow Definition

```bash
# SSH to deployment node
ssh production-workflow-01

# Navigate to deployment directory
cd /opt/unrdf/workflows

# Create workflow definition file
cat > expense-approval.mjs << 'WORKFLOW_EOF'
import { createWorkflow } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

export async function deployWorkflow(store) {
  return await createWorkflow(store, {
    id: 'expense-approval',
    name: 'Expense Approval Workflow',
    tasks: [...],
    flow: [...],
  });
}
WORKFLOW_EOF

# Validate syntax
node --check expense-approval.mjs

# Deploy to store
node deploy-workflow.mjs --workflow expense-approval
```

**Expected output:**
```
✓ Workflow validated
✓ Workflow created: expense-approval
✓ Receipt generated: receipt-abc123def456
✓ Hash: 8f3e7a2b1c9d...
```

#### 2. Configure Bridge

```bash
# Update bridge configuration
cat > /etc/unrdf/bridge-config.json << 'CONFIG_EOF'
{
  "daemonNodeId": "node-production-01",
  "maxConcurrentCases": 100,
  "enableAutoRetry": true,
  "enableTimeoutTracking": true,
  "retryPolicy": {
    "maxAttempts": 3,
    "backoffMs": 1000,
    "backoffMultiplier": 2,
    "maxBackoffMs": 30000,
    "jitterFactor": 0.1
  },
  "timeoutDefaults": {
    "taskTimeoutMs": 30000,
    "caseTimeoutMs": 3600000,
    "checkIntervalMs": 5000
  }
}
CONFIG_EOF

# Restart bridge with new config
systemctl restart unrdf-bridge
```

#### 3. Schedule Workflow Execution

```bash
# For recurring workflows, schedule via bridge
node schedule-workflow.mjs \
  --workflow-id expense-approval \
  --schedule "0 * * * *" \
  --case-prefix "expense" \
  --input-data '{"environment": "production"}'
```

**Expected output:**
```
✓ Schedule created: operation-yawl-case-expense-approval-1234567890
✓ Next execution: 2026-01-10T14:00:00Z
```

#### 4. Verify Deployment

```bash
# Check daemon operations
curl http://localhost:8080/health
# Expected: {"isRunning": true, "queuedOperations": 1}

# Check bridge stats
curl http://localhost:8080/bridge/stats
# Expected: {"caseSchedules": 1, "activeTimeouts": 0}

# Tail logs
journalctl -u unrdf-daemon -f
journalctl -u unrdf-bridge -f
```

### Post-Deployment

- [ ] **Monitor first execution** (Watch logs for errors)
- [ ] **Verify case creation** (Check case ID generated)
- [ ] **Verify receipt generation** (Check receipt hash chain)
- [ ] **Check metrics** (Success rate, duration, error rate)
- [ ] **Update documentation** (Note deployment date, version)
- [ ] **Notify team** (Slack/email with deployment summary)

---

## Checklist: Monitor Workflow Execution

### Real-Time Monitoring

#### 1. Check Daemon Health

```bash
# Health endpoint
curl http://localhost:8080/health | jq
```

**Expected response:**
```json
{
  "nodeId": "node-production-01",
  "clusterId": "cluster-prod",
  "isRunning": true,
  "uptime": 1234567,
  "activeOperations": 2,
  "queuedOperations": 5,
  "completedOperations": 42
}
```

**Thresholds:**
- `isRunning`: MUST be `true`
- `activeOperations`: Should be < `concurrency` limit
- `queuedOperations`: Should be < 100 (indicates backlog)

#### 2. Check Bridge Stats

```bash
# Bridge stats endpoint
curl http://localhost:8080/bridge/stats | jq
```

**Expected response:**
```json
{
  "bridgeId": "yawl-bridge-1234567890",
  "isRunning": true,
  "caseSchedules": 5,
  "activeTimeouts": 3,
  "activeRetries": 1,
  "activeTriggers": 0,
  "distributions": 2
}
```

**Thresholds:**
- `isRunning`: MUST be `true`
- `activeRetries`: Should be < 10 (indicates frequent failures)
- `activeTimeouts`: Should be < `maxConcurrentCases * 0.1`

#### 3. Check Metrics

```bash
# Metrics endpoint
curl http://localhost:8080/metrics | jq
```

**Expected response:**
```json
{
  "totalOperations": 100,
  "successfulOperations": 95,
  "failedOperations": 5,
  "successRate": 95.0,
  "averageDuration": 1234.5,
  "totalDuration": 123450
}
```

**Thresholds (SLOs):**
- `successRate`: >= 99% (< 1% error rate)
- `averageDuration`: <= P95 latency target
- `failedOperations`: Investigate if > 5% of total

### Long-Term Monitoring (Grafana Queries)

#### Dashboard: YAWL Workflow Health

**Panel 1: Case Creation Rate**
```promql
rate(yawl_cases_created_total{workflow_id="expense-approval"}[5m])
```
**Expected:** Matches schedule (e.g., 1/hour for hourly cron)

**Panel 2: Task Success Rate**
```promql
sum(rate(yawl_tasks_completed_total{status="success"}[5m])) 
/ 
sum(rate(yawl_tasks_completed_total[5m])) * 100
```
**Threshold:** >= 99%

**Panel 3: Average Task Duration**
```promql
avg(yawl_task_duration_seconds{workflow_id="expense-approval"})
```
**Threshold:** <= P95 target (e.g., 5s for API calls)

**Panel 4: Retry Rate**
```promql
rate(yawl_tasks_retried_total[5m])
```
**Threshold:** <= 0.05 (5% of tasks)

**Panel 5: Timeout Rate**
```promql
rate(yawl_tasks_timed_out_total[5m])
```
**Threshold:** <= 0.01 (1% of tasks)

---

## Checklist: Diagnose Workflow Failures

### Symptom: No cases created

**Diagnosis steps:**
```bash
# 1. Check daemon status
systemctl status unrdf-daemon
# Should be: active (running)

# 2. Check bridge status
systemctl status unrdf-bridge
# Should be: active (running)

# 3. Check scheduled operations
curl http://localhost:8080/operations | jq '.[] | select(.name | contains("case"))'
# Should return case creation operation

# 4. Check logs for errors
journalctl -u unrdf-daemon --since "1 hour ago" | grep -i error
journalctl -u unrdf-bridge --since "1 hour ago" | grep -i error
```

**Common causes:**
- Daemon not started: `systemctl start unrdf-daemon`
- Bridge not started: `systemctl start unrdf-bridge`
- Invalid cron syntax: Check cron expression at crontab.guru
- Workflow not found: Verify workflow ID exists in store

**Resolution:**
```bash
# Restart services
systemctl restart unrdf-daemon unrdf-bridge

# Reschedule case creation
node schedule-workflow.mjs --workflow-id <workflow-id> --schedule "<cron>"
```

### Symptom: Tasks timing out

**Diagnosis steps:**
```bash
# 1. Check timeout configuration
cat /etc/unrdf/bridge-config.json | jq '.timeoutDefaults'

# 2. Check active timeouts
curl http://localhost:8080/bridge/stats | jq '.activeTimeouts'

# 3. Check timeout events
journalctl -u unrdf-bridge --since "1 hour ago" | grep "timeout-enforced"

# 4. Query task duration metrics
curl http://localhost:8080/metrics | jq '.averageDuration'
```

**Common causes:**
- Timeout too aggressive (< P95 latency)
- External dependency slow (API, DB)
- Resource contention (CPU, memory)

**Resolution:**
```bash
# Option 1: Increase timeout threshold
# Edit /etc/unrdf/bridge-config.json
# Change timeoutDefaults.taskTimeoutMs to higher value
# Restart: systemctl restart unrdf-bridge

# Option 2: Optimize task handler
# Profile slow tasks, optimize queries, add caching
```

### Symptom: Retry exhausted

**Diagnosis steps:**
```bash
# 1. Check retry configuration
cat /etc/unrdf/bridge-config.json | jq '.retryPolicy'

# 2. Check retry events
journalctl -u unrdf-bridge --since "1 hour ago" | grep "retry-exhausted"

# 3. Query failed tasks
curl "http://localhost:8080/yawl/cases/<case-id>/tasks" | jq '.[] | select(.status == "failed")'
```

**Common causes:**
- Permanent failure (not transient)
- Max attempts too low
- Backoff too aggressive (exceeds recovery time)

**Resolution:**
```bash
# For transient failures: Increase max attempts
# Edit /etc/unrdf/bridge-config.json
# Change retryPolicy.maxAttempts to higher value

# For permanent failures: Manual intervention required
# 1. Investigate error: journalctl -u unrdf-daemon | grep "<task-id>"
# 2. Fix root cause (e.g., invalid input data)
# 3. Recreate case or skip failed task
```

### Symptom: Memory leak / high memory usage

**Diagnosis steps:**
```bash
# 1. Check daemon memory usage
ps aux | grep unrdf-daemon

# 2. Check completed operations cache
curl http://localhost:8080/health | jq '.completedOperations'
# Should be <= 1000 (LRU cache limit)

# 3. Check metrics retention
cat /etc/unrdf/daemon-config.json | jq '.metricsRetentionMs'

# 4. Profile heap usage
node --inspect /opt/unrdf/bin/daemon.mjs &
chrome://inspect
# Take heap snapshot, analyze retained objects
```

**Common causes:**
- Unbounded metrics retention
- Large payload in operation results
- Event listener leak (not unsubscribed)

**Resolution:**
```bash
# Reduce metrics retention
# Edit /etc/unrdf/daemon-config.json
# Change metricsRetentionMs to lower value (e.g., 1800000 = 30 min)

# Restart daemon
systemctl restart unrdf-daemon

# For long-running systems: Export metrics to external system
```

---

## Checklist: Handle Timeouts/Retries

### Timeout Scenarios

#### Scenario 1: Expected timeout (slow task)

**Action:** Increase timeout threshold
```bash
# Edit bridge config
nano /etc/unrdf/bridge-config.json
# Change: "taskTimeoutMs": 60000  (increase to 60s)

# Restart bridge
systemctl restart unrdf-bridge
```

#### Scenario 2: Unexpected timeout (dependency failure)

**Action:** Investigate dependency
```bash
# Check external API health
curl https://api.external-service.com/health

# Check database connectivity
psql -h db.example.com -U unrdf -c "SELECT 1"

# Review error logs
journalctl -u unrdf-daemon --since "5 minutes ago" | grep -i timeout
```

**Resolution:**
- If dependency down: Wait for recovery (retry will handle)
- If dependency slow: Increase timeout or optimize query
- If network issue: Check firewall, DNS, routing

### Retry Scenarios

#### Scenario 1: Transient failure (recovered)

**Expected behavior:**
```
[INFO] Task failed: api-call-task (ECONNREFUSED)
[INFO] Retry scheduled: attempt 1/3
[INFO] Backoff: 1000ms
[INFO] Retry executed: attempt 1
[INFO] Task succeeded: api-call-task
```

**No action required** (automatic recovery).

#### Scenario 2: Retry exhausted (permanent failure)

**Expected behavior:**
```
[ERROR] Task failed: api-call-task (ECONNREFUSED)
[INFO] Retry scheduled: attempt 1/3
[INFO] Retry scheduled: attempt 2/3
[INFO] Retry scheduled: attempt 3/3
[ERROR] Retry exhausted: api-call-task after 3 attempts
```

**Manual intervention required:**
```bash
# 1. Investigate root cause
journalctl -u unrdf-daemon | grep "api-call-task" | tail -20

# 2. Fix issue (e.g., update API endpoint, fix credentials)

# 3. Recreate case or manually complete task
node retry-task.mjs --case-id <case-id> --task-id api-call-task
```

---

## Success Criteria & SLOs

### Operational SLOs

| Metric | Target | Measurement | Alert Threshold |
|--------|--------|-------------|-----------------|
| **Case creation success rate** | 99.9% | Cases created / Scheduled executions | < 99% |
| **Task success rate** | 99.0% | Successful tasks / Total tasks | < 95% |
| **Average case duration** | <= P95 + 20% | Case completion time | > P95 + 50% |
| **Timeout rate** | < 1% | Timed out tasks / Total tasks | >= 2% |
| **Retry success rate** | 90% | Successful retries / Total retries | < 80% |
| **Bridge uptime** | 99.9% | Uptime seconds / Total seconds | < 99% |
| **Daemon uptime** | 99.9% | Uptime seconds / Total seconds | < 99% |

### Alert Rules

#### Critical Alerts (Page on-call)

```yaml
# Case creation failure
- alert: YawlCaseCreationFailure
  expr: rate(yawl_cases_failed_total[5m]) > 0.01
  for: 5m
  annotations:
    summary: "YAWL case creation failing"
    description: "Case creation failure rate > 1% for 5 minutes"
```

```yaml
# Bridge down
- alert: YawlBridgeDown
  expr: up{job="unrdf-bridge"} == 0
  for: 2m
  annotations:
    summary: "YAWL bridge is down"
    description: "Bridge unavailable for 2 minutes"
```

#### Warning Alerts (Slack notification)

```yaml
# High retry rate
- alert: YawlHighRetryRate
  expr: rate(yawl_tasks_retried_total[5m]) > 0.05
  for: 10m
  annotations:
    summary: "High retry rate detected"
    description: "Retry rate > 5% for 10 minutes"
```

```yaml
# Slow task duration
- alert: YawlSlowTaskDuration
  expr: avg(yawl_task_duration_seconds) > 10
  for: 5m
  annotations:
    summary: "Task duration exceeding threshold"
    description: "Average duration > 10s for 5 minutes"
```

---

## Escalation

### Level 1: On-call engineer
- **Timeouts** (adjust thresholds)
- **Transient failures** (retry exhausted, but service recovered)
- **Performance degradation** (slow tasks, high memory)

### Level 2: Platform team lead
- **Bridge/daemon crashes** (service unavailable)
- **Cascading failures** (multiple workflows affected)
- **Data corruption** (receipt verification failures)

### Level 3: Architecture team
- **Design flaws** (workflow pattern issues)
- **Scalability limits** (concurrency exceeded)
- **Security incidents** (unauthorized access)

---

## Emergency Procedures

### Disable All Workflows

```bash
# Stop bridge (stops new case creation)
systemctl stop unrdf-bridge

# Unschedule all operations
curl -X POST http://localhost:8080/operations/unschedule-all

# Verify
curl http://localhost:8080/operations | jq 'length'
# Expected: 0
```

### Emergency Rollback

```bash
# Revert to previous workflow version
git revert <commit-hash>
git push origin main

# Redeploy workflow
node deploy-workflow.mjs --workflow <workflow-id>

# Restart services
systemctl restart unrdf-daemon unrdf-bridge
```

---

**Version**: 1.0.0  
**Last Updated**: 2026-01-10  
**Maintainer**: UNRDF Core Team  
**On-call**: Slack #unrdf-oncall | PagerDuty: unrdf-workflows
