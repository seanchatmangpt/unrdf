# Event-Driven Processing with Hooks

Knowd's hook system provides powerful event-driven processing capabilities, allowing you to react to data changes, validate transactions, and trigger automated workflows based on your knowledge graph data.

## Overview

Hooks are event-driven functions that execute in response to:
- **Transaction events** - When data is added, modified, or removed
- **Query events** - When specific queries are executed
- **Time-based triggers** - Scheduled execution
- **Validation events** - Before/after SHACL validation

## Hook Types

### 1. SPARQL-ASK Hooks

Execute SPARQL ASK queries to check conditions:

```turtle
ex:RiskAlertHook a ex:Hook ;
  ex:type "sparql-ask" ;
  ex:query "ASK WHERE { ?person ex:riskLevel \"high\" . ?person ex:notified false }" ;
  ex:trigger "after-transaction" ;
  ex:action "send-alert" .
```

**Trigger conditions:**
```sparql
# Alert if high-risk person added
ASK WHERE {
  ?person ex:riskLevel "high" .
  ?person ex:notified false .
}
```

### 2. SPARQL-SELECT Hooks

Execute SPARQL SELECT queries for data processing:

```turtle
ex:NotificationHook a ex:Hook ;
  ex:type "sparql-select" ;
  ex:query "SELECT ?person ?email WHERE { ?person ex:riskLevel \"high\" . ?person ex:email ?email }" ;
  ex:trigger "after-transaction" ;
  ex:action "send-email" .
```

### 3. Threshold Hooks

Trigger based on numeric thresholds:

```turtle
ex:CapacityAlertHook a ex:Hook ;
  ex:type "threshold" ;
  ex:query "SELECT (COUNT(?person) AS ?count) WHERE { ?person a ex:Person }" ;
  ex:threshold 1000 ;
  ex:operator ">" ;
  ex:action "scale-cluster" .
```

### 4. Count Hooks

Trigger based on count conditions:

```turtle
ex:DailyReportHook a ex:Hook ;
  ex:type "count" ;
  ex:query "SELECT (COUNT(?event) AS ?count) WHERE { ?event a ex:ErrorEvent . ?event ex:timestamp ?ts . FILTER(?ts >= NOW() - \"P1D\"^^xsd:duration) }" ;
  ex:count 10 ;
  ex:action "send-daily-report" .
```

### 5. Window Hooks

Trigger based on time windows:

```turtle
ex:BatchProcessHook a ex:Hook ;
  ex:type "window" ;
  ex:query "SELECT ?item WHERE { ?item ex:status \"pending\" }" ;
  ex:window "300" ;  # 5 minutes
  ex:action "process-batch" .
```

## Hook Configuration

### Basic Hook Definition

```turtle
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:MyHook a ex:Hook ;
  ex:id "unique-hook-id" ;
  ex:name "My Custom Hook" ;
  ex:type "sparql-ask" ;
  ex:query "ASK WHERE { ?person ex:requiresAttention true }" ;
  ex:trigger "after-transaction" ;
  ex:enabled true ;
  ex:schedule "0 */6 * * *" ;  # Every 6 hours
  ex:priority 10 ;  # Higher priority executes first
  ex:config [
    ex:action "send-notification" ;
    ex:recipients ("admin@example.com" "team@example.com") ;
  ] .
```

### Trigger Types

**Transaction Triggers:**
- `before-transaction` - Before data changes are applied
- `after-transaction` - After data changes are committed
- `on-add` - When triples are added
- `on-remove` - When triples are removed

**Query Triggers:**
- `before-query` - Before query execution
- `after-query` - After query execution
- `on-slow-query` - When query execution exceeds threshold

**Time Triggers:**
- `scheduled` - Cron-based scheduling
- `interval` - Fixed time intervals

### Hook Actions

**Built-in Actions:**
- `send-email` - Send notification emails
- `send-webhook` - Call external webhook
- `create-ticket` - Create support tickets
- `scale-cluster` - Trigger cluster scaling
- `backup-data` - Trigger data backup

**Custom Actions:**
```turtle
ex:CustomAction a ex:HookAction ;
  ex:name "process-payment" ;
  ex:endpoint "https://api.payment.com/process" ;
  ex:method "POST" ;
  ex:headers (
    "Authorization: Bearer ${API_TOKEN}"
    "Content-Type: application/json"
  ) .
```

## Hook Examples

### 1. Data Quality Monitoring

```turtle
# Alert on data quality issues
ex:QualityAlertHook a ex:Hook ;
  ex:type "sparql-ask" ;
  ex:query """
    ASK WHERE {
      ?person a ex:Person .
      FILTER NOT EXISTS { ?person ex:name ?name }
    }
  """ ;
  ex:trigger "after-transaction" ;
  ex:action "send-alert" ;
  ex:config [
    ex:recipients ("data-quality@example.com") ;
    ex:subject "Data Quality Alert: Missing Names" ;
  ] .
```

### 2. Security Monitoring

```turtle
# Alert on suspicious activities
ex:SecurityAlertHook a ex:Hook ;
  ex:type "threshold" ;
  ex:query "SELECT (COUNT(?login) AS ?count) WHERE { ?login a ex:FailedLogin . ?login ex:timestamp ?ts . FILTER(?ts >= NOW() - \"PT5M\"^^xsd:duration) }" ;
  ex:threshold 5 ;
  ex:operator ">" ;
  ex:action "security-alert" ;
  ex:config [
    ex:recipients ("security@example.com") ;
    ex:urgency "high" ;
  ] .
```

### 3. Business Process Automation

```turtle
# Auto-approve low-risk transactions
ex:AutoApprovalHook a ex:Hook ;
  ex:type "sparql-ask" ;
  ex:query """
    ASK WHERE {
      ?tx ex:amount ?amount .
      ?tx ex:riskScore ?score .
      FILTER(?amount < 1000 && ?score < 0.3)
    }
  """ ;
  ex:trigger "before-transaction" ;
  ex:action "auto-approve" .
```

### 4. Scheduled Maintenance

```turtle
# Daily cleanup of old data
ex:CleanupHook a ex:Hook ;
  ex:type "sparql-select" ;
  ex:query "SELECT ?item WHERE { ?item ex:created ?date . FILTER(?date < NOW() - \"P30D\"^^xsd:duration) }" ;
  ex:trigger "scheduled" ;
  ex:schedule "0 2 * * *" ;  # Daily at 2 AM
  ex:action "cleanup-old-data" .
```

## Hook Management API

### Register Hook

```bash
curl -X POST http://localhost:8090/v1/hooks/evaluate \
  -H "Content-Type: application/json" \
  -d '{
    "hook": {
      "id": "alert-hook",
      "type": "sparql-ask",
      "query": "ASK WHERE { ?person ex:riskLevel \"high\" }",
      "enabled": true
    },
    "persist": true
  }'
```

### Evaluate Hook

```bash
curl -X POST http://localhost:8090/v1/hooks/evaluate \
  -H "Content-Type: application/json" \
  -d '{
    "hook": {
      "id": "alert-hook"
    },
    "persist": false
  }'
```

**Response:**
```json
{
  "fired": true,
  "result": {
    "hookId": "alert-hook",
    "data": {
      "highRiskPersons": 3,
      "alertTriggered": true
    }
  }
}
```

## Hook Development

### Custom Hook Types

**Define custom hook logic:**
```turtle
ex:CustomValidationHook a ex:Hook ;
  ex:type "custom-validator" ;
  ex:script """
    function validate(data) {
      if (data.age < 18) {
        return { valid: false, message: "Must be 18 or older" };
      }
      return { valid: true };
    }
  """ ;
  ex:trigger "before-transaction" .
```

### Hook Chaining

**Execute hooks in sequence:**
```turtle
ex:ChainHook a ex:Hook ;
  ex:type "chain" ;
  ex:hooks ("validate-hook" "process-hook" "notify-hook") ;
  ex:trigger "after-transaction" .
```

### Conditional Execution

**Execute hooks based on conditions:**
```turtle
ex:ConditionalHook a ex:Hook ;
  ex:type "conditional" ;
  ex:condition "data-changed" ;
  ex:hook "backup-hook" ;
  ex:trigger "after-transaction" .
```

## Best Practices

### 1. Hook Performance

**Efficient queries:**
```sparql
# Good - specific patterns
ASK WHERE {
  ?person ex:riskLevel "high" .
  ?person ex:notified false .
}

# Avoid - broad patterns
ASK WHERE {
  ?s ?p ?o .
  FILTER(?p = ex:riskLevel && ?o = "high")
}
```

**Batch processing:**
```turtle
# Process in batches for better performance
ex:BatchHook a ex:Hook ;
  ex:type "batch" ;
  ex:batchSize 100 ;
  ex:query "SELECT ?item WHERE { ?item ex:status \"pending\" }" ;
  ex:action "process-batch" .
```

### 2. Error Handling

**Graceful failure:**
```turtle
ex:RobustHook a ex:Hook ;
  ex:type "sparql-select" ;
  ex:query "SELECT ?person WHERE { ?person ex:email ?email }" ;
  ex:onError "skip" ;  # Skip on error instead of failing
  ex:action "send-notification" .
```

**Retry logic:**
```turtle
ex:ReliableHook a ex:Hook ;
  ex:type "external-call" ;
  ex:endpoint "https://api.external.com/webhook" ;
  ex:retryCount 3 ;
  ex:retryDelay "PT5S" ;
  ex:action "external-notification" .
```

### 3. Security Considerations

**Secure hook execution:**
```turtle
# Limit hook permissions
ex:SecureHook a ex:Hook ;
  ex:type "external-call" ;
  ex:allowedEndpoints ("https://trusted-api.com/*") ;
  ex:timeout "PT30S" ;
  ex:action "secure-external-call" .
```

**Audit logging:**
```turtle
# Log all hook executions
ex:AuditedHook a ex:Hook ;
  ex:type "any" ;
  ex:audit true ;
  ex:auditLevel "detailed" ;
  ex:action "any-action" .
```

## Hook Integration

### With Transactions

**Pre-transaction validation:**
```turtle
ex:PreTxHook a ex:Hook ;
  ex:type "shacl-validate" ;
  ex:shapes ex:PersonShape ;
  ex:trigger "before-transaction" ;
  ex:onFailure "reject" .
```

**Post-transaction processing:**
```turtle
ex:PostTxHook a ex:Hook ;
  ex:type "sparql-select" ;
  ex:query "SELECT ?person WHERE { ?person ex:requiresNotification true }" ;
  ex:trigger "after-transaction" ;
  ex:action "send-notifications" .
```

### With Queries

**Query optimization:**
```turtle
ex:QueryHook a ex:Hook ;
  ex:type "query-analyzer" ;
  ex:queryPattern "SELECT * WHERE { ?s ?p ?o }" ;
  ex:suggestion "Use specific patterns: ?s ex:name ?name" ;
  ex:trigger "before-query" .
```

### With SHACL Validation

**Validation-triggered hooks:**
```turtle
ex:ValidationHook a ex:Hook ;
  ex:type "validation-result" ;
  ex:condition "validation-failed" ;
  ex:action "create-support-ticket" ;
  ex:trigger "after-validation" .
```

## Monitoring and Debugging

### Hook Metrics

**View hook performance:**
```bash
curl http://localhost:8090/v1/store/stats | jq '.hooks'
```

**Response:**
```json
{
  "totalHooks": 15,
  "activeHooks": 12,
  "executionsPerMinute": 45,
  "averageExecutionTimeMs": 23,
  "failedExecutions": 2
}
```

### Hook Debugging

**Enable hook logging:**
```bash
KNOWD_LOG_LEVEL=debug KNOWD_HOOKS_DEBUG=true ./knowd
```

**Debug hook execution:**
```bash
curl -X POST http://localhost:8090/v1/hooks/evaluate \
  -H "X-Debug-Hooks: true" \
  -d '{"hook": {"id": "test-hook"}}'
```

## Troubleshooting

### Common Hook Issues

**"Hook not firing":**
- Check trigger conditions
- Verify hook is enabled
- Ensure query syntax is correct

**"Performance issues":**
- Use efficient SPARQL patterns
- Implement batch processing
- Monitor execution times

**"Memory leaks":**
- Check for infinite loops in custom logic
- Monitor hook resource usage
- Implement proper cleanup

### Hook Logs

**View hook execution logs:**
```bash
# In debug mode
KNOWD_LOG_LEVEL=debug ./knowd 2>&1 | grep -E "(HOOK|hook)"

# Check hook-specific logs
tail -f /var/log/knowd/hooks.log
```

## Advanced Patterns

### Hook Composition

**Combine multiple hooks:**
```turtle
ex:CompositeHook a ex:Hook ;
  ex:type "composite" ;
  ex:hooks (
    "validate-hook"
    "process-hook"
    "notify-hook"
  ) ;
  ex:executionMode "sequential" ;  # or "parallel"
  ex:onFailure "stop" .
```

### Dynamic Hook Configuration

**Runtime hook modification:**
```turtle
# Enable/disable hooks dynamically
ex:DynamicHook a ex:Hook ;
  ex:type "dynamic" ;
  ex:baseHook "static-hook" ;
  ex:condition "business-hours" ;
  ex:enabled true .
```

### Hook State Management

**Maintain state across executions:**
```turtle
ex:StatefulHook a ex:Hook ;
  ex:type "stateful" ;
  ex:stateVariable "lastProcessedTimestamp" ;
  ex:query "SELECT ?event WHERE { ?event ex:timestamp ?ts . FILTER(?ts > $lastProcessedTimestamp) }" ;
  ex:action "process-events" .
```

This comprehensive hook system enables powerful automation, monitoring, and data processing capabilities for your knowledge graph applications.
