# Configuration Drift Detection Hook

This example demonstrates a Knowledge Hook for detecting unauthorized changes to critical infrastructure configurations.

## Hook Definition

```json
{
  "id": "ex:InfrastructureDrift",
  "name": "Infrastructure Configuration Drift",
  "description": "Detects unauthorized changes to critical infrastructure configuration",
  "select": "SELECT ?config ?value ?environment ?approved WHERE { ?config ex:currentValue ?value ; ex:environment ?environment . OPTIONAL { ?config ex:approved ?approved } }",
  "predicates": [
    {
      "kind": "DELTA",
      "spec": {
        "change": "any",
        "key": ["config", "environment"],
        "threshold": 0.01
      }
    },
    {
      "kind": "ASK",
      "spec": {
        "query": "ASK WHERE { ?config ex:approved false }",
        "expected": false
      }
    }
  ],
  "combine": "AND",
  "baseline": {
    "store": "approved-configs.ttl",
    "key": "configHash"
  },
  "output": {
    "schema": {
      "type": "object",
      "properties": {
        "config": { "type": "string" },
        "environment": { "type": "string" },
        "change": {
          "type": "object",
          "properties": {
            "type": {
              "type": "string",
              "enum": ["added", "removed", "modified"]
            },
            "magnitude": { "type": "number" },
            "previousValue": { "type": ["string", "number", "null"] },
            "currentValue": { "type": ["string", "number", "null"] }
          }
        },
        "approved": { "type": "boolean" },
        "severity": {
          "type": "string",
          "enum": ["info", "warning", "critical"]
        },
        "recommendations": {
          "type": "array",
          "items": { "type": "string" }
        }
      },
      "required": ["config", "environment", "change", "approved", "severity"]
    },
    "format": "json",
    "destination": "webhook",
    "webhook": {
      "url": "https://api.example.com/security/incidents",
      "method": "POST",
      "headers": {
        "Authorization": "Bearer security-token",
        "Content-Type": "application/json"
      }
    }
  }
}
```

## Baseline Configuration

```turtle
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Approved configuration baseline
ex:maxConnections ex:currentValue 100 ;
  ex:environment "production" ;
  ex:approved true ;
  ex:critical true .

ex:timeout ex:currentValue 30 ;
  ex:environment "production" ;
  ex:approved true ;
  ex:critical false .

ex:logLevel ex:currentValue "INFO" ;
  ex:environment "production" ;
  ex:approved true ;
  ex:critical false .

ex:databaseUrl ex:currentValue "prod-db.example.com" ;
  ex:environment "production" ;
  ex:approved true ;
  ex:critical true .
```

## Current Configuration (Drifted)

```turtle
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Current configuration with drift
ex:maxConnections ex:currentValue 200 ;
  ex:environment "production" ;
  ex:approved false ;  # Changed without approval
  ex:critical true .

ex:timeout ex:currentValue 30 ;
  ex:environment "production" ;
  ex:approved true ;
  ex:critical false .  # Unchanged

ex:logLevel ex:currentValue "DEBUG" ;
  ex:environment "production" ;
  ex:approved false ;  # Changed to DEBUG without approval
  ex:critical false .

ex:databaseUrl ex:currentValue "test-db.example.com" ;
  ex:environment "production" ;
  ex:approved false ;  # Changed to test database!
  ex:critical true .
```

## CLI Usage

### Create the hook
```bash
unrdf hook create --template delta --output hooks/drift-detection.json
```

### Load baseline configuration
```bash
cat > baseline-config.ttl << 'EOF'
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:maxConnections ex:currentValue 100 ;
  ex:environment "production" ;
  ex:approved true .

ex:timeout ex:currentValue 30 ;
  ex:environment "production" ;
  ex:approved true .

ex:logLevel ex:currentValue "INFO" ;
  ex:environment "production" ;
  ex:approved true .

ex:databaseUrl ex:currentValue "prod-db.example.com" ;
  ex:environment "production" ;
  ex:approved true .
EOF
```

### Load current configuration
```bash
cat > current-config.ttl << 'EOF'
@prefix ex: <http://example.org/> .

ex:maxConnections ex:currentValue 200 ;
  ex:environment "production" ;
  ex:approved false .

ex:timeout ex:currentValue 30 ;
  ex:environment "production" ;
  ex:approved true .

ex:logLevel ex:currentValue "DEBUG" ;
  ex:environment "production" ;
  ex:approved false .

ex:databaseUrl ex:currentValue "test-db.example.com" ;
  ex:environment "production" ;
  ex:approved false .
EOF
```

### Evaluate for drift
```bash
unrdf hook eval --hook hooks/drift-detection.json --data current-config.ttl --persist
```

## Expected Output

```json
{
  "id": "ex:InfrastructureDrift",
  "fired": true,
  "predicates": [
    {
      "kind": "DELTA",
      "ok": false,
      "meta": {
        "changes": [
          {
            "config": "ex:maxConnections",
            "environment": "production",
            "changeType": "modified",
            "magnitude": 1.0,
            "previousValue": 100,
            "currentValue": 200
          },
          {
            "config": "ex:logLevel",
            "environment": "production",
            "changeType": "modified",
            "magnitude": 0.5,
            "previousValue": "INFO",
            "currentValue": "DEBUG"
          },
          {
            "config": "ex:databaseUrl",
            "environment": "production",
            "changeType": "modified",
            "magnitude": 0.8,
            "previousValue": "prod-db.example.com",
            "currentValue": "test-db.example.com"
          }
        ]
      },
      "duration": 35
    },
    {
      "kind": "ASK",
      "ok": false,
      "meta": {
        "matched": 3
      },
      "duration": 12
    }
  ],
  "durations": {
    "totalMs": 58,
    "queryMs": 8,
    "predicateMs": 47,
    "canonicalizationMs": 3
  },
  "provenance": {
    "hookHash": "sha256:abc123...",
    "queryHash": "sha256:def456...",
    "graphHash": "sha256:ghi789...",
    "baselineHash": "sha256:jkl012...",
    "receiptHash": "sha256:mno345..."
  },
  "at": "2024-01-01T10:00:00.000Z",
  "input": {
    "bindings": 4,
    "variables": ["config", "value", "environment", "approved"]
  }
}
```

## Drift Analysis

### Detected Changes

1. **maxConnections**: 100 → 200 (100% increase)
   - **Severity**: Critical (configuration drift on critical setting)
   - **Impact**: Could cause performance issues or security risks

2. **logLevel**: "INFO" → "DEBUG" (50% change)
   - **Severity**: Warning (unauthorized change detected)
   - **Impact**: Could affect debugging and monitoring

3. **databaseUrl**: "prod-db.example.com" → "test-db.example.com" (80% change)
   - **Severity**: Critical (environment contamination)
   - **Impact**: Production system pointing to test database

### Automated Response

```javascript
if (receipt.fired) {
  const changes = receipt.predicates.find(p => p.kind === 'DELTA')?.meta?.changes;

  for (const change of changes) {
    const incident = {
      config: change.config,
      environment: change.environment,
      change: change,
      timestamp: receipt.at,
      severity: determineSeverity(change),
      approved: false
    };

    // Create security incident
    await createSecurityIncident(incident);

    // Notify relevant teams
    if (change.changeType === 'modified' && change.magnitude > 0.5) {
      await notifyDevOpsTeam(incident);
    }

    if (change.critical) {
      await notifySecurityTeam(incident);
    }

    // Rollback critical changes
    if (change.critical && change.magnitude > 0.8) {
      await rollbackConfiguration(change.config, change.previousValue);
    }
  }
}

function determineSeverity(change) {
  if (change.magnitude > 0.8) return 'critical';
  if (change.magnitude > 0.3) return 'warning';
  return 'info';
}
```

## Benefits

### Proactive Security
- **Real-time Detection**: Identifies configuration drift immediately
- **Automated Response**: Triggers security responses without human intervention
- **Comprehensive Coverage**: Monitors all configuration parameters

### Change Management
- **Approval Tracking**: Ensures all changes are properly approved
- **Audit Trail**: Complete history of configuration changes
- **Rollback Capability**: Automated restoration of approved configurations

### Risk Mitigation
- **Environment Protection**: Prevents test/production contamination
- **Critical Asset Protection**: Focuses on high-impact configurations
- **Compliance Support**: Maintains configuration compliance

## Advanced Features

### Critical Configuration Protection
```json
{
  "id": "ex:CriticalConfigMonitor",
  "select": "SELECT ?config ?value WHERE { ?config ex:currentValue ?value ; ex:critical true }",
  "predicates": [
    {
      "kind": "DELTA",
      "spec": {
        "change": "any",
        "key": ["config"],
        "threshold": 0.0  // Any change to critical config triggers
      }
    }
  ]
}
```

### Environment-Specific Monitoring
```json
{
  "id": "ex:EnvironmentDrift",
  "select": "SELECT ?config ?value ?env WHERE { ?config ex:currentValue ?value ; ex:environment ?env }",
  "predicates": [
    {
      "kind": "DELTA",
      "spec": {
        "change": "any",
        "key": ["config", "env"],
        "baseline": {
          "store": "env-baselines.ttl",
          "key": "envConfigHash"
        }
      }
    }
  ]
}
```

### Integration with Change Management Systems
```json
{
  "output": {
    "destination": "custom",
    "handler": async (data) => {
      // Create change request in ITSM system
      const changeRequest = await itsm.createChangeRequest({
        title: `Configuration Drift Detected: ${data.config}`,
        description: `Unauthorized change detected in ${data.environment}`,
        priority: data.severity === 'critical' ? 'high' : 'medium',
        assignee: 'security-team'
      });

      // Update CMDB with drift information
      await cmdb.recordDrift(data);

      // Escalate if critical
      if (data.severity === 'critical') {
        await escalateToSecurity(data);
      }
    }
  }
}
```

## Production Deployment

### Continuous Monitoring
```bash
# Monitor for drift every 5 minutes
unrdf hook watch --hook ex:InfrastructureDrift --interval 300

# Monitor critical configurations every minute
unrdf hook watch --hook ex:CriticalConfigMonitor --interval 60
```

### Integration with SIEM
```bash
# Send drift events to security monitoring
unrdf hook eval --hook ex:InfrastructureDrift --output json | \
  curl -X POST -H "Content-Type: application/json" \
  -H "Authorization: Bearer siem-token" \
  -d @- https://siem.example.com/events
```

### Alerting and Notification
```bash
# Slack notifications for critical drift
unrdf hook eval --hook ex:InfrastructureDrift --output json | \
  jq -r 'if .severity == "critical" then . else empty end' | \
  curl -X POST -H "Content-Type: application/json" \
  -d @- https://hooks.slack.com/services/YOUR/SLACK/WEBHOOK
```

## Performance Optimization

### Efficient Querying
```sparql
# Use specific variables and filters
SELECT ?config ?value ?env WHERE {
  ?config ex:currentValue ?value ;
          ex:environment ?env ;
          ex:approved false .  # Only check unapproved configs
}
```

### Selective Monitoring
```json
{
  "id": "ex:SelectiveDriftMonitor",
  "select": "SELECT ?config ?value WHERE { ?config ex:currentValue ?value ; ex:monitor true }",
  "predicates": [
    {
      "kind": "DELTA",
      "spec": {
        "change": "any",
        "key": ["config"]
      }
    }
  ]
}
```

## Best Practices

### Configuration Management
1. **Regular Baseline Updates**: Keep baseline configurations current
2. **Change Approval Process**: Implement formal change approval workflow
3. **Rollback Procedures**: Maintain capability to restore previous configurations
4. **Documentation**: Document all configuration changes and their rationale

### Security
1. **Least Privilege**: Monitor for privilege escalations
2. **Environment Isolation**: Prevent cross-environment contamination
3. **Critical Asset Focus**: Prioritize monitoring of high-impact configurations
4. **Audit Requirements**: Ensure compliance with regulatory requirements

### Operations
1. **Alert Fatigue Prevention**: Implement intelligent alerting thresholds
2. **Automated Remediation**: Enable automatic rollback for critical changes
3. **Integration**: Connect with existing security and operations tools
4. **Monitoring**: Track drift detection effectiveness and response times

## Conclusion

Configuration drift detection is essential for maintaining infrastructure security and compliance. This Knowledge Hook provides:

- **Real-time Detection**: Immediate identification of unauthorized changes
- **Automated Response**: Programmatic security incident creation
- **Complete Auditability**: Cryptographically signed evaluation records
- **Risk-based Prioritization**: Focus on critical configurations first

By implementing this hook, organizations can significantly reduce the risk of configuration-related security incidents while maintaining operational efficiency and regulatory compliance.
