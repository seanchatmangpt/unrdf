# GDPR Compliance Monitoring Hook

This example demonstrates a Knowledge Hook for monitoring GDPR compliance in data processing operations.

## Hook Definition

```json
{
  "id": "ex:GDPRComplianceGate",
  "name": "GDPR Data Compliance Gate",
  "description": "Ensures all sensitive data processing complies with GDPR requirements",
  "select": "SELECT ?resource ?consentGiven ?dataRetention ?securityLevel WHERE { ?resource ex:sensitive true ; ex:consentGiven ?consentGiven . OPTIONAL { ?resource ex:dataRetention ?dataRetention ; ex:securityLevel ?securityLevel } }",
  "predicates": [
    {
      "kind": "SHACL",
      "spec": {
        "shapes": "ex:GDPRShape",
        "mode": "violations",
        "strict": true
      }
    },
    {
      "kind": "ASK",
      "spec": {
        "query": "ASK WHERE { ?resource ex:consentGiven false }",
        "expected": false
      }
    },
    {
      "kind": "THRESHOLD",
      "spec": {
        "var": "dataRetention",
        "op": ">",
        "value": 365
      }
    }
  ],
  "combine": "AND",
  "output": {
    "schema": {
      "type": "object",
      "properties": {
        "resource": { "type": "string" },
        "violations": {
          "type": "array",
          "items": { "type": "string" }
        },
        "severity": { "type": "string", "enum": ["warning", "critical"] },
        "recommendations": {
          "type": "array",
          "items": { "type": "string" }
        }
      },
      "required": ["resource", "violations", "severity"]
    },
    "format": "json",
    "destination": "console"
  }
}
```

## SHACL Shapes

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# GDPR Compliance Shape
ex:GDPRShape a sh:NodeShape ;
  sh:targetClass ex:SensitiveData ;

  # Consent must be explicitly given
  sh:property [
    sh:path ex:consentGiven ;
    sh:minCount 1 ;
    sh:datatype xsd:boolean ;
    sh:minInclusive true ;
  ] ;

  # Data retention must be specified
  sh:property [
    sh:path ex:dataRetention ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
    sh:maxInclusive 365 ;
  ] ;

  # Security level must be high for sensitive data
  sh:property [
    sh:path ex:securityLevel ;
    sh:minCount 1 ;
    sh:in ( "high" "critical" ) ;
  ] ;

  # Processing purpose must be specified
  sh:property [
    sh:path ex:processingPurpose ;
    sh:minCount 1 ;
    sh:minLength 10 ;
  ] .
```

## Sample Data

### Compliant Data
```turtle
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Compliant sensitive data
ex:customer1 a ex:SensitiveData ;
  ex:consentGiven true ;
  ex:dataRetention 180 ;
  ex:securityLevel "high" ;
  ex:processingPurpose "Customer service and support" .

ex:customer2 a ex:SensitiveData ;
  ex:consentGiven true ;
  ex:dataRetention 90 ;
  ex:securityLevel "critical" ;
  ex:processingPurpose "Financial transaction processing" .
```

### Non-Compliant Data
```turtle
@prefix ex: <http://example.org/> .

# Missing consent
ex:customer3 a ex:SensitiveData ;
  ex:dataRetention 180 ;
  ex:securityLevel "high" ;
  ex:processingPurpose "Marketing" .

# Excessive data retention
ex:customer4 a ex:SensitiveData ;
  ex:consentGiven true ;
  ex:dataRetention 730 ;
  ex:securityLevel "high" ;
  ex:processingPurpose "Analytics" .

# Low security level
ex:customer5 a ex:SensitiveData ;
  ex:consentGiven true ;
  ex:dataRetention 180 ;
  ex:securityLevel "low" ;
  ex:processingPurpose "Newsletter" .
```

## CLI Usage

### Create the hook
```bash
unrdf hook create --template shacl --output hooks/gdpr-compliance.json
```

### Load SHACL shapes
```bash
cat > shapes.ttl << 'EOF'
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:GDPRShape a sh:NodeShape ;
  sh:targetClass ex:SensitiveData ;
  sh:property [
    sh:path ex:consentGiven ;
    sh:minCount 1 ;
    sh:datatype xsd:boolean ;
    sh:minInclusive true ;
  ] ;
  sh:property [
    sh:path ex:dataRetention ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
    sh:maxInclusive 365 ;
  ] ;
  sh:property [
    sh:path ex:securityLevel ;
    sh:minCount 1 ;
    sh:in ( "high" "critical" ) ;
  ] .
EOF
```

### Load test data
```bash
cat > test-data.ttl << 'EOF'
@prefix ex: <http://example.org/> .

# Compliant data
ex:customer1 a ex:SensitiveData ;
  ex:consentGiven true ;
  ex:dataRetention 180 ;
  ex:securityLevel "high" .

# Non-compliant: missing consent
ex:customer2 a ex:SensitiveData ;
  ex:dataRetention 180 ;
  ex:securityLevel "high" .

# Non-compliant: excessive retention
ex:customer3 a ex:SensitiveData ;
  ex:consentGiven true ;
  ex:dataRetention 730 ;
  ex:securityLevel "high" .

# Non-compliant: low security
ex:customer4 a ex:SensitiveData ;
  ex:consentGiven true ;
  ex:dataRetention 180 ;
  ex:securityLevel "low" .
EOF
```

### Evaluate compliance
```bash
unrdf hook eval --hook hooks/gdpr-compliance.json --data shapes.ttl --data test-data.ttl
```

## Expected Output

```json
{
  "id": "ex:GDPRComplianceGate",
  "fired": true,
  "predicates": [
    {
      "kind": "SHACL",
      "ok": false,
      "meta": {
        "violations": [
          {
            "focusNode": "ex:customer2",
            "property": "ex:consentGiven",
            "message": "Property ex:consentGiven is required but missing"
          },
          {
            "focusNode": "ex:customer3",
            "property": "ex:dataRetention",
            "message": "Value 730 exceeds maximum allowed retention period of 365 days"
          },
          {
            "focusNode": "ex:customer4",
            "property": "ex:securityLevel",
            "message": "Value 'low' is not in allowed values: high, critical"
          }
        ]
      },
      "duration": 25
    },
    {
      "kind": "ASK",
      "ok": false,
      "meta": {
        "matched": 1
      },
      "duration": 8
    },
    {
      "kind": "THRESHOLD",
      "ok": false,
      "meta": {
        "matched": 1,
        "threshold": 365,
        "maxValue": 730
      },
      "duration": 12
    }
  ],
  "durations": {
    "totalMs": 52,
    "queryMs": 7,
    "predicateMs": 45,
    "canonicalizationMs": 0
  },
  "provenance": {
    "hookHash": "sha256:abc123...",
    "queryHash": "sha256:def456...",
    "graphHash": "sha256:ghi789...",
    "receiptHash": "sha256:jkl012..."
  },
  "at": "2024-01-01T10:00:00.000Z",
  "input": {
    "bindings": 4,
    "variables": ["resource", "consentGiven", "dataRetention", "securityLevel"]
  }
}
```

## Explanation

This hook will fire when ALL of the following conditions are met:

1. **SHACL Validation Fails**: Any sensitive data violates GDPR requirements
2. **Missing Consent**: Any resource lacks proper consent (consentGiven = false)
3. **Excessive Retention**: Any data is retained longer than 365 days

The hook fires because customer2, customer3, and customer4 all have compliance issues.

## Compliance Violations Detected

1. **customer2**: Missing consent (required for GDPR compliance)
2. **customer3**: Excessive data retention (730 days > 365 days allowed)
3. **customer4**: Insufficient security level (low instead of high/critical)

## Automated Response

In a production environment, this hook could trigger:

```javascript
// Automated compliance response
if (receipt.fired) {
  const violations = receipt.predicates.find(p => p.kind === 'SHACL')?.meta?.violations;

  for (const violation of violations) {
    switch (violation.property) {
      case 'ex:consentGiven':
        // Trigger consent collection process
        await initiateConsentProcess(violation.focusNode);
        break;
      case 'ex:dataRetention':
        // Schedule data deletion
        await scheduleDataDeletion(violation.focusNode, 30); // 30 days notice
        break;
      case 'ex:securityLevel':
        // Escalate security review
        await escalateSecurityReview(violation.focusNode);
        break;
    }
  }

  // Log compliance incident
  await logComplianceIncident(receipt);

  // Notify compliance officer
  await notifyComplianceOfficer(receipt);
}
```

## Benefits

### Proactive Compliance
- **Real-time Monitoring**: Detects compliance issues immediately
- **Automated Enforcement**: Triggers corrective actions automatically
- **Comprehensive Coverage**: Monitors all aspects of GDPR requirements

### Audit Trail
- **Cryptographic Proof**: All evaluations are cryptographically signed
- **Complete History**: Full audit trail of compliance checks
- **Regulatory Reporting**: Automated compliance reporting

### Risk Reduction
- **Early Detection**: Identifies issues before they become problems
- **Consistent Enforcement**: Applies rules uniformly across all data
- **Reduced Human Error**: Eliminates manual compliance checking

## Advanced Features

### Delta Monitoring for Changes
```json
{
  "id": "ex:GDPRDriftDetector",
  "select": "SELECT ?resource ?consentGiven ?retention WHERE { ?resource ex:sensitive true ; ex:consentGiven ?consentGiven ; ex:dataRetention ?retention }",
  "predicates": [
    {
      "kind": "DELTA",
      "spec": {
        "change": "any",
        "key": ["resource"],
        "baseline": {
          "store": "baseline-compliance.ttl",
          "key": "resourceHash"
        }
      }
    }
  ]
}
```

### Integration with Privacy Tools
```json
{
  "output": {
    "destination": "custom",
    "handler": async (data) => {
      // Integrate with privacy management tools
      await privacyTool.logViolation(data);

      // Trigger automated remediation
      await remediationSystem.fixViolation(data);

      // Update compliance dashboard
      await dashboard.updateStatus(data);
    }
  }
}
```

## Production Considerations

### Performance
- **Selective Validation**: Use SHACL target declarations for efficiency
- **Incremental Checking**: Validate only changed data when possible
- **Caching**: Cache validation results for frequently accessed resources

### Scalability
- **Parallel Validation**: Process multiple resources concurrently
- **Batch Processing**: Group related validations for efficiency
- **Resource Limits**: Set appropriate limits for validation scope

### Monitoring
- **Alert Fatigue Prevention**: Implement intelligent alerting thresholds
- **Compliance Metrics**: Track compliance rates over time
- **Trend Analysis**: Monitor compliance trends and patterns

## Conclusion

This GDPR compliance hook demonstrates how Knowledge Hooks can provide enterprise-grade compliance monitoring with:

- **Real-time Detection**: Immediate identification of compliance issues
- **Automated Response**: Programmatic remediation of violations
- **Complete Auditability**: Cryptographically signed evaluation records
- **Regulatory Compliance**: Alignment with GDPR requirements

This approach transforms compliance from a reactive burden into a proactive advantage, ensuring data protection while maintaining operational efficiency.
