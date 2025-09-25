# 🎯 Knowledge Hooks Examples

**Real-world examples demonstrating the power of Knowledge Hooks for enterprise applications**

## Overview

These examples showcase practical applications of Knowledge Hooks across different domains. Each example includes complete hook definitions, sample data, CLI usage instructions, and expected outputs.

## Examples

### 🏥 Service Health Monitoring
**[service-health.md](service-health.md)**

Demonstrates monitoring service health metrics including error rates and latency with intelligent alerting.

**Key Features:**
- Multi-threshold monitoring (error rates, latency)
- Critical service prioritization
- Real-time performance tracking
- Automated alerting via webhooks

```bash
unrdf hook eval --hook hooks/service-health.json --data services.ttl
```

### 🔒 GDPR Compliance Monitoring
**[compliance.md](compliance.md)**

Shows how to implement GDPR compliance monitoring using SHACL validation and consent tracking.

**Key Features:**
- SHACL-based validation rules
- Consent verification
- Data retention monitoring
- Automated compliance reporting

```bash
unrdf hook eval --hook hooks/gdpr-compliance.json --data shapes.ttl --data data.ttl
```

### 🔄 Configuration Drift Detection
**[drift-detection.md](drift-detection.md)**

Illustrates infrastructure configuration drift detection with change tracking and security incident creation.

**Key Features:**
- Baseline comparison with cryptographic hashing
- Environment-specific monitoring
- Critical configuration protection
- Automated security response

```bash
unrdf hook eval --hook hooks/drift-detection.json --data current-config.ttl --persist
```

## Getting Started

### 1. Choose an Example

Select an example that matches your use case:

- **Service Health**: For monitoring application performance metrics
- **Compliance**: For regulatory compliance and data governance
- **Drift Detection**: For infrastructure security and change management

### 2. Set Up the Hook

Each example provides a complete hook definition. You can:

- Copy the JSON definition to a file
- Use the provided CLI commands to create the hook
- Modify the hook for your specific requirements

### 3. Load Sample Data

Each example includes sample data to test the hook:

```bash
# Load the provided sample data
cat > test-data.ttl << 'EOF'
# ... sample data from example
EOF
```

### 4. Evaluate the Hook

Run the hook evaluation:

```bash
unrdf hook eval --hook your-hook.json --data test-data.ttl
```

### 5. Analyze Results

Review the evaluation receipt:

- **Hook Fired**: Whether the hook detected issues
- **Predicate Results**: Which conditions were met
- **Performance Metrics**: Evaluation timing and efficiency
- **Provenance**: Cryptographic signatures for audit trails

## Customization

### Adapting to Your Environment

1. **Modify Thresholds**: Adjust numeric thresholds to match your requirements
2. **Update Queries**: Modify SPARQL queries to match your data structure
3. **Change Output**: Customize output format and destinations
4. **Add Predicates**: Include additional monitoring conditions

### Example Customization

```json
{
  "id": "ex:CustomServiceMonitor",
  "name": "Custom Service Monitor",
  "description": "Monitor services for your organization",
  "select": "SELECT ?service ?cpu ?memory WHERE { ?service ex:cpuUsage ?cpu ; ex:memoryUsage ?memory }",
  "predicates": [
    {
      "kind": "THRESHOLD",
      "spec": {
        "var": "cpu",
        "op": ">",
        "value": 80  // Your CPU threshold
      }
    },
    {
      "kind": "THRESHOLD",
      "spec": {
        "var": "memory",
        "op": ">",
        "value": 90  // Your memory threshold
      }
    }
  ],
  "combine": "OR",
  "output": {
    "schema": {
      "type": "object",
      "properties": {
        "service": { "type": "string" },
        "alert": { "type": "string" },
        "team": { "type": "string" }  // Your team identifier
      }
    },
    "destination": "webhook",
    "webhook": {
      "url": "https://your-monitoring-system.com/alerts"
    }
  }
}
```

## Integration Patterns

### Webhook Integration
All examples support webhook output for integration with existing systems:

```javascript
// Webhook payload structure
{
  "service": "ex:critical-service",
  "alert": "High error rate detected",
  "severity": "critical",
  "metrics": {
    "errorRate": 0.08,
    "latency": 2500
  },
  "recommendations": [
    "Investigate service performance",
    "Check for recent deployments",
    "Review error logs"
  ]
}
```

### Custom Processing
Use the custom output destination for specialized processing:

```javascript
// Custom handler example
const customHandler = async (data) => {
  // Log to your monitoring system
  await logToMonitoring(data);

  // Create incident ticket
  await createIncident(data);

  // Notify on-call engineer
  if (data.severity === 'critical') {
    await notifyOnCall(data);
  }
};
```

## Performance Considerations

### Query Optimization
- Use specific variable names in SELECT clauses
- Include only necessary variables
- Use LIMIT when monitoring large datasets
- Consider using COUNT predicates for cardinality checks

### Evaluation Frequency
- Service Health: Monitor every 1-5 minutes
- Compliance: Check every 15-30 minutes
- Drift Detection: Monitor continuously with short intervals

### Resource Management
- Monitor hook evaluation performance
- Set appropriate timeouts for complex evaluations
- Use baseline caching for frequently accessed data

## Troubleshooting

### Hook Not Firing
1. **Check Data**: Ensure your data matches the SPARQL query patterns
2. **Verify Thresholds**: Confirm threshold values are appropriate
3. **Test Predicates**: Test individual predicates separately
4. **Review Logs**: Check evaluation logs for errors

### Performance Issues
1. **Optimize Queries**: Use more specific SPARQL patterns
2. **Reduce Data**: Limit query results with FILTER or LIMIT
3. **Cache Results**: Use baseline caching for static data
4. **Monitor Resources**: Check system resource usage

### Integration Problems
1. **Validate Webhooks**: Test webhook endpoints independently
2. **Check Authentication**: Verify API keys and credentials
3. **Format Data**: Ensure output format matches receiver expectations
4. **Error Handling**: Implement proper error handling in receivers

## Best Practices

### Hook Design
- **Single Responsibility**: Each hook should monitor one logical domain
- **Clear Naming**: Use descriptive identifiers and names
- **Appropriate Granularity**: Balance detail with performance
- **Regular Review**: Periodically review and update hook logic

### Data Management
- **Consistent Schema**: Maintain consistent RDF data structure
- **Quality Data**: Ensure data accuracy and completeness
- **Timely Updates**: Keep baseline data current
- **Backup Strategy**: Implement data backup and recovery procedures

### Security
- **Access Control**: Restrict hook management to authorized personnel
- **Secure Communication**: Use HTTPS for all webhook communications
- **Audit Logging**: Maintain comprehensive audit trails
- **Credential Management**: Use secure credential storage and rotation

### Operations
- **Monitoring**: Track hook performance and reliability
- **Alerting**: Implement intelligent alerting to prevent fatigue
- **Documentation**: Document hook purposes and expected behaviors
- **Testing**: Regularly test hooks with various data scenarios

## Support

### Getting Help
- **Documentation**: Complete API reference available
- **Examples**: Use these examples as starting points
- **Community**: GitHub issues for bug reports and feature requests
- **Discussions**: GitHub discussions for questions and ideas

### Contributing
We welcome contributions! Each example should include:
- Complete hook definition
- Sample input data
- CLI usage instructions
- Expected output
- Explanation of behavior
- Customization guidance

## License

MIT License - see the main project LICENSE for details.
