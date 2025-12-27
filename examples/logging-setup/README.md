# UNRDF Log Aggregation Examples

This directory contains production-ready configurations for log aggregation with ELK Stack and Loki.

## Overview

Both setups implement:
- **Log sampling**: 1% DEBUG log sampling (configurable)
- **Priority filtering**: WARN/ERROR logs always captured
- **Structured logging**: JSON format with OpenTelemetry trace context
- **Retention policies**: 14-day default retention
- **High availability**: Ready for scaling

## ELK Stack Setup

### Components
- **Elasticsearch**: Log storage and indexing
- **Logstash**: Log processing pipeline with sampling
- **Kibana**: Visualization and search UI
- **Filebeat**: Log shipping from files/containers

### Quick Start
```bash
cd examples/logging-setup
docker-compose -f docker-compose-elk.yml up -d
```

### Access
- Kibana: http://localhost:5601
- Elasticsearch: http://localhost:9200

### Configuration
Edit `logstash-config/logstash.conf` to adjust:
- DEBUG log sampling rate (default: 1 in 100)
- Index patterns
- Filtering rules

## Loki Setup

### Components
- **Loki**: Log aggregation system
- **Promtail**: Log collection agent
- **Grafana**: Visualization (includes dashboards from /dashboards)

### Quick Start
```bash
cd examples/logging-setup
docker-compose -f docker-compose-loki.yml up -d
```

### Access
- Grafana: http://localhost:3000 (admin/admin)
- Loki API: http://localhost:3100

### Configuration
Edit `loki-config/promtail-config.yml` to adjust:
- DEBUG log sampling rate (default: 0.01 = 1%)
- Log paths
- Label extraction

## UNRDF Integration

### Structured Logging Format
```javascript
import { defaultObservabilityManager } from '@unrdf/knowledge-engine';

// Use the built-in sampled logger
observabilityManager.log('DEBUG', 'Processing transaction', {
  transaction_id: txId,
  trace_id: traceId
});

// WARN and ERROR always logged
observabilityManager.log('ERROR', 'Validation failed', {
  score: 45,
  threshold: 80
});
```

### Environment Variables
```bash
# Enable OTEL logs export to aggregation system
export OTEL_LOGS_EXPORTER=otlp
export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318

# For Logstash
export LOG_SHIPPER_HOST=localhost
export LOG_SHIPPER_PORT=5000

# For Loki
export LOKI_URL=http://localhost:3100
```

## Query Examples

### ELK (Kibana)
```
# Find all errors in last hour
service:"unrdf-kgc" AND log_level:"ERROR" AND @timestamp:[now-1h TO now]

# Trace a specific transaction
trace_id:"abc123" AND service:"unrdf-kgc"

# High-latency operations
kgc_metric:"transaction_latency" AND duration:>5000
```

### Loki (LogQL)
```
# Find all errors
{service="unrdf-kgc",level="ERROR"}

# Trace-based filtering
{service="unrdf-kgc"} |= "trace_id" | json | trace_id="abc123"

# Rate of errors
rate({service="unrdf-kgc",level="ERROR"}[5m])

# Sampled DEBUG logs
{service="unrdf-kgc",level="DEBUG"} # Auto-sampled at 1%
```

## Performance Notes

### Sampling Impact
- **Without sampling**: ~10GB/day logs at 1000 req/s
- **With 1% DEBUG sampling**: ~1-2GB/day logs
- **WARN/ERROR overhead**: <50MB/day (critical path always captured)

### Resource Requirements

#### ELK Stack
- Elasticsearch: 2GB RAM minimum, 4GB recommended
- Logstash: 512MB RAM minimum
- Kibana: 512MB RAM minimum

#### Loki
- Loki: 512MB RAM minimum, 1GB recommended
- Promtail: 128MB RAM minimum
- Grafana: 512MB RAM minimum

## Alerting Integration

Both setups integrate with Prometheus Alertmanager (see `/dashboards/prometheus-alerts.yml`):

```yaml
# Alert on high error log rate
- alert: HighErrorLogRate
  expr: rate({level="ERROR"}[5m]) > 0.1
  annotations:
    summary: "High error log rate detected"
```

## Production Checklist

- [ ] Adjust retention policies based on compliance requirements
- [ ] Configure authentication (disabled in examples)
- [ ] Set up index lifecycle management (ELK) or compaction (Loki)
- [ ] Enable TLS for log shipping
- [ ] Configure backup/restore procedures
- [ ] Tune sampling rates based on traffic
- [ ] Set up alerting rules
- [ ] Configure RBAC for team access

## Troubleshooting

### ELK: Logs not appearing
1. Check Logstash pipeline: `docker logs unrdf-logstash`
2. Verify connectivity: `curl http://localhost:9200/_cat/indices`
3. Check Filebeat: `docker exec unrdf-filebeat filebeat test output`

### Loki: No data in Grafana
1. Check Loki ingestion: `curl http://localhost:3100/ready`
2. Verify Promtail: `docker logs unrdf-promtail`
3. Test query: `curl -G -s "http://localhost:3100/loki/api/v1/query" --data-urlencode 'query={job="unrdf-logs"}'`

## References

- [ELK Stack Documentation](https://www.elastic.co/guide/index.html)
- [Grafana Loki Documentation](https://grafana.com/docs/loki/latest/)
- [OpenTelemetry Logs](https://opentelemetry.io/docs/specs/otel/logs/)
- [UNRDF Observability Guide](../../docs/observability.md)
