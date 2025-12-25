# UNRDF Grafana Dashboards

This directory contains Grafana dashboard configurations for monitoring UNRDF production systems.

## Available Dashboards

### 1. `unrdf-overview.json` - System Overview Dashboard

**Purpose**: Executive dashboard for overall system health and performance

**Panels**:
- System Health (Status indicator)
- OTEL Validation Score (Gauge 0-100)
- Request Rate (Time series)
- Error Rate (Time series with thresholds)
- Request Latency P50/P95/P99 (Multi-line graph)
- Memory Usage (Heap/RSS metrics)
- Test Quality Metrics (Failure rate)
- Slow Queries (Time series)
- Active Alerts (Table)
- CPU Usage (User/System)
- Dependency Health (Status table)

**Import Instructions**:

```bash
# Using Grafana API
curl -X POST http://localhost:3000/api/dashboards/import \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer ${GRAFANA_API_KEY}" \
  -d @unrdf-overview.json

# Or import via UI:
# 1. Go to Grafana UI > Dashboards > Import
# 2. Upload unrdf-overview.json
# 3. Select Prometheus datasource
# 4. Click Import
```

## Dashboard Variables

All dashboards support these template variables:

- `datasource` - Prometheus datasource (default: Prometheus)
- `environment` - Environment filter (production, staging, dev)

## Alert Annotations

Dashboards automatically show annotations for:
- **Deployments** (blue) - When service version changes
- **Alerts** (red) - Critical alerts firing

## Customization

### Adding Custom Panels

1. Edit the dashboard JSON file
2. Add new panel to `panels` array
3. Set unique `id` and appropriate `gridPos`
4. Configure target queries using PromQL

Example panel:
```json
{
  "id": 12,
  "title": "Custom Metric",
  "type": "graph",
  "gridPos": { "x": 0, "y": 20, "w": 12, "h": 6 },
  "targets": [
    {
      "expr": "unrdf_custom_metric",
      "legendFormat": "{{label}}",
      "refId": "A"
    }
  ]
}
```

### Modifying Thresholds

Update `fieldConfig.defaults.thresholds.steps`:

```json
{
  "thresholds": {
    "mode": "absolute",
    "steps": [
      { "value": 0, "color": "green" },
      { "value": 80, "color": "yellow" },
      { "value": 90, "color": "red" }
    ]
  }
}
```

## Dashboard Best Practices

1. **Keep it Simple** - Show only actionable metrics (80/20 principle)
2. **Set Thresholds** - Color-code based on SLOs
3. **Use Annotations** - Mark deployments and incidents
4. **Consistent Time Ranges** - Sync time ranges across panels
5. **Test Alerts** - Verify dashboards show alert states correctly

## Metric Queries

### Common PromQL Queries

**Error Rate**:
```promql
rate(unrdf_errors_total[5m]) / rate(unrdf_requests_total[5m])
```

**P95 Latency**:
```promql
histogram_quantile(0.95, rate(unrdf_request_duration_seconds_bucket[5m]))
```

**Memory Usage Percentage**:
```promql
unrdf_memory_heap_used_bytes / unrdf_memory_heap_total_bytes * 100
```

**OTEL Score**:
```promql
unrdf_otel_validation_score
```

## Troubleshooting

### Dashboard Not Loading

1. Check datasource is configured: Settings > Data Sources > Prometheus
2. Verify Prometheus is scraping metrics: `curl http://localhost:3000/metrics`
3. Test queries in Explore tab

### Missing Data

1. Check time range (top-right corner)
2. Verify metrics exist: Explore > Run query
3. Check label selectors match your environment

### Alerts Not Showing

1. Verify Alertmanager is configured
2. Check alert rules in Prometheus: Status > Rules
3. Ensure annotations are enabled in dashboard settings

## Support

- **Documentation**: `/home/user/unrdf/MONITORING.md`
- **Runbook**: `/home/user/unrdf/monitoring/RUNBOOK.md`
- **Alert Rules**: `/home/user/unrdf/monitoring/alerts.yml`

---

**Remember**: Dashboards should enable quick incident response, not just look pretty. Every panel must answer: "What action do I take if this changes?"
