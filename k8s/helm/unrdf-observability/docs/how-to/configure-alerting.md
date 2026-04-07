# How to: Configure Alerting

Set up Prometheus alert rules and Alertmanager notification routes.

---

## Alert rules

Alert rules are defined in the Prometheus ConfigMap (`configmap-prometheus.yaml`). The chart includes them as part of the `prometheus.yml` data:

```yaml
rule_files:
  - /etc/prometheus/alert-rules.yml
```

To add custom alert rules, edit `configmap-prometheus.yaml` and add rules under the `alert-rules.yml` key. Use Helm escaping for Prometheus template variables:

```yaml
data:
  prometheus.yml: |
    # ...
  alert-rules.yml: |
    groups:
      - name: my-rules
        rules:
          - alert: HighErrorRate
            expr: |
              rate(http_requests_total{status=~"5.."}[5m])
              / rate(http_requests_total[5m]) > 0.05
            for: 5m
            labels:
              severity: warning
            annotations:
              summary: "High error rate on {{ "{{" }} $labels.job {{ "}}" }}"
```

After editing, upgrade:

```bash
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability --reuse-values
kubectl -n unrdf-observability rollout restart deployment/unrdf-observability-prometheus
```

## Alertmanager routing

The default Alertmanager config uses a `null` receiver (alerts are captured but not sent anywhere). To route to real receivers:

### Slack

```bash
helm upgrade unrdf-observability k8s/helm/unrdf-observability \
  --namespace unrdf-observability \
  --reuse-values \
  --set alertmanager.slackWebhookUrl="https://hooks.slack.com/services/XXX/YYY/ZZZ"
```

Then update `configmap-alertmanager.yaml` to include a Slack receiver:

```yaml
receivers:
  - name: slack
    slack_configs:
      - api_url: http://unrdf-observability-alertmanager:9093
        channel: '#alerts'
        send_resolved: true
```

### PagerDuty

```yaml
receivers:
  - name: pagerduty
    pagerduty_configs:
      - service_key: <YOUR_PAGERDUTY_KEY>
        severity: '{{ "{{" }} .GroupLabels.severity {{ "}}" }}'
```

### Webhook

```yaml
receivers:
  - name: webhook
    webhook_configs:
      - url: "https://your-webhook.example.com/alerts"
        send_resolved: true
```

## Loki alert rules

Loki alert rules are in `configmap-loki-rules.yaml`. The chart includes a sample `rate-limit-errors` rule. Add your own rules in the same file:

```yaml
data:
  loki-rules.yaml: |
    groups:
      - name: unrdf-rules
        rules:
          - alert: HighLogErrorRate
            expr: |
              sum(rate({namespace="unrdf-observability"} |= "error" [5m])) by (app_kubernetes_io_component)
              > 10
            for: 10m
            labels:
              severity: warning
```

## Verify alerts

```bash
# Check Prometheus targets
curl -s http://localhost:9091/api/v1/targets | jq '.data.activeTargets[] | {job: .labels.job, health: .health}'

# Check Alertmanager
curl -s http://localhost:9093/api/v2/alerts | jq '.[] | {status: .status.state, name: .labels.alertname}'
```
