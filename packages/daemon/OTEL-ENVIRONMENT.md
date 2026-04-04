# OTEL Environment Configuration - @unrdf/daemon

**Version**: 26.4.4
**Last Updated**: 2026-04-03

---

## Overview

The @unrdf/daemon package includes full OpenTelemetry (OTEL) integration for distributed tracing, metrics collection, and semantic convention enforcement via Weaver. This document describes all environment variables for configuring OTEL.

---

## Quick Start

To enable OTEL tracing in the daemon:

```bash
# Enable OTEL tracing
export OTEL_ENABLED=true

# Configure OTLP endpoint
export OTEL_EXPORTER_OTLP_ENDPOINT=localhost:4317

# Start the daemon
node src/daemon.mjs
```

---

## Required Environment Variables

### `OTEL_ENABLED`

Enable or disable OTEL tracing.

**Type**: `boolean`
**Default**: `true`
**Description**: When `false`, all OTEL instrumentation is disabled

**Example**:

```bash
export OTEL_ENABLED=true
```

**Validation**:

- If `false`, OTEL SDK is not initialized
- If `true`, OTEL SDK is initialized on daemon startup

---

## OTEL SDK Configuration

### `OTEL_EXPORTER_OTLP_ENDPOINT`

OTLP gRPC exporter endpoint URL.

**Type**: `string`
**Default**: `localhost:4317`
**Description**: The endpoint where OTEL spans will be exported

**Example**:

```bash
# Local OTLP collector
export OTEL_EXPORTER_OTLP_ENDPOINT=localhost:4317

# Remote OTLP collector
export OTEL_EXPORTER_OTLP_ENDPOINT=otel-collector:4317

# HTTPS endpoint
export OTEL_EXPORTER_OTLP_ENDPOINT=https://otel-collector.company.com
```

**Reference**: See `otel-sdk.mjs` lines 34-37

---

## Service Resource Attributes

### `OTEL_SERVICE_NAME`

Service name for all telemetry.

**Type**: `string`
**Default**: `unrdf-daemon`
**Description**: Name of the service for tracing and metrics

**Example**:

```bash
export OTEL_SERVICE_NAME=unrdf-daemon
```

**Note**: This attribute is included in all spans and metrics

---

### `OTEL_SERVICE_VERSION`

Service version.

**Type**: `string`
**Default**: `26.4.4`
**Description**: Version of the service for tracing

**Example**:

```bash
export OTEL_SERVICE_VERSION=26.4.4
```

---

### `OTEL_RESOURCE_ATTRIBUTES`

Additional resource attributes.

**Type**: `string`
**Default**: `service.name=unrdf-daemon,service.version=26.4.4`
**Description**: Comma-separated list of additional resource attributes

**Example**:

```bash
export OTEL_RESOURCE_ATTRIBUTES=service.name=unrdf-daemon,service.version=26.4.4,deployment.environment=production
```

---

## Sampling Configuration

### `OTEL_TRACES_SAMPLER`

Sampling strategy for distributed tracing.

**Type**: `string`
**Default**: `parentbased_traceidratio`
**Valid Values**:

- `always_on` - Sample all traces
- `always_off` - Sample no traces
- `traceidratio` - Sample based on trace ID ratio
- `parentbased_always_on` - Sample if parent is sampled
- `parentbased_traceidratio` - Sample if parent is sampled, else by trace ID ratio

**Example**:

```bash
# Sample 100% of traces
export OTEL_TRACES_SAMPLER=always_on

# Sample 50% of traces
export OTEL_TRACES_SAMPLER=traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.5

# Sample only if parent is sampled (recommended)
export OTEL_TRACES_SAMPLER=parentbased_traceidratio
export OTEL_TRACES_SAMPLER_ARG=1.0
```

---

### `OTEL_TRACES_SAMPLER_ARG`

Sampling rate for trace ID ratio sampler.

**Type**: `number` (0.0 to 1.0)
**Default**: `1.0` (100% sampling)
**Description**: Probability of sampling each trace

**Example**:

```bash
export OTEL_TRACES_SAMPLER=traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.1  # 10% sampling
```

---

## Performance Configuration

### `OTEL_BATCH_MAX_QUEUE_SIZE`

Maximum queue size for span batching.

**Type**: `number`
**Default**: `1000`
**Description**: Maximum number of spans to queue before export

**Example**:

```bash
export OTEL_BATCH_MAX_QUEUE_SIZE=10000
```

**Reference**: See `otel-sdk.mjs` lines 41-44

---

### `OTEL_EXPORT_TIMEOUT_MILLIS`

Timeout for span export in milliseconds.

**Type**: `number`
**Default**: `100`
**Description**: Maximum time to wait for span export

**Example**:

```bash
export OTEL_EXPORT_TIMEOUT_MILLIS=5000
```

**Note**: Lower values increase latency, higher values increase reliability

---

## Production Configuration

### Complete Production Setup

```bash
# Enable OTEL
export OTEL_ENABLED=true

# OTLP endpoint
export OTEL_EXPORTER_OTLP_ENDPOINT=otel-collector.company.com:4317

# Service identity
export OTEL_SERVICE_NAME=unrdf-daemon
export OTEL_SERVICE_VERSION=26.4.4
export OTEL_RESOURCE_ATTRIBUTES=deployment.environment=production,cluster=us-west-2

# Sampling (10% recommended)
export OTEL_TRACES_SAMPLER=parentbased_traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.1

# Performance
export OTEL_BATCH_MAX_QUEUE_SIZE=10000
export OTEL_EXPORT_TIMEOUT_MILLIS=5000

# Start daemon
node src/daemon.mjs
```

---

## Troubleshooting

### Issue: No spans being collected

**Possible Causes**:

1. `OTEL_ENABLED=false`
2. OTLP collector not running
3. Network connectivity issues

**Solutions**:

```bash
# Verify OTEL is enabled
echo $OTEL_ENABLED  # Should be "true"

# Check OTLP endpoint
echo $OTEL_EXPORTER_OTLP_ENDPOINT  # Should be reachable

# Check daemon logs for OTEL initialization
# Look for: [OTEL SDK] OpenTelemetry SDK initialized successfully
```

---

### Issue: High memory usage

**Possible Causes**:

1. Large `OTEL_BATCH_MAX_QUEUE_SIZE`
2. Too much sampling

**Solutions**:

```bash
# Reduce queue size
export OTEL_BATCH_MAX_QUEUE_SIZE=100

# Reduce sampling rate
export OTEL_TRACES_SAMPLER_ARG=0.01  # 1% sampling
```

---

### Issue: Spans not visible in Jaeger/Tempo

**Possible Causes**:

1. Incorrect OTLP endpoint
2. Network firewall blocking
3. Sampling too low

**Solutions**:

```bash
# Verify endpoint is correct
curl http://<endpoint>/metrics  # Should return Prometheus metrics

# Check sampling rate
export OTEL_TRACES_SAMPLER=always_on  # Test with 100% sampling

# Check Jaeger UI for traces
# Navigate to: http://localhost:16686/search
```

---

## Integration with Weaver

### Semantic Convention Validation

Weaver automatically validates all OTEL spans against custom conventions:

- `daemon_mcp` - MCP tool execution conventions
- `daemon_scheduling` - Daemon scheduling operations
- `daemon_cluster` - Cluster membership and state

**Validation Workflow**:

```bash
# Run Weaver live-check
weaver registry live-check \
  --registry 'https://github.com/open-telemetry/semantic-conventions.git[model]' \
  --input-source stdin \
  --input-format json \
  --output none < otlp-export.json
```

**Sample Output**:

```
✅ daemon_mcp.mcp.tool.name
✅ daemon_mcp.mcp.tool.success
✅ daemon_scheduling.daemon.trigger.type
✅ daemon_cluster.daemon.cluster.is_leader
```

---

## References

- [OpenTelemetry Specification](https://opentelemetry.io/docs/reference/specification/)
- [Weaver Documentation](https://github.com/weaver-ops/weaver)
- [OTLP gRPC Protocol](https://opentelemetry.io/docs/reference/specification/protocol/otlp/)
- [Jaeger UI](https://www.jaegertracing.io/docs/latest/ui/)
- [Tempo UI](https://grafana.com/docs/tempo/latest/ui/)

---

## Support

For issues or questions:

1. Check daemon logs for OTEL initialization messages
2. Verify all environment variables are set correctly
3. Test OTEL endpoint connectivity
4. Review Weaver validation output

**Log Messages**:

- `[OTEL SDK] Initializing OpenTelemetry SDK...`
- `[OTEL SDK] OpenTelemetry SDK initialized successfully`
- `[OTEL SDK] Service: unrdf-daemon`
- `[OTEL SDK] Endpoint: localhost:4317`
- `[OTEL SDK] Shutting down OpenTelemetry SDK...`
- `[OTEL SDK] OpenTelemetry SDK shutdown complete`
