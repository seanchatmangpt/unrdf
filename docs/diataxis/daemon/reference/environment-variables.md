# Environment Variables Reference

All environment variables recognized by `@unrdf/daemon`. Variables with no prefix are standard OpenTelemetry variables; `UNRDF_`-prefixed variables are daemon-specific.

---

## Core Daemon

| Variable        | Default | Description                                                                                                                                                                                                                             |
| --------------- | ------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `UNRDF_API_KEY` | —       | API key for authenticating daemon requests. When set, the daemon uses this value for BLAKE3-hashed key verification. If absent in `production` mode, all requests are rejected. In `development` mode, absence triggers a warning only. |

---

## OpenTelemetry: Enable / Disable

| Variable       | Default | Description                                                                                                                         |
| -------------- | ------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| `OTEL_ENABLED` | `true`  | Set to `false` to disable all OTel instrumentation. When `false`, `withMcpSpan()` is a no-op and the OTEL SDK is never initialized. |

---

## OpenTelemetry: Exporter

| Variable                      | Default          | Description                                                                |
| ----------------------------- | ---------------- | -------------------------------------------------------------------------- |
| `OTEL_EXPORTER_OTLP_ENDPOINT` | `localhost:4317` | gRPC endpoint for the OTLP span exporter. Accepts `host:port` or full URL. |

```bash
# Local collector
export OTEL_EXPORTER_OTLP_ENDPOINT=localhost:4317

# Remote collector
export OTEL_EXPORTER_OTLP_ENDPOINT=otel-collector.prod.internal:4317

# HTTPS endpoint
export OTEL_EXPORTER_OTLP_ENDPOINT=https://otel.company.com
```

---

## OpenTelemetry: Service Resource

| Variable                   | Default                                            | Description                                     |
| -------------------------- | -------------------------------------------------- | ----------------------------------------------- |
| `OTEL_SERVICE_NAME`        | `unrdf-daemon`                                     | Service name attached to all spans and metrics. |
| `OTEL_SERVICE_VERSION`     | `26.4.4`                                           | Service version attached to all spans.          |
| `OTEL_RESOURCE_ATTRIBUTES` | `service.name=unrdf-daemon,service.version=26.4.4` | Comma-separated additional resource attributes. |

```bash
export OTEL_RESOURCE_ATTRIBUTES=service.name=unrdf-daemon,service.version=26.4.4,deployment.environment=production
```

---

## OpenTelemetry: Sampling

| Variable                  | Default                    | Description                                                                       |
| ------------------------- | -------------------------- | --------------------------------------------------------------------------------- |
| `OTEL_TRACES_SAMPLER`     | `parentbased_traceidratio` | Sampling strategy.                                                                |
| `OTEL_TRACES_SAMPLER_ARG` | `1.0`                      | Sampling ratio (0.0–1.0) when using `traceidratio` or `parentbased_traceidratio`. |

**Valid sampler values:**

| Value                      | Behavior                                           |
| -------------------------- | -------------------------------------------------- |
| `always_on`                | Sample every trace                                 |
| `always_off`               | Sample no traces                                   |
| `traceidratio`             | Sample by ratio regardless of parent               |
| `parentbased_always_on`    | Sample if parent span is sampled                   |
| `parentbased_traceidratio` | Sample by ratio, but always follow parent decision |

```bash
# 10% sampling in high-traffic environments
export OTEL_TRACES_SAMPLER=traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.1

# Recommended for production (follow parent, sample 100% of root spans)
export OTEL_TRACES_SAMPLER=parentbased_traceidratio
export OTEL_TRACES_SAMPLER_ARG=1.0
```

---

## OpenTelemetry: Batch Export Performance

| Variable                           | Default | Description                                                                               |
| ---------------------------------- | ------- | ----------------------------------------------------------------------------------------- |
| `OTEL_BATCH_MAX_QUEUE_SIZE`        | `1000`  | Maximum number of spans to queue before export. Increase for high-throughput deployments. |
| `OTEL_BATCH_SCHEDULE_DELAY_MILLIS` | `500`   | Delay (ms) between scheduled batch exports.                                               |

```bash
export OTEL_BATCH_MAX_QUEUE_SIZE=10000
export OTEL_BATCH_SCHEDULE_DELAY_MILLIS=200
```

---

## Quick Reference: Minimal Production Setup

```bash
# Required
export OTEL_ENABLED=true
export OTEL_EXPORTER_OTLP_ENDPOINT=otel-collector:4317
export UNRDF_API_KEY=<your-secret-key>

# Recommended
export OTEL_SERVICE_NAME=unrdf-daemon
export OTEL_TRACES_SAMPLER=parentbased_traceidratio
export OTEL_TRACES_SAMPLER_ARG=1.0
export NODE_ENV=production

node src/daemon.mjs
```

## Quick Reference: Local Development

```bash
# Disable OTEL to avoid needing a local collector
export OTEL_ENABLED=false
export NODE_ENV=development

node src/daemon.mjs
# No API key required in development mode (warns but continues)
```

---

## See also

- [How to Enable OTel Tracing](../how-to/02-enable-otel-tracing.md)
- [How to Configure Security](../how-to/03-configure-security.md)
- `packages/daemon/OTEL-ENVIRONMENT.md` — extended OTEL variable documentation
- `packages/daemon/AUTHENTICATION.md` — full authentication configuration
