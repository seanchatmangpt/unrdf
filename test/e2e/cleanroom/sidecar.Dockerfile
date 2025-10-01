# KGC Sidecar Dockerfile with OpenTelemetry Instrumentation
# Purpose: gRPC server for knowledge graph operations with full OTEL observability
# Base: Node.js 20 Alpine for minimal size
# Startup: < 15 seconds

ARG NODE_VERSION=20
FROM node:${NODE_VERSION}-alpine AS base

# Install system dependencies
RUN apk add --no-cache \
    dumb-init \
    curl \
    wget \
    git \
    ca-certificates \
    && rm -rf /var/cache/apk/*

# Install grpc_health_probe for health checks
RUN wget -qO /usr/local/bin/grpc_health_probe \
    https://github.com/grpc-ecosystem/grpc-health-probe/releases/download/v0.4.24/grpc_health_probe-linux-amd64 \
    && chmod +x /usr/local/bin/grpc_health_probe

WORKDIR /app

# =============================================================================
# DEPENDENCIES STAGE
# =============================================================================
FROM base AS dependencies

# Copy package files
COPY package.json pnpm-lock.yaml ./

# Install pnpm and production dependencies
RUN npm install -g pnpm@8.15.0 && \
    pnpm install --frozen-lockfile --prod --ignore-scripts

# =============================================================================
# BUILD STAGE
# =============================================================================
FROM base AS build

# Copy package files
COPY package.json pnpm-lock.yaml ./

# Install all dependencies (including dev)
RUN npm install -g pnpm@8.15.0 && \
    pnpm install --frozen-lockfile --ignore-scripts

# Copy source code
COPY src/ ./src/
COPY test/fixtures/ ./test/fixtures/

# Build TypeScript if needed (optional, if using TS)
# RUN pnpm run build

# =============================================================================
# RUNTIME STAGE
# =============================================================================
FROM base AS runtime

# Create non-root user
RUN addgroup -g 1001 -S nodejs && \
    adduser -S nodejs -u 1001

# Copy dependencies from dependencies stage
COPY --from=dependencies --chown=nodejs:nodejs /app/node_modules ./node_modules

# Copy built application from build stage
COPY --from=build --chown=nodejs:nodejs /app/src ./src
COPY --from=build --chown=nodejs:nodejs /app/test/fixtures ./test/fixtures
COPY --chown=nodejs:nodejs package.json ./

# Install OpenTelemetry dependencies for runtime instrumentation
RUN npm install --no-save \
    @opentelemetry/sdk-node@0.45.1 \
    @opentelemetry/auto-instrumentations-node@0.40.3 \
    @opentelemetry/exporter-trace-otlp-grpc@0.45.1 \
    @opentelemetry/exporter-metrics-otlp-grpc@0.45.1 \
    @opentelemetry/instrumentation-grpc@0.45.1 \
    @opentelemetry/instrumentation-http@0.45.1 \
    @opentelemetry/resources@1.18.1 \
    @opentelemetry/semantic-conventions@1.18.1

# Copy OpenTelemetry instrumentation script
COPY --chown=nodejs:nodejs test/e2e/cleanroom/otel-instrumentation.mjs ./otel-instrumentation.mjs

# Set OpenTelemetry environment variables (defaults, can be overridden)
ENV OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317 \
    OTEL_EXPORTER_OTLP_PROTOCOL=grpc \
    OTEL_SERVICE_NAME=kgc-sidecar \
    OTEL_RESOURCE_ATTRIBUTES=service.version=2.0.0,deployment.environment=cleanroom \
    OTEL_TRACES_EXPORTER=otlp \
    OTEL_METRICS_EXPORTER=otlp \
    OTEL_LOGS_EXPORTER=otlp \
    OTEL_LOG_LEVEL=info \
    NODE_OPTIONS="--require ./otel-instrumentation.mjs"

# Application environment
ENV NODE_ENV=production \
    GRPC_PORT=50051 \
    LOG_LEVEL=info

# Expose ports
EXPOSE 50051 9464

# Health check using grpc_health_probe
HEALTHCHECK --interval=10s --timeout=5s --start-period=20s --retries=3 \
    CMD grpc_health_probe -addr=:50051 || exit 1

# Switch to non-root user
USER nodejs

# Use dumb-init to handle signals properly
ENTRYPOINT ["dumb-init", "--"]

# Start sidecar with OpenTelemetry auto-instrumentation
# The NODE_OPTIONS environment variable will load otel-instrumentation.mjs
CMD ["node", "src/sidecar/grpc-server.mjs"]

# =============================================================================
# BUILD INSTRUCTIONS
# =============================================================================
#
# Build image:
#   docker build -f test/e2e/cleanroom/sidecar.Dockerfile -t kgc-sidecar:latest .
#
# Run standalone:
#   docker run -p 50051:50051 \
#     -e OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317 \
#     -e POSTGRES_URL=postgresql://user:pass@postgres:5432/db \
#     kgc-sidecar:latest
#
# Run with docker-compose:
#   docker-compose -f test/e2e/cleanroom/docker-compose.yml up sidecar
#
# Health check:
#   grpc_health_probe -addr localhost:50051
#
# =============================================================================
# OPENTELEMETRY INSTRUMENTATION
# =============================================================================
#
# Automatic instrumentation via:
#   - @opentelemetry/auto-instrumentations-node (HTTP, gRPC, DNS, etc.)
#   - Custom instrumentation in otel-instrumentation.mjs
#
# Exported telemetry:
#   - Traces: All gRPC calls, HTTP requests, database queries
#   - Metrics: Request latency, error rates, throughput
#   - Logs: Structured logs with trace correlation
#
# OTLP endpoint configured via OTEL_EXPORTER_OTLP_ENDPOINT
# Default: http://otel-collector:4317 (gRPC)
#
# =============================================================================
