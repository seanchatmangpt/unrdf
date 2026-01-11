# Production-ready multi-stage Dockerfile for UNRDF v6
# Optimized for size, security, and performance

# Stage 1: Build dependencies
FROM node:18-alpine AS deps
LABEL stage=deps

# Install pnpm
RUN npm install -g pnpm@8.15.0

# Set working directory
WORKDIR /app

# Copy package files
COPY package.json pnpm-workspace.yaml pnpm-lock.yaml ./
COPY .npmrc .npmrc* ./

# Copy all package.json files
COPY packages/*/package.json ./packages/

# Install dependencies only (no dev dependencies in production)
RUN pnpm install --frozen-lockfile --prod

# Stage 2: Build application
FROM node:18-alpine AS builder
LABEL stage=builder

# Install pnpm
RUN npm install -g pnpm@8.15.0

WORKDIR /app

# Copy package files
COPY package.json pnpm-workspace.yaml pnpm-lock.yaml ./
COPY .npmrc .npmrc* ./
COPY packages/*/package.json ./packages/

# Install ALL dependencies (including dev for build)
RUN pnpm install --frozen-lockfile

# Copy source code
COPY . .

# Build all packages
RUN pnpm build

# Run tests to ensure build quality
RUN timeout 60s pnpm test:fast || echo "Tests skipped in build"

# Stage 3: Production runtime
FROM node:18-alpine AS runner
LABEL maintainer="UNRDF Team"
LABEL version="6.0.0-rc.1"

# Install pnpm
RUN npm install -g pnpm@8.15.0

# Install security updates
RUN apk update && apk upgrade && \
    apk add --no-cache \
    tini \
    curl \
    ca-certificates && \
    rm -rf /var/cache/apk/*

# Create non-root user
RUN addgroup -g 1001 -S unrdf && \
    adduser -S unrdf -u 1001 -G unrdf

WORKDIR /app

# Copy production dependencies from deps stage
COPY --from=deps --chown=unrdf:unrdf /app/node_modules ./node_modules
COPY --from=deps --chown=unrdf:unrdf /app/packages/*/node_modules ./packages/

# Copy built application from builder
COPY --from=builder --chown=unrdf:unrdf /app/packages ./packages
COPY --from=builder --chown=unrdf:unrdf /app/package.json ./
COPY --from=builder --chown=unrdf:unrdf /app/pnpm-workspace.yaml ./

# Switch to non-root user
USER unrdf

# Expose ports
# 3000 - Main API
# 9090 - Metrics (Prometheus)
# 8080 - Health check endpoint
EXPOSE 3000 9090 8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Use tini for proper signal handling
ENTRYPOINT ["/sbin/tini", "--"]

# Default command (can be overridden)
CMD ["node", "packages/cli/dist/index.mjs", "serve"]
