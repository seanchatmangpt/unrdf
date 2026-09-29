# Multi-stage Dockerfile for the UNRDF CLI (@unrdf/cli and its workspace dependencies).
#
# The CLI is executed from source (packages/cli/package.json "bin" is
# src/cli/main.mjs), so no build step is required. Node version follows
# .tool-versions; the lockfile is lockfileVersion 9, which needs pnpm >= 9.

# Stage 1: install dependencies for the CLI and the workspace packages it needs
FROM node:24-alpine AS builder
LABEL stage=builder

RUN npm install -g pnpm@10

WORKDIR /app

# The workspace layout (packages/<name>/package.json) must be preserved for a
# frozen-lockfile install, and a glob COPY flattens directories, so copy the
# whole tree (.dockerignore keeps it small).
COPY . .

# --ignore-scripts: no native build toolchain in the image; the CLI does not
# rely on install scripts.
RUN pnpm install --frozen-lockfile --ignore-scripts --filter "@unrdf/cli..."

# Run the CLI test suite to ensure build quality (fails the build on failure)
RUN timeout 120s pnpm --filter @unrdf/cli test:fast

# Stage 2: runtime
FROM node:24-alpine AS runner
LABEL maintainer="UNRDF Team"
LABEL version="6.0.0-rc.1"

RUN apk add --no-cache tini ca-certificates

RUN addgroup -g 1001 -S unrdf && \
    adduser -S unrdf -u 1001 -G unrdf

WORKDIR /app

COPY --from=builder --chown=unrdf:unrdf /app /app

USER unrdf

ENV NODE_ENV=production

# 3000 - Main API, 9090 - Metrics (Prometheus), 8080 - Health check endpoint
EXPOSE 3000 9090 8080

# The CLI has no HTTP health endpoint yet, so verify the CLI entrypoint loads.
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD node packages/cli/src/cli/main.mjs --version || exit 1

# Use tini for proper signal handling
ENTRYPOINT ["/sbin/tini", "--", "node", "packages/cli/src/cli/main.mjs"]

# Default command (can be overridden, e.g. `docker run unrdf sync --config ...`)
CMD ["--help"]
