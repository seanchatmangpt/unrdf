# Distributed Orchestration Dockerfile
FROM node:18-alpine

WORKDIR /app

# Install pnpm
RUN npm install -g pnpm@8.15.0

# Copy package files
COPY package.json ./
COPY ../../package.json ../../pnpm-workspace.yaml ../../pnpm-lock.yaml ./

# Install dependencies
RUN pnpm install --frozen-lockfile

# Copy source code
COPY src/ ./src/

# Expose ports
EXPOSE 8080 3000

# Default to orchestrator mode
CMD ["node", "src/orchestrator.mjs"]
