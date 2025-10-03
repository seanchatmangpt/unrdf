# Multi-stage build for knowd
FROM golang:1.22-alpine AS builder

# Install build dependencies
RUN apk add --no-cache git bash curl

# Set working directory
WORKDIR /app

# Copy go mod and sum files
COPY go.mod go.sum ./

# Download dependencies
RUN go mod download

# Copy source code
COPY . .

# Build the application
RUN go build -o knowd ./cmd/knowd

# Final stage
FROM alpine:latest

# Install runtime dependencies
RUN apk add --no-cache ca-certificates

# Create knowd user
RUN adduser -D -s /bin/sh knowd

# Set working directory
WORKDIR /app

# Copy binary from builder stage
COPY --from=builder /app/knowd .

# Create data directory
RUN mkdir -p /app/data && chown -R knowd:knowd /app

# Switch to knowd user
USER knowd

# Expose port
EXPOSE 8090

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8090/healthz || exit 1

# Run the application
CMD ["./knowd"]
