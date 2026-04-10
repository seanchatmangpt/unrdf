
# codemanufactory-service

> Microservice scaffolded from CodeManufactory ontology

## Overview

codemanufactory-service is a microservice built for high-performance and scalability.

## Quick Start

```bash
cd 
make dev
make test
make build
```

## Service Details

- **Name**: codemanufactory-service
- **Port**: 8080

## API Endpoints

### Health Check

```
GET /health
```

**Response:**
```json
{
  "status": "healthy",
  "service": "",
  "version": "1.0.0"
}
```

## Configuration

### Environment Variables

```bash
SERVICE_NAME=
SERVICE_PORT=8080
NODE_ENV=development
OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
```

## Deployment

### Docker

```bash
docker build -t :latest .
docker run -p 8080:8080 :latest
```

## Monitoring

- **Health Endpoint**: `http://localhost:8080/health`
- **Metrics**: `http://localhost:8080/metrics`
- **Traces**: Available via OpenTelemetry
