# Cloud Integrations

This directory contains comprehensive cloud integrations for deploying knowd in various cloud environments.

## Supported Platforms

- **Kubernetes**: Production-ready deployment manifests and Helm charts
- **AWS**: Complete Terraform modules for EKS, VPC, and monitoring
- **GCP**: Terraform modules for GKE and Cloud Operations
- **Azure**: Terraform modules for AKS and Azure Monitor

## Quick Start

### Kubernetes (Helm)

```bash
# Install knowd using Helm
helm repo add knowd https://charts.unrdf.com
helm repo update
helm install knowd knowd/knowd -f values-production.yaml
```

### AWS (Terraform)

```bash
# Deploy complete knowd infrastructure on AWS
cd deploy/terraform/aws
terraform init
terraform plan
terraform apply
```

### GCP (Terraform)

```bash
# Deploy on Google Cloud Platform
cd deploy/terraform/gcp
terraform init
terraform plan
terraform apply
```

## Architecture Overview

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Load Balancer │    │   EKS/GKE/AKS   │    │   knowd Pods    │
│   (NLB/ALB)     │────│   Cluster       │────│   (3 replicas)  │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                │
                ┌───────────────┼───────────────┐
                │               │               │
        ┌───────▼───────┐ ┌─────▼──────┐ ┌─────▼──────┐
        │   Prometheus   │ │   Jaeger   │ │  Grafana   │
        │   Monitoring   │ │   Tracing  │ │ Dashboards │
        └───────────────┘ └────────────┘ └────────────┘
```

## Features

- **High Availability**: Multi-zone deployment with 3 replicas
- **Auto-scaling**: Horizontal Pod Autoscaler based on CPU/memory
- **Monitoring**: Prometheus metrics, Jaeger tracing, Grafana dashboards
- **Persistence**: Persistent storage for data and configuration
- **Security**: Pod Security Standards, RBAC, network policies
- **Backup**: Automated backups and disaster recovery

## Security Best Practices

- Non-root containers with restricted permissions
- Network policies for pod-to-pod communication
- mTLS for service-to-service authentication
- Encrypted persistent volumes
- Regular security scanning and updates

## Performance Optimization

- Resource requests and limits configured
- Horizontal Pod Autoscaler for scaling
- Persistent volume optimization
- Query result caching
- Connection pooling for external services

## Monitoring & Alerting

- **Metrics**: Query latency, throughput, error rates
- **Traces**: Distributed tracing for request flows
- **Logs**: Structured logging with correlation IDs
- **Alerts**: Critical system health and performance alerts
- **Dashboards**: Comprehensive Grafana dashboards

## Cost Optimization

- Right-sized resource allocation
- Auto-scaling based on demand
- Persistent volume optimization
- Efficient monitoring stack
- Cost monitoring and alerting

## Troubleshooting

- **Pod Issues**: Check logs, resource usage, and events
- **Service Issues**: Verify service endpoints and load balancer
- **Storage Issues**: Check persistent volume status and capacity
- **Network Issues**: Review network policies and security groups

## Support

For issues and questions:
- Check the troubleshooting guide above
- Review the logs in the monitoring dashboards
- Open an issue in the knowd repository
- Contact the knowd support team
