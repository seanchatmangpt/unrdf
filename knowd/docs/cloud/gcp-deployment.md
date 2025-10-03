# GCP Deployment Guide

This guide covers deploying knowd on Google Cloud Platform using Terraform and Kubernetes.

## Prerequisites

- Google Cloud CLI configured with appropriate permissions
- Terraform >= 1.0 installed
- kubectl configured to access your cluster
- Helm >= 3.0 installed

## Architecture

The GCP deployment includes:

- **VPC**: Multi-region VPC with custom subnets
- **GKE**: Autopilot or Standard GKE cluster with auto-scaling
- **Cloud SQL**: Optional PostgreSQL database for persistence
- **Cloud Operations**: Cloud Monitoring, Cloud Logging, Cloud Trace
- **Load Balancing**: Google Cloud Load Balancer for high availability

## Quick Deployment

### 1. Initialize Terraform

```bash
cd deploy/terraform/gcp
terraform init
```

### 2. Review Configuration

```bash
terraform plan
```

### 3. Deploy Infrastructure

```bash
terraform apply
```

### 4. Configure kubectl

```bash
gcloud container clusters get-credentials knowd-cluster --region us-central1
```

### 5. Deploy knowd

```bash
cd ../..
helm install knowd deploy/helm/knowd -f deploy/helm/knowd/values-production.yaml
```

## Configuration Options

### Environment Variables

```bash
export GOOGLE_CLOUD_PROJECT="your-project-id"
export GOOGLE_CLOUD_REGION="us-central1"
export TF_VAR_environment="production"
export TF_VAR_knowd_replica_count="5"
```

### Custom Values

Create a `terraform.tfvars` file:

```hcl
project_id = "your-project-id"
region = "us-central1"
environment = "production"
knowd_replica_count = 5
enable_cloud_sql = true
monitoring_enabled = true
```

## Cloud Operations Setup

### Access Cloud Monitoring

```bash
# View metrics in Cloud Console
open https://console.cloud.google.com/monitoring
```

### Access Cloud Logging

```bash
# View logs in Cloud Console
open https://console.cloud.google.com/logs
```

### Access Cloud Trace

```bash
# View traces in Cloud Console
open https://console.cloud.google.com/traces
```

## Scaling

### Manual Scaling

```bash
kubectl scale deployment knowd --replicas=5
```

### Auto-scaling

The deployment includes HPA that scales based on CPU and memory usage:

```bash
kubectl get hpa knowd-hpa
```

### GKE Cluster Auto-scaler

GKE automatically scales nodes based on resource requirements:

```bash
gcloud container clusters update knowd-cluster \
  --enable-autoscaling \
  --min-nodes=3 \
  --max-nodes=10
```

## Backup and Recovery

### Persistent Disk Snapshots

```bash
# Create snapshot of persistent disks
kubectl get pv -o jsonpath='{.items[?(@.spec.gcePersistentDisk.pdName)].spec.gcePersistentDisk.pdName}'
gcloud compute disks snapshot <disk-name> --zone <zone>
```

### Cloud SQL Backups

```bash
# Cloud SQL automatic backups are enabled
gcloud sql backups list --instance=knowd-postgres
```

## Security

### IAM and RBAC

The deployment creates the following IAM roles:

- **GKE Service Account**: Kubernetes Engine Service Agent
- **Node Service Account**: Cloud KMS CryptoKey Encrypter/Decrypter (if using CMEK)

### Network Security

- **VPC Firewall Rules**: Allow necessary traffic between components
- **Private GKE Cluster**: Master authorized networks configured
- **Cloud Armor**: Optional WAF protection for load balancer

### Binary Authorization

Enable Binary Authorization for enhanced security:

```bash
gcloud container clusters update knowd-cluster \
  --enable-binauthz
```

## Cost Optimization

### Right-sizing

- Monitor resource usage in Cloud Monitoring dashboards
- Use appropriate machine types for GKE nodes
- Configure preemptible nodes for non-critical workloads

### Auto-scaling

- HPA scales pods based on CPU/memory utilization
- GKE cluster auto-scaler scales nodes based on demand
- Use committed use discounts for predictable workloads

### Storage Optimization

- Use appropriate persistent disk types (SSD for performance, HDD for cost)
- Monitor storage usage and plan capacity accordingly
- Use Cloud SQL only if relational data persistence is needed

## Monitoring Integration

### Cloud Monitoring Metrics

The deployment automatically exports metrics to Cloud Monitoring:

- **knowd metrics**: Query latency, throughput, error rates
- **Kubernetes metrics**: Pod CPU/memory, node utilization
- **Custom metrics**: Application-specific business metrics

### Cloud Logging

All logs are automatically sent to Cloud Logging:

- **knowd logs**: Application logs with structured format
- **Kubernetes logs**: Pod and container logs
- **Audit logs**: Control plane and data access logs

### Cloud Trace

Distributed tracing data is sent to Cloud Trace:

- **Request tracing**: Full request lifecycle traces
- **Service dependencies**: Inter-service communication traces
- **Performance analysis**: Latency and bottleneck identification

## Troubleshooting

### Check Pod Status

```bash
kubectl get pods -n knowd
kubectl describe pod <pod-name> -n knowd
kubectl logs <pod-name> -n knowd
```

### Check Service Status

```bash
kubectl get svc -n knowd
kubectl describe svc knowd-service -n knowd
```

### Check Persistent Volumes

```bash
kubectl get pvc -n knowd
kubectl describe pvc knowd-data-pvc -n knowd
```

### Common Issues

1. **Pods not starting**: Check resource limits and node capacity
2. **Service not accessible**: Verify load balancer configuration and firewall rules
3. **Persistent volumes**: Check storage class and disk availability
4. **Network issues**: Review VPC firewall rules and network policies

## Production Checklist

- [ ] Multi-region deployment with 3+ zones
- [ ] Load balancer health checks configured
- [ ] Cloud Operations monitoring and alerting set up
- [ ] Backup strategy implemented
- [ ] VPC and firewall rules reviewed
- [ ] Resource limits and requests validated
- [ ] Auto-scaling policies tested
- [ ] Disaster recovery plan documented
- [ ] Binary Authorization enabled
- [ ] Network policies implemented

## Cost Monitoring

### Cloud Billing

- Set up billing alerts for cost thresholds
- Use Cloud Billing reports to track expenses
- Monitor resource utilization vs. costs

### Cost Optimization Tools

- **GCP Cost Explorer**: Analyze and optimize costs
- **Committed Use Discounts**: Plan for predictable workloads
- **Sustained Use Discounts**: Automatic discounts for sustained usage

## Support

For GCP-specific issues:
- Check Google Cloud Status Dashboard
- Review Cloud Logging and Cloud Monitoring
- Verify IAM permissions and service accounts
- Check VPC and firewall configurations

Contact knowd support for application-specific issues.
