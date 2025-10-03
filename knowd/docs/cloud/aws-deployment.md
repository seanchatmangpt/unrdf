# AWS Deployment Guide

This guide covers deploying knowd on AWS using Terraform and Kubernetes.

## Prerequisites

- AWS CLI configured with appropriate permissions
- Terraform >= 1.0 installed
- kubectl configured to access your cluster
- Helm >= 3.0 installed

## Architecture

The AWS deployment includes:

- **VPC**: Multi-AZ VPC with public and private subnets
- **EKS**: Managed Kubernetes cluster with auto-scaling node groups
- **RDS**: Optional PostgreSQL database for persistence
- **Monitoring**: Prometheus, Jaeger, and Grafana stack
- **Load Balancing**: Network Load Balancer for high availability

## Quick Deployment

### 1. Initialize Terraform

```bash
cd deploy/terraform/aws
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
aws eks update-kubeconfig --region us-west-2 --name knowd-cluster
```

### 5. Deploy knowd

```bash
cd ../..
helm install knowd deploy/helm/knowd -f deploy/helm/knowd/values-production.yaml
```

## Configuration Options

### Environment Variables

```bash
export AWS_REGION="us-west-2"
export TF_VAR_environment="production"
export TF_VAR_knowd_replica_count="5"
```

### Custom Values

Create a `terraform.tfvars` file:

```hcl
region = "us-west-2"
environment = "production"
knowd_replica_count = 5
enable_rds = true
prometheus_enabled = true
jaeger_enabled = true
grafana_enabled = true
```

## Monitoring Setup

### Access Prometheus

```bash
kubectl port-forward svc/prometheus-server 9090:80 -n monitoring
# Open http://localhost:9090
```

### Access Grafana

```bash
kubectl port-forward svc/grafana 3000:80 -n monitoring
# Open http://localhost:3000 (admin/admin)
```

### Access Jaeger

```bash
kubectl port-forward svc/jaeger-query 16686:16686 -n monitoring
# Open http://localhost:16686
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

## Backup and Recovery

### EBS Snapshots

```bash
# Create snapshot of persistent volumes
kubectl get pv -o jsonpath='{.items[?(@.spec.claimRef.name=="knowd-data-pvc")].spec.awsElasticBlockStore.volumeID}'
aws ec2 create-snapshot --volume-id <volume-id>
```

### RDS Backups

```bash
# RDS automatic backups are enabled
aws rds describe-db-snapshots --db-instance-identifier knowd-postgres
```

## Security

### IAM Permissions

The deployment creates the following IAM roles:

- **EKS Cluster Role**: AmazonEKSClusterPolicy, AmazonEKSVPCResourceController
- **Node Group Role**: AmazonEKSWorkerNodePolicy, AmazonEC2ContainerRegistryReadOnly, CloudWatchAgentServerPolicy

### Security Groups

- **EKS Cluster**: Allows inbound from load balancer and nodes
- **Node Group**: Allows inter-node communication and cluster access

### Network Policies

```bash
kubectl apply -f deploy/k8s/network-policy.yaml
```

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
2. **Service not accessible**: Verify load balancer configuration
3. **Persistent volumes**: Check storage class and availability
4. **Monitoring not working**: Check Prometheus and Jaeger pod status

## Cost Optimization

### Right-sizing

- Monitor resource usage in Grafana dashboards
- Adjust CPU/memory requests and limits based on actual usage
- Use appropriate instance types for node groups

### Auto-scaling

- HPA scales pods based on CPU/memory utilization
- Cluster auto-scaler scales nodes based on pod requirements

### Storage Optimization

- Use appropriate storage class (gp3 for cost/performance)
- Monitor storage usage and plan capacity accordingly
- Use RDS only if relational data persistence is needed

## Production Checklist

- [ ] Multi-AZ deployment across 3 availability zones
- [ ] Load balancer health checks configured
- [ ] Monitoring and alerting rules set up
- [ ] Backup strategy implemented
- [ ] Security groups and network policies reviewed
- [ ] Resource limits and requests validated
- [ ] Auto-scaling policies tested
- [ ] Disaster recovery plan documented

## Support

For AWS-specific issues:
- Check AWS service health status
- Review CloudWatch logs and metrics
- Verify IAM permissions and policies
- Check VPC and security group configurations

Contact knowd support for application-specific issues.
