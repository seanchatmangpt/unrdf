# Production Deployment Architecture

Deploying UNRDF + Next.js to production requires careful architecture. This chapter covers battle-tested deployment patterns.

## Vercel Deployment (Recommended)

**Best for:** Most applications, fastest time-to-production

```
┌─────────────────────────────────────────────┐
│         Vercel Edge Network (300+ POPs)    │
├─────────────────────────────────────────────┤
│  Edge Functions (Semantic Search, Queries)  │
│  └─ <50ms global latency                   │
├─────────────────────────────────────────────┤
│  Serverless Functions (Hooks, Mutations)   │
│  └─ Auto-scaling, 0 cold starts            │
├─────────────────────────────────────────────┤
│  Static Assets (CDN)                        │
│  └─ HTML, CSS, JS, Images                  │
└─────────────────────────────────────────────┘
                  ↓
┌─────────────────────────────────────────────┐
│            Supabase (Database)              │
├─────────────────────────────────────────────┤
│  Postgres (RDF Quads)                       │
│  pgvector (Embeddings)                      │
│  Real-time Subscriptions                    │
└─────────────────────────────────────────────┘
```

### Setup

```bash
# Install Vercel CLI
pnpm add -g vercel

# Deploy
vercel --prod
```

### Configuration

```javascript
// vercel.json
{
  "buildCommand": "pnpm build",
  "devCommand": "pnpm dev",
  "installCommand": "pnpm install",
  "framework": "nextjs",
  "regions": ["iad1", "sfo1", "fra1", "sin1"], // Multi-region
  "env": {
    "DATABASE_URL": "@database-url",
    "UNRDF_ENABLE_OBSERVABILITY": "true"
  },
  "headers": [
    {
      "source": "/api/(.*)",
      "headers": [
        { "key": "Cache-Control", "value": "s-maxage=60, stale-while-revalidate" }
      ]
    }
  ]
}
```

## AWS Deployment

**Best for:** Enterprise requirements, regulatory compliance, cost optimization

```
┌────────────────────────────────────────────────┐
│           CloudFront (CDN + WAF)               │
└────────────────────────────────────────────────┘
                  ↓
┌────────────────────────────────────────────────┐
│      Application Load Balancer (ALB)           │
└────────────────────────────────────────────────┘
                  ↓
┌────────────────────────────────────────────────┐
│    ECS Fargate (Auto-scaling Next.js)         │
│    ├─ Task 1: Next.js Server                  │
│    ├─ Task 2: Next.js Server                  │
│    └─ Task N: Next.js Server                  │
└────────────────────────────────────────────────┘
                  ↓
┌────────────────────────────────────────────────┐
│        RDS Aurora (Postgres)                   │
│        ├─ Writer Instance                      │
│        └─ Reader Replicas (3x)                 │
└────────────────────────────────────────────────┘
```

### Terraform Configuration

```hcl
# infrastructure/main.tf
resource "aws_ecs_service" "nextjs" {
  name            = "unrdf-nextjs"
  cluster         = aws_ecs_cluster.main.id
  task_definition = aws_ecs_task_definition.nextjs.arn
  desired_count   = 3
  
  load_balancer {
    target_group_arn = aws_lb_target_group.nextjs.arn
    container_name   = "nextjs"
    container_port   = 3000
  }
  
  network_configuration {
    subnets          = aws_subnet.private.*.id
    security_groups  = [aws_security_group.ecs.id]
  }
}

resource "aws_rds_cluster" "knowledge" {
  cluster_identifier = "unrdf-knowledge-graph"
  engine             = "aurora-postgresql"
  engine_version     = "15.3"
  database_name      = "knowledge"
  master_username    = var.db_username
  master_password    = var.db_password
  
  serverlessv2_scaling_configuration {
    min_capacity = 0.5
    max_capacity = 16
  }
}
```

## Kubernetes Deployment

**Best for:** Multi-cloud, hybrid cloud, on-premises

```
┌──────────────────────────────────────────────┐
│         Ingress (nginx/istio)                │
└──────────────────────────────────────────────┘
                  ↓
┌──────────────────────────────────────────────┐
│    Next.js Deployment (HPA: 2-10 pods)      │
│    ├─ Pod 1: Next.js + UNRDF                │
│    ├─ Pod 2: Next.js + UNRDF                │
│    └─ Pod N: Next.js + UNRDF                │
└──────────────────────────────────────────────┘
                  ↓
┌──────────────────────────────────────────────┐
│    PostgreSQL StatefulSet (3 replicas)      │
│    ├─ Primary                                │
│    └─ Replicas (2x)                          │
└──────────────────────────────────────────────┘
```

### Kubernetes Manifests

```yaml
# k8s/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: unrdf-nextjs
spec:
  replicas: 3
  selector:
    matchLabels:
      app: unrdf-nextjs
  template:
    metadata:
      labels:
        app: unrdf-nextjs
    spec:
      containers:
      - name: nextjs
        image: unrdf/nextjs:latest
        ports:
        - containerPort: 3000
        env:
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: db-credentials
              key: url
        - name: UNRDF_ENABLE_OBSERVABILITY
          value: "true"
        resources:
          requests:
            memory: "512Mi"
            cpu: "500m"
          limits:
            memory: "2Gi"
            cpu: "2000m"
        livenessProbe:
          httpGet:
            path: /api/health
            port: 3000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /api/ready
            port: 3000
          initialDelaySeconds: 5
          periodSeconds: 5

---
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: unrdf-nextjs-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: unrdf-nextjs
  minReplicas: 2
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
```

## Health Checks

```typescript
// app/api/health/route.ts
import { createKnowledgeEngine } from 'unrdf';

export async function GET() {
  const checks = {
    timestamp: new Date().toISOString(),
    status: 'healthy',
    checks: {}
  };
  
  try {
    // Check database connectivity
    const engine = await createKnowledgeEngine();
    await engine.query('SELECT * WHERE { ?s ?p ?o } LIMIT 1');
    checks.checks.database = 'ok';
  } catch (error) {
    checks.status = 'unhealthy';
    checks.checks.database = 'error';
  }
  
  const status = checks.status === 'healthy' ? 200 : 503;
  return Response.json(checks, { status });
}
```

## Monitoring Setup

```typescript
// lib/observability.ts
import { createKnowledgeEngine } from 'unrdf';
import { PrometheusExporter } from '@opentelemetry/exporter-prometheus';
import { JaegerExporter } from '@opentelemetry/exporter-jaeger';

export async function initializeObservability() {
  const engine = await createKnowledgeEngine({
    observability: {
      serviceName: 'unrdf-production',
      serviceVersion: process.env.APP_VERSION || 'unknown',
      exporters: [
        new PrometheusExporter({ port: 9090 }),
        new JaegerExporter({
          endpoint: process.env.JAEGER_ENDPOINT
        })
      ],
      sampleRate: parseFloat(process.env.TRACE_SAMPLE_RATE || '0.1'),
      attributes: {
        'deployment.environment': process.env.NODE_ENV,
        'service.namespace': 'knowledge-graph'
      }
    }
  });
  
  return engine;
}
```

## Disaster Recovery

### Backup Strategy

```typescript
// scripts/backup.ts
import { createKnowledgeEngine } from 'unrdf';
import { S3Client, PutObjectCommand } from '@aws-sdk/client-s3';

async function backupKnowledgeGraph() {
  const engine = await createKnowledgeEngine();
  const s3 = new S3Client({ region: 'us-east-1' });
  
  // Export to N-Quads
  const nquads = await engine.serialize('nquads');
  
  // Upload to S3
  await s3.send(new PutObjectCommand({
    Bucket: 'unrdf-backups',
    Key: `backup-${Date.now()}.nq`,
    Body: nquads,
    ServerSideEncryption: 'AES256'
  }));
  
  console.log('Backup complete');
}

// Run daily
backupKnowledgeGraph();
```

### Point-in-Time Recovery

```typescript
// scripts/restore.ts
async function restoreFromBackup(timestamp: number) {
  const engine = await createKnowledgeEngine();
  
  // Download backup from S3
  const backup = await downloadBackup(timestamp);
  
  // Clear current data
  await engine.deleteAll();
  
  // Restore from backup
  await engine.deserialize(backup, 'nquads');
  
  console.log('Restore complete');
}
```

## Cost Optimization

### Vercel

- **Edge Functions:** Free for 100GB-hours/month
- **Serverless Functions:** $0.40 per 100GB-hours
- **Bandwidth:** First 100GB free

**Estimated cost for 100K users/month:** $50-200

### AWS

- **ECS Fargate:** $0.04048/vCPU/hour + $0.004445/GB/hour
- **RDS Aurora:** $0.12/hour (serverless) to $1.50/hour (provisioned)
- **CloudFront:** $0.085/GB (first 10TB)

**Estimated cost for 100K users/month:** $500-2000

### Kubernetes (Self-Hosted)

- **Infrastructure:** Variable (cloud or on-prem)
- **Postgres:** Free (self-hosted)
- **Monitoring:** Free (Prometheus + Grafana)

**Estimated cost:** $200-1000/month (cloud) or hardware costs (on-prem)

## Next Steps

- **[Performance Optimization](./performance.md)**
- **[Security Hardening](./security.md)**
- **[Monitoring & Alerting](./monitoring.md)**

---

> **🎯 Production Tip:** Start with Vercel for fastest deployment, migrate to AWS/K8s when you need fine-grained control or have specific regulatory requirements.
