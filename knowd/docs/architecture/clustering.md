# Cluster Architecture

This document describes the clustering architecture of Knowd, which enables horizontal scaling, high availability, and federated query execution across multiple nodes.

## Architecture Overview

Knowd implements a leader-follower cluster architecture optimized for knowledge graph workloads:

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   Leader    │◀───┤   Follower  │◀───┤   Follower  │
│   Node      │    │   Node 1    │    │   Node 2    │
│             │    │             │    │             │
│ • Writes    │    │ • Reads     │    │ • Reads     │
│ • WAL       │───▶│ • WAL       │───▶│ • WAL       │
│ • Hooks     │    │ • Queries   │    │ • Queries   │
│ • SHACL     │    │ • Vectors   │    │ • Vectors   │
└─────────────┘    └─────────────┘    └─────────────┘
        │                   │                   │
        └───────────────────┼───────────────────┘
                            ▼
                    ┌─────────────┐
                    │   Client    │
                    │   Load      │
                    │   Balancer  │
                    └─────────────┘
```

## Cluster Roles

### Leader Node

The leader node is responsible for:
- **Write Operations**: All transactions (POST /v1/tx) are processed by the leader
- **WAL Management**: Maintains the write-ahead log for durability
- **Hook Execution**: Runs knowledge hooks and SHACL validation
- **Receipt Generation**: Creates cryptographic receipts for all transactions
- **Snapshot Creation**: Generates periodic full store snapshots
- **Replication Coordination**: Ships WAL entries and snapshots to followers

**Key Files:**
- `internal/cluster/leader.go` - Leader election and write coordination
- `internal/store/disk/wal.go` - WAL append and management
- `internal/store/disk/snapshot.go` - Snapshot generation

### Follower Nodes

Follower nodes provide:
- **Read Scaling**: Handle read-only operations (queries, vector search)
- **WAL Replication**: Receive and apply WAL entries from leader
- **Query Federation**: Participate in distributed query execution
- **Automatic Recovery**: Catch up from snapshots after failures
- **Promotion Capability**: Can be promoted to leader if needed

**Key Files:**
- `internal/cluster/follower.go` - Follower state management
- `internal/cluster/rpc.go` - gRPC replication protocol

## Replication Protocol

### WAL Replication

**Write-Ahead Log Shipping:**

```
Leader Node:
    1. Receive Transaction
    2. Execute Hooks & Validation
    3. Append to WAL
    4. Stream WAL Entry → Followers

Follower Nodes:
    1. Receive WAL Entry
    2. Apply to Local Store
    3. Acknowledge Receipt
    4. Update Read Indexes
```

**Features:**
- **Backpressure Control**: Followers can signal when they're overwhelmed
- **Batch Processing**: Multiple WAL entries can be batched for efficiency
- **Compression**: WAL entries are compressed during transmission
- **Encryption**: Optional encryption for sensitive data

### Snapshot Replication

**Periodic State Synchronization:**

```
Leader Node (Every 5 minutes):
    1. Create Store Snapshot
    2. Compress & Encrypt
    3. Upload to Object Storage
    4. Notify Followers

Follower Nodes:
    1. Download Latest Snapshot
    2. Verify Integrity (SHA256)
    3. Replace Local Store
    4. Resume WAL Application
```

**Benefits:**
- **Fast Recovery**: Followers can catch up quickly from snapshots
- **Storage Efficiency**: Snapshots are compressed and deduplicated
- **Integrity Verification**: Cryptographic verification of snapshot contents

## Federated Query Execution

### Scatter/Gather Pattern

**Distributed Query Processing:**

```
Client Query → Leader Node
    ↓
Parse & Plan Query
    ↓
Identify Eligible Followers
    ↓
Scatter Sub-Queries → Followers
    ↓
Followers Execute Local Queries
    ↓
Gather & Merge Results → Leader
    ↓
Apply ORDER BY/LIMIT → Client
```

**Query Types Supported:**
- **SELECT Queries**: Can be federated across followers
- **ASK Queries**: Boolean results from any follower
- **Vector Search**: Distributed similarity search

**Optimization:**
- **Predicate Pushdown**: Push filtering to followers where possible
- **Parallel Execution**: Execute sub-queries concurrently
- **Result Deduplication**: Remove duplicate results during merging

## Configuration

### Cluster Modes

```bash
# Single node (default)
KNOWD_CLUSTER_MODE=single

# Leader node
KNOWD_CLUSTER_MODE=leader
KNOWD_PEER_ADDRS=follower1:8091,follower2:8092
KNOWD_MTLS_CERT=/path/to/cert.pem
KNOWD_MTLS_KEY=/path/to/key.pem
KNOWD_MTLS_CA=/path/to/ca.pem

# Follower node
KNOWD_CLUSTER_MODE=follower
KNOWD_PEER_ADDRS=leader:8090
KNOWD_MTLS_CERT=/path/to/cert.pem
KNOWD_MTLS_KEY=/path/to/key.pem
KNOWD_MTLS_CA=/path/to/ca.pem
```

### Replication Settings

```bash
# WAL replication
KNOWD_REPL_SNAPSHOT_SEC=300        # Snapshot every 5 minutes
KNOWD_REPL_STREAM_CHUNK_BYTES=1048576  # 1MB chunks

# Performance tuning
KNOWD_CLUSTER_READ_TIMEOUT=30s      # Read operation timeout
KNOWD_CLUSTER_WRITE_TIMEOUT=60s     # Write operation timeout
KNOWD_CLUSTER_MAX_PEERS=10          # Maximum cluster size
```

## Monitoring and Health Checks

### Cluster Status Endpoint

```bash
# Get cluster status
curl http://leader:8090/v1/cluster/status

# Response
{
  "mode": "leader",
  "followers": 2,
  "lagSec": 0.4,
  "lastSnapshot": "2025-01-01T10:00:00Z",
  "walSize": 1048576
}
```

### Health Checks

**Leader Health:**
- WAL write capability
- Hook execution status
- Follower connectivity
- Snapshot generation

**Follower Health:**
- WAL application rate
- Query execution status
- Leader connectivity
- Index consistency

## Operational Procedures

### Adding a Follower

1. **Configure Follower:**
   ```bash
   KNOWD_CLUSTER_MODE=follower
   KNOWD_PEER_ADDRS=leader:8090
   KNOWD_MTLS_CERT=follower.crt
   KNOWD_MTLS_KEY=follower.key
   KNOWD_MTLS_CA=ca.crt
   ```

2. **Start Follower:** Follower will automatically sync from latest snapshot

3. **Verify Replication:** Check cluster status endpoint

### Leader Failover

**Automatic Detection:**
- Heartbeat monitoring between nodes
- Leader election if heartbeat fails
- Automatic follower promotion

**Manual Promotion:**
```bash
# Promote follower to leader
curl -X POST http://follower:8090/v1/admin/promote-follower \
  -H "Content-Type: application/json" \
  -d '{"barrier": "latest"}'
```

### Scaling Operations

**Horizontal Scaling:**
- Add followers for read scaling
- Monitor query distribution and balance load
- Scale vector indexes across nodes

**Vertical Scaling:**
- Increase memory for larger caches
- Add CPU cores for parallel query execution
- Expand storage for larger datasets

## Security Considerations

### Network Security
- **mTLS Required**: All cluster communication uses mutual TLS
- **Certificate Management**: Automated certificate rotation
- **Network Segmentation**: Cluster traffic isolated from client traffic

### Data Security
- **Encryption at Rest**: Store snapshots encrypted with AES-256
- **Encryption in Transit**: All replication data encrypted
- **Access Control**: Namespace isolation prevents cross-tenant access

### Operational Security
- **Audit Logging**: All cluster operations logged
- **Access Monitoring**: Track who accesses cluster admin endpoints
- **Secure Defaults**: All security features enabled by default

## Performance Characteristics

### Scalability Metrics

**Linear Scaling:**
- Read throughput scales linearly with follower count
- Vector search performance scales with index distribution
- Query federation efficiency improves with more followers

**Overhead:**
- WAL replication: <5% CPU overhead
- Snapshot shipping: <10% network overhead
- Federated queries: <15% coordination overhead

### Latency Targets

**Local Operations:**
- Single-node queries: <100ms p95
- Vector searches: <5ms p95
- Hook execution: <50ms p95

**Distributed Operations:**
- Federated queries: <200ms p95
- Cross-node transactions: <500ms p95
- Failover recovery: <30 seconds

## Troubleshooting

### Common Issues

**Replication Lag:**
- Check network connectivity between nodes
- Monitor follower resource utilization
- Verify WAL compression settings

**Query Federation Failures:**
- Check follower health status
- Verify network partitions
- Monitor query complexity limits

**Leader Election Issues:**
- Validate mTLS certificate validity
- Check cluster configuration consistency
- Monitor heartbeat intervals

### Diagnostic Commands

```bash
# Check cluster health
curl http://leader:8090/healthz

# View detailed cluster status
curl http://leader:8090/v1/cluster/status

# Monitor replication metrics
curl http://leader:8090/metrics | grep knowd_cluster

# Check follower sync status
curl http://follower:8090/v1/cluster/status
```

This clustering architecture provides the scalability, reliability, and performance required for production knowledge graph deployments while maintaining the security and operational simplicity needed for enterprise environments.
