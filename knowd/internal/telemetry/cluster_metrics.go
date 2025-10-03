package telemetry

import (
	"sync"
	"time"
)

// ClusterMetrics tracks cluster-specific metrics.
type ClusterMetrics struct {
	mu sync.RWMutex

	// Replication metrics
	ReplicationLagSeconds   float64
	WALApplyRateOpsPerSec   float64
	SnapshotShipBytesPerSec float64
	FollowerCount           int

	// Leader metrics
	LeaderWriteRateOpsPerSec float64
	LeaderCommitLatencyMs    float64

	// Network metrics
	NetworkLatencyMs float64
	NetworkErrors    int64

	// Last update time
	LastUpdate time.Time
}

// NewClusterMetrics creates a new cluster metrics tracker.
func NewClusterMetrics() *ClusterMetrics {
	return &ClusterMetrics{
		LastUpdate: time.Now(),
	}
}

// UpdateReplicationLag updates the replication lag metric.
func (cm *ClusterMetrics) UpdateReplicationLag(lagSeconds float64) {
	cm.mu.Lock()
	defer cm.mu.Unlock()
	cm.ReplicationLagSeconds = lagSeconds
	cm.LastUpdate = time.Now()
}

// UpdateWALApplyRate updates the WAL apply rate metric.
func (cm *ClusterMetrics) UpdateWALApplyRate(rateOpsPerSec float64) {
	cm.mu.Lock()
	defer cm.mu.Unlock()
	cm.WALApplyRateOpsPerSec = rateOpsPerSec
	cm.LastUpdate = time.Now()
}

// UpdateSnapshotShipRate updates the snapshot shipping rate metric.
func (cm *ClusterMetrics) UpdateSnapshotShipRate(rateBytesPerSec float64) {
	cm.mu.Lock()
	defer cm.mu.Unlock()
	cm.SnapshotShipBytesPerSec = rateBytesPerSec
	cm.LastUpdate = time.Now()
}

// UpdateFollowerCount updates the follower count metric.
func (cm *ClusterMetrics) UpdateFollowerCount(count int) {
	cm.mu.Lock()
	defer cm.mu.Unlock()
	cm.FollowerCount = count
	cm.LastUpdate = time.Now()
}

// UpdateLeaderWriteRate updates the leader write rate metric.
func (cm *ClusterMetrics) UpdateLeaderWriteRate(rateOpsPerSec float64) {
	cm.mu.Lock()
	defer cm.mu.Unlock()
	cm.LeaderWriteRateOpsPerSec = rateOpsPerSec
	cm.LastUpdate = time.Now()
}

// UpdateLeaderCommitLatency updates the leader commit latency metric.
func (cm *ClusterMetrics) UpdateLeaderCommitLatency(latencyMs float64) {
	cm.mu.Lock()
	defer cm.mu.Unlock()
	cm.LeaderCommitLatencyMs = latencyMs
	cm.LastUpdate = time.Now()
}

// UpdateNetworkLatency updates the network latency metric.
func (cm *ClusterMetrics) UpdateNetworkLatency(latencyMs float64) {
	cm.mu.Lock()
	defer cm.mu.Unlock()
	cm.NetworkLatencyMs = latencyMs
	cm.LastUpdate = time.Now()
}

// IncrementNetworkErrors increments the network errors counter.
func (cm *ClusterMetrics) IncrementNetworkErrors() {
	cm.mu.Lock()
	defer cm.mu.Unlock()
	cm.NetworkErrors++
	cm.LastUpdate = time.Now()
}

// GetMetrics returns a copy of the current cluster metrics.
func (cm *ClusterMetrics) GetMetrics() ClusterMetricsSnapshot {
	cm.mu.RLock()
	defer cm.mu.RUnlock()

	return ClusterMetricsSnapshot{
		ReplicationLagSeconds:   cm.ReplicationLagSeconds,
		WALApplyRateOpsPerSec:   cm.WALApplyRateOpsPerSec,
		SnapshotShipBytesPerSec: cm.SnapshotShipBytesPerSec,
		FollowerCount:          cm.FollowerCount,
		LeaderWriteRateOpsPerSec: cm.LeaderWriteRateOpsPerSec,
		LeaderCommitLatencyMs:   cm.LeaderCommitLatencyMs,
		NetworkLatencyMs:        cm.NetworkLatencyMs,
		NetworkErrors:          cm.NetworkErrors,
		LastUpdate:             cm.LastUpdate,
	}
}

// ClusterMetricsSnapshot represents a snapshot of cluster metrics (safe for concurrent access).
type ClusterMetricsSnapshot struct {
	ReplicationLagSeconds   float64
	WALApplyRateOpsPerSec   float64
	SnapshotShipBytesPerSec float64
	FollowerCount           int
	LeaderWriteRateOpsPerSec float64
	LeaderCommitLatencyMs    float64
	NetworkLatencyMs        float64
	NetworkErrors           int64
	LastUpdate              time.Time
}

// IsHealthy returns true if the cluster appears healthy based on metrics.
func (cms *ClusterMetricsSnapshot) IsHealthy() bool {
	// Consider cluster healthy if:
	// - Replication lag is under 60 seconds
	// - Network latency is under 100ms
	// - No recent network errors (within last minute)
	now := time.Now()
	errorThreshold := now.Add(-time.Minute)

	if cms.ReplicationLagSeconds > 60 {
		return false
	}

	if cms.NetworkLatencyMs > 100 {
		return false
	}

	// If we have network errors, check if they were recent
	if cms.NetworkErrors > 0 {
		return cms.LastUpdate.After(errorThreshold)
	}

	return true
}