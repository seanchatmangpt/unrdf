// Package quotas provides per-namespace rate limiting using token buckets.
package quotas

import (
	"context"
	"fmt"
	"sync"

	"golang.org/x/time/rate"
)

// NamespaceQuota represents quotas for a namespace.
type NamespaceQuota struct {
	QPS    int `json:"qps"`    // Queries per second (0 = unlimited)
	RowsPS int `json:"rowsps"` // Rows per second (0 = unlimited)
}

// QuotaManager manages per-namespace quotas using token buckets.
type QuotaManager struct {
	mu       sync.RWMutex
	quotas   map[string]*NamespaceQuota
	limiters map[string]*rate.Limiter
}

// NewQuotaManager creates a new quota manager.
func NewQuotaManager() *QuotaManager {
	return &QuotaManager{
		quotas:   make(map[string]*NamespaceQuota),
		limiters: make(map[string]*rate.Limiter),
	}
}

// SetQuota sets or updates quotas for a namespace.
func (qm *QuotaManager) SetQuota(namespace string, quota NamespaceQuota) {
	qm.mu.Lock()
	defer qm.mu.Unlock()

	qm.quotas[namespace] = &quota

	// Create or update rate limiter
	if quota.QPS > 0 {
		// rate.Limit is events per second
		limit := rate.Limit(quota.QPS)
		burst := quota.QPS // Allow burst up to QPS

		if existing, exists := qm.limiters[namespace]; exists {
			// Update existing limiter
			existing.SetLimit(limit)
			existing.SetBurst(burst)
		} else {
			// Create new limiter
			qm.limiters[namespace] = rate.NewLimiter(limit, burst)
		}
	} else {
		// Remove limiter for unlimited QPS
		delete(qm.limiters, namespace)
	}
}

// GetQuota gets the current quota for a namespace.
func (qm *QuotaManager) GetQuota(namespace string) (*NamespaceQuota, bool) {
	qm.mu.RLock()
	defer qm.mu.RUnlock()

	quota, exists := qm.quotas[namespace]
	if !exists {
		return nil, false
	}

	// Return a copy to prevent external modification
	quotaCopy := *quota
	return &quotaCopy, true
}

// CheckQPS checks if a query is allowed under the QPS quota.
func (qm *QuotaManager) CheckQPS(ctx context.Context, namespace string) error {
	qm.mu.RLock()
	limiter, exists := qm.limiters[namespace]
	qm.mu.RUnlock()

	if !exists {
		// No quota set, allow
		return nil
	}

	// Check if we can reserve a token (non-blocking for queries)
	if !limiter.Allow() {
		return fmt.Errorf("QPS quota exceeded for namespace %s", namespace)
	}

	return nil
}

// ReserveRows reserves rows for streaming queries.
func (qm *QuotaManager) ReserveRows(ctx context.Context, namespace string, rowCount int) error {
	qm.mu.RLock()
	quota, exists := qm.quotas[namespace]
	qm.mu.RUnlock()

	if !exists || quota.RowsPS == 0 {
		// No rows quota or unlimited
		return nil
	}

	// For simplicity, we'll implement this as a basic check
	// In a full implementation, this would use a separate token bucket for rows

	// This is a simplified implementation
	// A real implementation would need a separate rate limiter for rows
	return fmt.Errorf("rows quota implementation incomplete")
}

// GetQuotaStats returns statistics about quota usage.
func (qm *QuotaManager) GetQuotaStats(namespace string) (*QuotaStats, error) {
	qm.mu.RLock()
	defer qm.mu.RUnlock()

	stats := &QuotaStats{
		Namespace: namespace,
	}

	quota, exists := qm.quotas[namespace]
	if exists {
		stats.QPS = quota.QPS
		stats.RowsPS = quota.RowsPS
		stats.HasQuota = true
	} else {
		stats.HasQuota = false
	}

	// In a real implementation, we'd track actual usage stats
	stats.CurrentQPS = 0
	stats.CurrentRowsPS = 0

	return stats, nil
}

// ListNamespaces returns all namespaces that have quotas configured.
func (qm *QuotaManager) ListNamespaces() []string {
	qm.mu.RLock()
	defer qm.mu.RUnlock()

	var namespaces []string
	for ns := range qm.quotas {
		namespaces = append(namespaces, ns)
	}

	return namespaces
}

// ClearQuota removes quotas for a namespace.
func (qm *QuotaManager) ClearQuota(namespace string) {
	qm.mu.Lock()
	defer qm.mu.Unlock()

	delete(qm.quotas, namespace)
	delete(qm.limiters, namespace)
}

// QuotaStats represents statistics about quota usage.
type QuotaStats struct {
	Namespace     string `json:"namespace"`
	HasQuota      bool   `json:"has_quota"`
	QPS           int    `json:"qps"`
	RowsPS        int    `json:"rowsps"`
	CurrentQPS    int    `json:"current_qps"`
	CurrentRowsPS int    `json:"current_rowsps"`
}

// SetQuotasRequest represents a request to set quotas.
type SetQuotasRequest struct {
	Namespace string `json:"namespace"`
	QPS       int    `json:"qps"`
	RowsPS    int    `json:"rowsps"`
}

// SetQuotasResponse represents the response to setting quotas.
type SetQuotasResponse struct {
	Applied bool   `json:"applied"`
	Error   string `json:"error,omitempty"`
}

// ValidateQuotaRequest validates a quota setting request.
func ValidateQuotaRequest(req SetQuotasRequest) error {
	if req.Namespace == "" {
		return fmt.Errorf("namespace cannot be empty")
	}

	if req.QPS < 0 {
		return fmt.Errorf("QPS cannot be negative")
	}

	if req.RowsPS < 0 {
		return fmt.Errorf("RowsPS cannot be negative")
	}

	return nil
}
