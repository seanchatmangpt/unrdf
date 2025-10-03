// Package quotas provides administrative functions for quota management.
package quotas

import (
	"context"
	"encoding/json"
	"fmt"
)

// AdminServer provides administrative endpoints for quota management.
type AdminServer struct {
	quotaManager *QuotaManager
}

// NewAdminServer creates a new admin server for quotas.
func NewAdminServer(quotaManager *QuotaManager) *AdminServer {
	return &AdminServer{
		quotaManager: quotaManager,
	}
}

// SetQuotas sets quotas for a namespace.
func (as *AdminServer) SetQuotas(ctx context.Context, req SetQuotasRequest) (*SetQuotasResponse, error) {
	// Validate the request
	if err := ValidateQuotaRequest(req); err != nil {
		return &SetQuotasResponse{
			Applied: false,
			Error:   err.Error(),
		}, nil
	}

	// Set the quota
	quota := NamespaceQuota{
		QPS:    req.QPS,
		RowsPS: req.RowsPS,
	}
	as.quotaManager.SetQuota(req.Namespace, quota)

	return &SetQuotasResponse{
		Applied: true,
	}, nil
}

// GetQuotas gets quotas for a namespace.
func (as *AdminServer) GetQuotas(ctx context.Context, namespace string) (*SetQuotasResponse, error) {
	quota, exists := as.quotaManager.GetQuota(namespace)
	if !exists {
		return &SetQuotasResponse{
			Applied: false,
			Error:   fmt.Sprintf("no quota found for namespace %s", namespace),
		}, nil
	}

	// Convert quota back to request format for response
	responseBytes, err := json.Marshal(quota)
	if err != nil {
		return &SetQuotasResponse{
			Applied: false,
			Error:   fmt.Sprintf("failed to marshal quota: %v", err),
		}, nil
	}

	var req SetQuotasRequest
	if err := json.Unmarshal(responseBytes, &req); err != nil {
		return &SetQuotasResponse{
			Applied: false,
			Error:   fmt.Sprintf("failed to unmarshal quota: %v", err),
		}, nil
	}

	// Add the namespace to the response
	req.Namespace = namespace

	// Re-marshal with namespace
	responseBytes, err = json.Marshal(req)
	if err != nil {
		return &SetQuotasResponse{
			Applied: false,
			Error:   fmt.Sprintf("failed to marshal response: %v", err),
		}, nil
	}

	return &SetQuotasResponse{
		Applied: true,
	}, nil
}

// ListNamespaces lists all namespaces with quotas.
func (as *AdminServer) ListNamespaces(ctx context.Context) ([]string, error) {
	return as.quotaManager.ListNamespaces(), nil
}

// GetQuotaStats gets statistics for a namespace's quota usage.
func (as *AdminServer) GetQuotaStats(ctx context.Context, namespace string) (*QuotaStats, error) {
	return as.quotaManager.GetQuotaStats(namespace)
}

// ClearQuotas removes quotas for a namespace.
func (as *AdminServer) ClearQuotas(ctx context.Context, namespace string) error {
	as.quotaManager.ClearQuota(namespace)
	return nil
}