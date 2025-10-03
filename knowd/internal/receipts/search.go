// Package receipts provides search functionality for transaction receipts.
package receipts

import (
	"context"
	"fmt"
	"time"

	"github.com/unrdf/knowd/internal/types"
)

// SearchServer provides search endpoints for receipts.
type SearchServer struct {
	index *ReceiptIndex
}

// NewSearchServer creates a new search server.
func NewSearchServer(index *ReceiptIndex) *SearchServer {
	return &SearchServer{
		index: index,
	}
}

// SearchReceiptsRequest represents a search request.
type SearchReceiptsRequest struct {
	Actor string    `json:"actor"`
	Since time.Time `json:"since"`
	Until time.Time `json:"until"`
	Tags  []string  `json:"tags"`
	Limit int       `json:"limit"`
}

// SearchReceiptsResponse represents a search response.
type SearchReceiptsResponse struct {
	Receipts []*types.Receipt `json:"receipts"`
	Total    int              `json:"total"`
	Limit    int              `json:"limit"`
}

// SearchReceipts searches for receipts based on the provided filters.
func (ss *SearchServer) SearchReceipts(ctx context.Context, req SearchReceiptsRequest) (*SearchReceiptsResponse, error) {
	filters := ReceiptFilters{
		Actor: req.Actor,
		Since: req.Since,
		Until: req.Until,
		Tags:  req.Tags,
		Limit: req.Limit,
	}

	receipts, err := ss.index.SearchReceipts(ctx, filters)
	if err != nil {
		return nil, fmt.Errorf("failed to search receipts: %w", err)
	}

	total := len(receipts)
	if req.Limit > 0 && total > req.Limit {
		total = req.Limit
		receipts = receipts[:req.Limit]
	}

	return &SearchReceiptsResponse{
		Receipts: receipts,
		Total:    total,
		Limit:    req.Limit,
	}, nil
}

// GetReceipt retrieves a single receipt by ID.
func (ss *SearchServer) GetReceipt(ctx context.Context, receiptID string) (*types.Receipt, error) {
	receipt, exists := ss.index.GetReceipt(receiptID)
	if !exists {
		return nil, fmt.Errorf("receipt %s not found", receiptID)
	}

	return receipt, nil
}

// GetReceiptStats returns statistics about the receipt index.
func (ss *SearchServer) GetReceiptStats(ctx context.Context) (*ReceiptStats, error) {
	total := ss.index.GetReceiptCount()

	stats := &ReceiptStats{
		TotalReceipts: total,
		IndexSize:     total, // Simplified for now
	}

	return stats, nil
}

// ReceiptStats represents statistics about receipts.
type ReceiptStats struct {
	TotalReceipts int `json:"total_receipts"`
	IndexSize     int `json:"index_size"`
}

// ListReceiptsByActor lists all receipts for a specific actor.
func (ss *SearchServer) ListReceiptsByActor(ctx context.Context, actor string, limit int) ([]*types.Receipt, error) {
	filters := ReceiptFilters{
		Actor: actor,
		Limit: limit,
	}

	return ss.index.SearchReceipts(ctx, filters)
}

// ListReceiptsInTimeRange lists receipts within a time range.
func (ss *SearchServer) ListReceiptsInTimeRange(ctx context.Context, since, until time.Time, limit int) ([]*types.Receipt, error) {
	filters := ReceiptFilters{
		Since: since,
		Until: until,
		Limit: limit,
	}

	return ss.index.SearchReceipts(ctx, filters)
}

// GetRecentReceipts gets the most recent receipts.
func (ss *SearchServer) GetRecentReceipts(ctx context.Context, limit int) ([]*types.Receipt, error) {
	receiptIDs := ss.index.ListReceipts(limit)

	var receipts []*types.Receipt
	for _, id := range receiptIDs {
		receipt, exists := ss.index.GetReceipt(id)
		if exists {
			receipts = append(receipts, receipt)
		}
	}

	return receipts, nil
}

// ValidateSearchRequest validates a search request.
func ValidateSearchRequest(req SearchReceiptsRequest) error {
	if req.Actor != "" && len(req.Actor) > 255 {
		return fmt.Errorf("actor name too long")
	}

	if !req.Since.IsZero() && !req.Until.IsZero() && req.Since.After(req.Until) {
		return fmt.Errorf("since time cannot be after until time")
	}

	if req.Limit < 0 {
		return fmt.Errorf("limit cannot be negative")
	}

	if req.Limit > 10000 {
		return fmt.Errorf("limit cannot exceed 10000")
	}

	return nil
}
