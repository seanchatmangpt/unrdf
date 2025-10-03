// Package receipts provides in-memory indexing and search for transaction receipts.
package receipts

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/unrdf/knowd/internal/types"
)

// ReceiptIndex provides fast lookup and search for receipts.
type ReceiptIndex struct {
	mu       sync.RWMutex
	receipts map[string]*types.Receipt // receipt ID -> receipt
	byActor  map[string][]string      // actor -> receipt IDs
	byTime   []string                 // receipt IDs sorted by time
	byTag    map[string][]string      // tag -> receipt IDs
}

// NewReceiptIndex creates a new receipt index.
func NewReceiptIndex() *ReceiptIndex {
	return &ReceiptIndex{
		receipts: make(map[string]*types.Receipt),
		byActor:  make(map[string][]string),
		byTime:   make([]string, 0),
		byTag:    make(map[string][]string),
	}
}

// AddReceipt adds a receipt to the index.
func (ri *ReceiptIndex) AddReceipt(receipt *types.Receipt) error {
	if receipt == nil {
		return fmt.Errorf("receipt cannot be nil")
	}

	if receipt.ReceiptID == "" {
		return fmt.Errorf("receipt ID cannot be empty")
	}

	ri.mu.Lock()
	defer ri.mu.Unlock()

	// Check if already exists
	if _, exists := ri.receipts[receipt.ReceiptID]; exists {
		return fmt.Errorf("receipt %s already exists", receipt.ReceiptID)
	}

	// Add to main index
	ri.receipts[receipt.ReceiptID] = receipt

	// Add to actor index
	actor := receipt.Actor
	if actor != "" {
		ri.byActor[actor] = append(ri.byActor[actor], receipt.ReceiptID)
	}

	// Add to time index (maintain sorted order)
	ri.insertIntoTimeIndex(receipt.ReceiptID, receipt.Timestamp)

	// Add to tag index (if tags exist in receipt)
	// Note: This assumes receipt has a Tags field; adjust as needed
	// ri.addToTagIndex(receipt.ReceiptID, receipt.Tags)

	return nil
}

// GetReceipt retrieves a receipt by ID.
func (ri *ReceiptIndex) GetReceipt(receiptID string) (*types.Receipt, bool) {
	ri.mu.RLock()
	defer ri.mu.RUnlock()

	receipt, exists := ri.receipts[receiptID]
	if !exists {
		return nil, false
	}

	// Return a copy to prevent external modification
	receiptCopy := *receipt
	return &receiptCopy, true
}

// SearchReceipts searches for receipts based on filters.
func (ri *ReceiptIndex) SearchReceipts(ctx context.Context, filters ReceiptFilters) ([]*types.Receipt, error) {
	ri.mu.RLock()
	defer ri.mu.RUnlock()

	var candidates []string

	// Start with actor filter if provided
	if filters.Actor != "" {
		actorReceipts, exists := ri.byActor[filters.Actor]
		if !exists {
			return []*types.Receipt{}, nil
		}
		candidates = actorReceipts
	} else {
		// No actor filter, start with all receipts
		candidates = make([]string, 0, len(ri.receipts))
		for id := range ri.receipts {
			candidates = append(candidates, id)
		}
	}

	// Apply time filters
	if !filters.Since.IsZero() || !filters.Until.IsZero() {
		candidates = ri.filterByTime(candidates, filters.Since, filters.Until)
	}

	// Apply tag filters (if implemented)
	if len(filters.Tags) > 0 {
		candidates = ri.filterByTags(candidates, filters.Tags)
	}

	// Convert to receipts and apply limit
	var results []*types.Receipt
	for i, id := range candidates {
		if filters.Limit > 0 && i >= filters.Limit {
			break
		}

		if receipt, exists := ri.receipts[id]; exists {
			// Return a copy
			receiptCopy := *receipt
			results = append(results, &receiptCopy)
		}
	}

	return results, nil
}

// GetReceiptCount returns the total number of receipts in the index.
func (ri *ReceiptIndex) GetReceiptCount() int {
	ri.mu.RLock()
	defer ri.mu.RUnlock()
	return len(ri.receipts)
}

// ListReceipts returns all receipt IDs, optionally limited.
func (ri *ReceiptIndex) ListReceipts(limit int) []string {
	ri.mu.RLock()
	defer ri.mu.RUnlock()

	if limit <= 0 || limit > len(ri.receipts) {
		limit = len(ri.receipts)
	}

	// Return the most recent receipts first
	result := make([]string, 0, limit)
	timeIndexLen := len(ri.byTime)
	start := timeIndexLen - limit
	if start < 0 {
		start = 0
	}

	for i := start; i < timeIndexLen && len(result) < limit; i++ {
		result = append(result, ri.byTime[i])
	}

	return result
}

// ReceiptFilters defines filters for searching receipts.
type ReceiptFilters struct {
	Actor  string    `json:"actor"`
	Since  time.Time `json:"since"`
	Until  time.Time `json:"until"`
	Tags   []string  `json:"tags"`
	Limit  int       `json:"limit"`
}

// Helper methods

// insertIntoTimeIndex inserts a receipt ID into the time-sorted index.
func (ri *ReceiptIndex) insertIntoTimeIndex(receiptID string, timestamp time.Time) {
	// Find insertion point (maintain sorted order by timestamp)
	insertIndex := 0
	for i, id := range ri.byTime {
		if receipt, exists := ri.receipts[id]; exists {
			if receipt.Timestamp.After(timestamp) {
				insertIndex = i
				break
			}
		}
		insertIndex = i + 1
	}

	// Insert at the correct position
	ri.byTime = append(ri.byTime, "")
	copy(ri.byTime[insertIndex+1:], ri.byTime[insertIndex:])
	ri.byTime[insertIndex] = receiptID
}

// filterByTime filters receipt IDs by time range.
func (ri *ReceiptIndex) filterByTime(receiptIDs []string, since, until time.Time) []string {
	var filtered []string

	for _, id := range receiptIDs {
		receipt, exists := ri.receipts[id]
		if !exists {
			continue
		}

		// Check time range
		if !since.IsZero() && receipt.Timestamp.Before(since) {
			continue
		}
		if !until.IsZero() && receipt.Timestamp.After(until) {
			continue
		}

		filtered = append(filtered, id)
	}

	return filtered
}

// filterByTags filters receipt IDs by tags.
func (ri *ReceiptIndex) filterByTags(receiptIDs []string, tags []string) []string {
	// For now, return all candidates since tag indexing isn't implemented
	// In a full implementation, this would check the tag index
	return receiptIDs
}

// addToTagIndex adds a receipt to the tag index.
func (ri *ReceiptIndex) addToTagIndex(receiptID string, tags []string) {
	for _, tag := range tags {
		ri.byTag[tag] = append(ri.byTag[tag], receiptID)
	}
}