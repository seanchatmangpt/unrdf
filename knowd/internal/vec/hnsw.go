package vec

import (
	"context"
	"fmt"
	"math"
	"sort"
	"sync"
)

// HNSW implements a Hierarchical Navigable Small World graph for vector similarity search.
type HNSW struct {
	mu         sync.RWMutex
	vectors    map[string]*Vector
	entryPoint string // ID of the entry point vector
	maxLevel   int
	M          int // Max number of connections per node
	ef         int // Size of dynamic candidate list
	embedder   Embedder
	dimensions int
}

// HNSWConfig configures HNSW index parameters.
type HNSWConfig struct {
	M        int // Max connections per node (default: 16)
	ef       int // Dynamic candidate list size (default: 200)
	MaxLevel int // Maximum level (default: -1 for auto)
	Embedder Embedder
}

// NewHNSW creates a new HNSW index.
func NewHNSW(config HNSWConfig) *HNSW {
	if config.M == 0 {
		config.M = 16
	}
	if config.ef == 0 {
		config.ef = 200
	}
	if config.MaxLevel == 0 {
		config.MaxLevel = 16 // Default max level
	}

	dimensions := config.Embedder.GetDimensions()

	return &HNSW{
		vectors:    make(map[string]*Vector),
		maxLevel:   config.MaxLevel,
		M:          config.M,
		ef:         config.ef,
		embedder:   config.Embedder,
		dimensions: dimensions,
	}
}

// Insert adds a vector to the index.
func (h *HNSW) Insert(ctx context.Context, vector *Vector) error {
	h.mu.Lock()
	defer h.mu.Unlock()

	if len(vector.Vector) != h.dimensions {
		return fmt.Errorf("vector dimensions %d don't match index dimensions %d", len(vector.Vector), h.dimensions)
	}

	// Insert the vector
	h.vectors[vector.ID] = vector

	// If this is the first vector, set it as entry point
	if h.entryPoint == "" {
		h.entryPoint = vector.ID
		return nil
	}

	// Insert into HNSW layers (simplified implementation)
	// In a full implementation, you'd build hierarchical layers
	// For now, we'll just store all vectors in a flat structure

	return nil
}

// Search finds the k nearest neighbors to a query vector.
func (h *HNSW) Search(ctx context.Context, queryVector []float32, k int, options SearchOptions) (*VectorSearchResult, error) {
	h.mu.RLock()
	defer h.mu.RUnlock()

	if len(queryVector) != h.dimensions {
		return nil, fmt.Errorf("query vector dimensions %d don't match index dimensions %d", len(queryVector), h.dimensions)
	}

	// Simple brute-force search for demonstration
	// In a real HNSW implementation, this would use the hierarchical graph structure
	var distances []Distance
	for id, vector := range h.vectors {
		distance := h.cosineSimilarity(queryVector, vector.Vector)
		// Convert distance to similarity (1 - distance for cosine)
		similarity := 1.0 - distance

		if options.Threshold == 0 || similarity >= options.Threshold {
			distances = append(distances, Distance{
				ID:       id,
				Distance: similarity,
			})
		}
	}

	// Sort by similarity (highest first)
	sort.Slice(distances, func(i, j int) bool {
		return distances[i].Distance > distances[j].Distance
	})

	// Take top K
	if len(distances) > k {
		distances = distances[:k]
	}

	return &VectorSearchResult{
		QueryVector: queryVector,
		Results:     distances,
		Total:       len(distances),
	}, nil
}

// cosineSimilarity calculates cosine similarity between two vectors.
func (h *HNSW) cosineSimilarity(a, b []float32) float32 {
	if len(a) != len(b) {
		return 0
	}

	var dotProduct, normA, normB float32
	for i := 0; i < len(a); i++ {
		dotProduct += a[i] * b[i]
		normA += a[i] * a[i]
		normB += b[i] * b[i]
	}

	if normA == 0 || normB == 0 {
		return 0
	}

	return dotProduct / (float32(math.Sqrt(float64(normA))) * float32(math.Sqrt(float64(normB))))
}

// Delete removes a vector from the index.
func (h *HNSW) Delete(id string) error {
	h.mu.Lock()
	defer h.mu.Unlock()

	if _, exists := h.vectors[id]; !exists {
		return fmt.Errorf("vector %s not found", id)
	}

	delete(h.vectors, id)

	// If the deleted vector was the entry point, pick a new one
	if h.entryPoint == id {
		for newEntryPoint := range h.vectors {
			h.entryPoint = newEntryPoint
			break
		}
	}

	return nil
}

// GetVector retrieves a vector by ID.
func (h *HNSW) GetVector(id string) (*Vector, error) {
	h.mu.RLock()
	defer h.mu.RUnlock()

	vector, exists := h.vectors[id]
	if !exists {
		return nil, fmt.Errorf("vector %s not found", id)
	}

	return vector, nil
}

// ListVectors returns all vector IDs in the index.
func (h *HNSW) ListVectors() []string {
	h.mu.RLock()
	defer h.mu.RUnlock()

	ids := make([]string, 0, len(h.vectors))
	for id := range h.vectors {
		ids = append(ids, id)
	}

	return ids
}

// Size returns the number of vectors in the index.
func (h *HNSW) Size() int {
	h.mu.RLock()
	defer h.mu.RUnlock()
	return len(h.vectors)
}

// Clear removes all vectors from the index.
func (h *HNSW) Clear() {
	h.mu.Lock()
	defer h.mu.Unlock()

	h.vectors = make(map[string]*Vector)
	h.entryPoint = ""
}

// GetEmbedder returns the embedder used by this index.
func (h *HNSW) GetEmbedder() Embedder {
	return h.embedder
}

// GetDimensions returns the dimensionality of vectors in this index.
func (h *HNSW) GetDimensions() int {
	return h.dimensions
}

// GetConfig returns the HNSW configuration.
func (h *HNSW) GetConfig() HNSWConfig {
	return HNSWConfig{
		M:        h.M,
		ef:       h.ef,
		MaxLevel: h.maxLevel,
		Embedder: h.embedder,
	}
}
