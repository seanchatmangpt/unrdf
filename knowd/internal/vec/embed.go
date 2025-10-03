package vec

import (
	"context"
	"fmt"
)

// Embedder is an interface for embedding text into vectors.
type Embedder interface {
	// EmbedText converts text into a vector representation.
	EmbedText(ctx context.Context, text string) ([]float32, error)

	// EmbedTexts converts multiple texts into vector representations.
	EmbedTexts(ctx context.Context, texts []string) ([][]float32, error)

	// GetDimensions returns the dimensionality of the embedding vectors.
	GetDimensions() int

	// GetModelName returns the name/identifier of the embedding model.
	GetModelName() string
}

// HashEmbedder is a simple hash-based embedder for testing/demonstration.
type HashEmbedder struct {
	dimensions int
}

// NewHashEmbedder creates a new hash-based embedder.
func NewHashEmbedder(dimensions int) *HashEmbedder {
	return &HashEmbedder{
		dimensions: dimensions,
	}
}

// EmbedText implements Embedder for HashEmbedder.
func (h *HashEmbedder) EmbedText(ctx context.Context, text string) ([]float32, error) {
	vector := make([]float32, h.dimensions)

	// Simple hash-based embedding for demonstration
	for i := 0; i < h.dimensions; i++ {
		hash := 0
		for j, char := range text {
			hash = hash*31 + int(char) + i + j
		}
		// Normalize to [0, 1] range
		vector[i] = float32(hash%1000) / 1000.0
	}

	return vector, nil
}

// EmbedTexts implements Embedder for HashEmbedder.
func (h *HashEmbedder) EmbedTexts(ctx context.Context, texts []string) ([][]float32, error) {
	vectors := make([][]float32, len(texts))
	for i, text := range texts {
		vector, err := h.EmbedText(ctx, text)
		if err != nil {
			return nil, fmt.Errorf("failed to embed text %d: %w", i, err)
		}
		vectors[i] = vector
	}
	return vectors, nil
}

// GetDimensions implements Embedder for HashEmbedder.
func (h *HashEmbedder) GetDimensions() int {
	return h.dimensions
}

// GetModelName implements Embedder for HashEmbedder.
func (h *HashEmbedder) GetModelName() string {
	return fmt.Sprintf("hash-embedder-%d", h.dimensions)
}

// Vector represents a vector in the embedding space.
type Vector struct {
	ID       string                 `json:"id"`
	Vector   []float32              `json:"vector"`
	Metadata map[string]interface{} `json:"metadata,omitempty"`
}

// Distance represents the distance between two vectors.
type Distance struct {
	ID       string  `json:"id"`
	Distance float32 `json:"distance"`
}

// VectorSearchResult represents the result of a vector search.
type VectorSearchResult struct {
	QueryVector []float32  `json:"query_vector"`
	Results     []Distance `json:"results"`
	Total       int        `json:"total"`
}

// SearchOptions configures vector search parameters.
type SearchOptions struct {
	TopK          int     `json:"top_k"`          // Number of results to return
	Threshold     float32 `json:"threshold"`      // Minimum similarity threshold
	IncludeVector bool    `json:"include_vector"` // Include vectors in results
}
