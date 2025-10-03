package vec

import (
	"context"
	"encoding/json"
	"fmt"
	"sync"
)

// Index manages vector indices for different namespaces.
type Index struct {
	mu      sync.RWMutex
	indices map[string]*HNSW // namespace -> HNSW index
	config  IndexConfig
}

// IndexConfig configures the vector index manager.
type IndexConfig struct {
	DefaultDimensions int      // Default vector dimensions
	DefaultEmbedder   Embedder // Default embedder to use
}

// NewIndex creates a new vector index manager.
func NewIndex(config IndexConfig) *Index {
	if config.DefaultDimensions == 0 {
		config.DefaultDimensions = 384 // Default to common embedding size
	}
	if config.DefaultEmbedder == nil {
		config.DefaultEmbedder = NewHashEmbedder(config.DefaultDimensions)
	}

	return &Index{
		indices: make(map[string]*HNSW),
		config:  config,
	}
}

// GetOrCreateIndex gets an existing index for a namespace or creates a new one.
func (i *Index) GetOrCreateIndex(namespace string) (*HNSW, error) {
	i.mu.Lock()
	defer i.mu.Unlock()

	if index, exists := i.indices[namespace]; exists {
		return index, nil
	}

	// Create new HNSW index for this namespace
	hnswConfig := HNSWConfig{
		M:        16,
		ef:       200,
		MaxLevel: 16,
		Embedder: i.config.DefaultEmbedder,
	}

	index := NewHNSW(hnswConfig)
	i.indices[namespace] = index

	return index, nil
}

// GetIndex gets an index for a specific namespace.
func (i *Index) GetIndex(namespace string) (*HNSW, error) {
	i.mu.RLock()
	defer i.mu.RUnlock()

	index, exists := i.indices[namespace]
	if !exists {
		return nil, fmt.Errorf("no vector index found for namespace %s", namespace)
	}

	return index, nil
}

// DeleteIndex removes an index for a namespace.
func (i *Index) DeleteIndex(namespace string) error {
	i.mu.Lock()
	defer i.mu.Unlock()

	if _, exists := i.indices[namespace]; !exists {
		return fmt.Errorf("no vector index found for namespace %s", namespace)
	}

	delete(i.indices, namespace)
	return nil
}

// ListNamespaces returns all namespaces with vector indices.
func (i *Index) ListNamespaces() []string {
	i.mu.RLock()
	defer i.mu.RUnlock()

	namespaces := make([]string, 0, len(i.indices))
	for namespace := range i.indices {
		namespaces = append(namespaces, namespace)
	}

	return namespaces
}

// UpsertVector adds or updates a vector in the index for a namespace.
func (i *Index) UpsertVector(ctx context.Context, namespace, id string, vector []float32, metadata map[string]interface{}) error {
	index, err := i.GetOrCreateIndex(namespace)
	if err != nil {
		return fmt.Errorf("failed to get index for namespace %s: %w", namespace, err)
	}

	vectorObj := &Vector{
		ID:       id,
		Vector:   vector,
		Metadata: metadata,
	}

	return index.Insert(ctx, vectorObj)
}

// SearchVectors searches for similar vectors in a namespace.
func (i *Index) SearchVectors(ctx context.Context, namespace string, queryVector []float32, options SearchOptions) (*VectorSearchResult, error) {
	index, err := i.GetIndex(namespace)
	if err != nil {
		return nil, fmt.Errorf("failed to get index for namespace %s: %w", namespace, err)
	}

	return index.Search(ctx, queryVector, options.TopK, options)
}

// EmbedAndSearch embeds text and searches for similar vectors.
func (i *Index) EmbedAndSearch(ctx context.Context, namespace, text string, options SearchOptions) (*VectorSearchResult, error) {
	index, err := i.GetIndex(namespace)
	if err != nil {
		return nil, fmt.Errorf("failed to get index for namespace %s: %w", namespace, err)
	}

	// Embed the query text
	queryVector, err := index.GetEmbedder().EmbedText(ctx, text)
	if err != nil {
		return nil, fmt.Errorf("failed to embed query text: %w", err)
	}

	return index.Search(ctx, queryVector, options.TopK, options)
}

// GetVector retrieves a specific vector from a namespace.
func (i *Index) GetVector(namespace, id string) (*Vector, error) {
	index, err := i.GetIndex(namespace)
	if err != nil {
		return nil, fmt.Errorf("failed to get index for namespace %s: %w", namespace, err)
	}

	return index.GetVector(id)
}

// DeleteVector removes a vector from a namespace.
func (i *Index) DeleteVector(namespace, id string) error {
	index, err := i.GetIndex(namespace)
	if err != nil {
		return fmt.Errorf("failed to get index for namespace %s: %w", namespace, err)
	}

	return index.Delete(id)
}

// GetIndexStats returns statistics for a namespace index.
func (i *Index) GetIndexStats(namespace string) (map[string]interface{}, error) {
	index, err := i.GetIndex(namespace)
	if err != nil {
		return nil, fmt.Errorf("failed to get index for namespace %s: %w", namespace, err)
	}

	stats := map[string]interface{}{
		"size":       index.Size(),
		"dimensions": index.GetDimensions(),
		"embedder":   index.GetEmbedder().GetModelName(),
		"config":     index.GetConfig(),
	}

	return stats, nil
}

// GetAllStats returns statistics for all namespace indices.
func (i *Index) GetAllStats() map[string]interface{} {
	i.mu.RLock()
	defer i.mu.RUnlock()

	stats := make(map[string]interface{})
	for namespace, index := range i.indices {
		namespaceStats := map[string]interface{}{
			"size":       index.Size(),
			"dimensions": index.GetDimensions(),
			"embedder":   index.GetEmbedder().GetModelName(),
		}
		stats[namespace] = namespaceStats
	}

	return stats
}

// SerializeIndex serializes an index to JSON (for persistence).
func (i *Index) SerializeIndex(namespace string) ([]byte, error) {
	index, err := i.GetIndex(namespace)
	if err != nil {
		return nil, fmt.Errorf("failed to get index for namespace %s: %w", namespace, err)
	}

	i.mu.RLock()
	defer i.mu.RUnlock()

	// Convert vectors to serializable format
	vectors := make([]Vector, 0, len(index.vectors))
	for _, vector := range index.vectors {
		vectors = append(vectors, *vector)
	}

	data := map[string]interface{}{
		"namespace":  namespace,
		"vectors":    vectors,
		"dimensions": index.dimensions,
		"embedder":   index.embedder.GetModelName(),
		"config":     index.GetConfig(),
	}

	return json.Marshal(data)
}

// DeserializeIndex deserializes an index from JSON.
func (i *Index) DeserializeIndex(data []byte) error {
	var serialized map[string]interface{}
	if err := json.Unmarshal(data, &serialized); err != nil {
		return fmt.Errorf("failed to unmarshal index data: %w", err)
	}

	namespace, ok := serialized["namespace"].(string)
	if !ok {
		return fmt.Errorf("invalid namespace in serialized data")
	}

	vectorsData, ok := serialized["vectors"].([]interface{})
	if !ok {
		return fmt.Errorf("invalid vectors in serialized data")
	}

	// Create embedder for this index
	embedderName, _ := serialized["embedder"].(string)
	var embedder Embedder
	if embedderName == "" || embedderName == "hash-embedder-384" {
		embedder = NewHashEmbedder(384)
	} else {
		embedder = NewHashEmbedder(384) // Default fallback
	}

	// Create HNSW index
	hnswConfig := HNSWConfig{
		M:        16,
		ef:       200,
		MaxLevel: 16,
		Embedder: embedder,
	}

	index := NewHNSW(hnswConfig)

	// Add vectors
	for _, vectorData := range vectorsData {
		vectorMap, ok := vectorData.(map[string]interface{})
		if !ok {
			continue
		}

		id, _ := vectorMap["id"].(string)
		vectorInterface, _ := vectorMap["vector"].([]interface{})
		metadata, _ := vectorMap["metadata"].(map[string]interface{})

		if id == "" {
			continue
		}

		// Convert vector to float32 slice
		vector := make([]float32, len(vectorInterface))
		for j, v := range vectorInterface {
			if val, ok := v.(float64); ok {
				vector[j] = float32(val)
			}
		}

		vectorObj := &Vector{
			ID:       id,
			Vector:   vector,
			Metadata: metadata,
		}

		index.Insert(context.Background(), vectorObj)
	}

	i.mu.Lock()
	i.indices[namespace] = index
	i.mu.Unlock()

	return nil
}

// ClearAll removes all indices.
func (i *Index) ClearAll() {
	i.mu.Lock()
	defer i.mu.Unlock()

	i.indices = make(map[string]*HNSW)
}

// GetConfig returns the index manager configuration.
func (i *Index) GetConfig() IndexConfig {
	return i.config
}
