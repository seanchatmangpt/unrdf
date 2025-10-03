package vec

import (
	"context"
	"testing"
)

func TestIndex_NewIndex(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)

	if index.config.DefaultDimensions != 128 {
		t.Errorf("NewIndex() dimensions = %v, want 128", index.config.DefaultDimensions)
	}

	if len(index.ListNamespaces()) != 0 {
		t.Errorf("NewIndex() namespaces = %v, want 0", len(index.ListNamespaces()))
	}
}

func TestIndex_GetOrCreateIndex(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)

	// First call should create the index
	hnsw1, err := index.GetOrCreateIndex("test-namespace")
	if err != nil {
		t.Errorf("GetOrCreateIndex() error = %v", err)
		return
	}

	if hnsw1.GetDimensions() != 128 {
		t.Errorf("GetOrCreateIndex() dimensions = %v, want 128", hnsw1.GetDimensions())
	}

	// Second call should return the same index
	hnsw2, err := index.GetOrCreateIndex("test-namespace")
	if err != nil {
		t.Errorf("GetOrCreateIndex() error = %v", err)
		return
	}

	if hnsw1 != hnsw2 {
		t.Error("GetOrCreateIndex() should return same instance for same namespace")
	}

	// Different namespace should create different index
	hnsw3, err := index.GetOrCreateIndex("other-namespace")
	if err != nil {
		t.Errorf("GetOrCreateIndex() error = %v", err)
		return
	}

	if hnsw1 == hnsw3 {
		t.Error("GetOrCreateIndex() should return different instances for different namespaces")
	}
}

func TestIndex_GetIndex(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)

	// Getting index for non-existent namespace should fail
	_, err := index.GetIndex("non-existent")
	if err == nil {
		t.Error("GetIndex() should fail for non-existent namespace")
	}

	// Create index first
	_, err = index.GetOrCreateIndex("test-namespace")
	if err != nil {
		t.Errorf("GetOrCreateIndex() error = %v", err)
		return
	}

	// Now getting should succeed
	hnsw, err := index.GetIndex("test-namespace")
	if err != nil {
		t.Errorf("GetIndex() error = %v", err)
		return
	}

	if hnsw == nil {
		t.Error("GetIndex() returned nil")
	}
}

func TestIndex_DeleteIndex(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)

	// Create index
	_, err := index.GetOrCreateIndex("test-namespace")
	if err != nil {
		t.Errorf("GetOrCreateIndex() error = %v", err)
		return
	}

	// Delete index
	err = index.DeleteIndex("test-namespace")
	if err != nil {
		t.Errorf("DeleteIndex() error = %v", err)
		return
	}

	// Getting deleted index should fail
	_, err = index.GetIndex("test-namespace")
	if err == nil {
		t.Error("GetIndex() should fail for deleted namespace")
	}
}

func TestIndex_ListNamespaces(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)

	// Initially empty
	namespaces := index.ListNamespaces()
	if len(namespaces) != 0 {
		t.Errorf("ListNamespaces() = %v, want 0", len(namespaces))
	}

	// Create some indices
	index.GetOrCreateIndex("ns1")
	index.GetOrCreateIndex("ns2")
	index.GetOrCreateIndex("ns3")

	namespaces = index.ListNamespaces()
	if len(namespaces) != 3 {
		t.Errorf("ListNamespaces() = %v, want 3", len(namespaces))
	}
}

func TestIndex_UpsertVector(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)

	vector := make([]float32, 128)
	vector[0] = 0.5

	err := index.UpsertVector(context.Background(), "test-namespace", "vec1", vector, map[string]interface{}{
		"key": "value",
	})

	if err != nil {
		t.Errorf("UpsertVector() error = %v", err)
		return
	}

	// Verify vector was inserted
	retrieved, err := index.GetVector("test-namespace", "vec1")
	if err != nil {
		t.Errorf("GetVector() error = %v", err)
		return
	}

	if retrieved.ID != "vec1" {
		t.Errorf("GetVector() ID = %v, want vec1", retrieved.ID)
	}

	if retrieved.Metadata["key"] != "value" {
		t.Errorf("GetVector() metadata = %v, want value", retrieved.Metadata["key"])
	}
}

func TestIndex_SearchVectors(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)

	// Insert test vectors
	vectors := map[string][]float32{
		"vec1": make([]float32, 128),
		"vec2": make([]float32, 128),
		"vec3": make([]float32, 128),
	}

	vectors["vec1"][0] = 1.0
	vectors["vec2"][0] = 0.8
	vectors["vec3"][0] = 0.6

	for id, vector := range vectors {
		index.UpsertVector(context.Background(), "test-namespace", id, vector, nil)
	}

	// Search
	queryVector := make([]float32, 128)
	queryVector[0] = 0.9

	options := SearchOptions{
		TopK: 2,
	}

	result, err := index.SearchVectors(context.Background(), "test-namespace", queryVector, options)
	if err != nil {
		t.Errorf("SearchVectors() error = %v", err)
		return
	}

	if len(result.Results) != 2 {
		t.Errorf("SearchVectors() results = %v, want 2", len(result.Results))
	}

	// Should find vec2 as most similar (0.8 vs 0.9)
	if result.Results[0].ID != "vec2" {
		t.Errorf("SearchVectors() first result = %v, want vec2", result.Results[0].ID)
	}
}

func TestIndex_EmbedAndSearch(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)

	// Insert test vectors from text
	texts := map[string]string{
		"doc1": "machine learning algorithms",
		"doc2": "artificial intelligence systems",
		"doc3": "computer science fundamentals",
	}

	for id, text := range texts {
		vector := make([]float32, 128)
		// Simulate embedding by setting some values
		vector[0] = float32(len(text)) / 100.0
		index.UpsertVector(context.Background(), "test-namespace", id, vector, map[string]interface{}{
			"text": text,
		})
	}

	// Search using text embedding
	options := SearchOptions{
		TopK: 2,
	}

	result, err := index.EmbedAndSearch(context.Background(), "test-namespace", "machine learning", options)
	if err != nil {
		t.Errorf("EmbedAndSearch() error = %v", err)
		return
	}

	if len(result.Results) != 2 {
		t.Errorf("EmbedAndSearch() results = %v, want 2", len(result.Results))
	}
}

func TestIndex_DeleteVector(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)

	// Insert vector
	vector := make([]float32, 128)
	vector[0] = 0.5

	err := index.UpsertVector(context.Background(), "test-namespace", "vec1", vector, nil)
	if err != nil {
		t.Errorf("UpsertVector() error = %v", err)
		return
	}

	// Delete vector
	err = index.DeleteVector("test-namespace", "vec1")
	if err != nil {
		t.Errorf("DeleteVector() error = %v", err)
		return
	}

	// Verify deletion
	_, err = index.GetVector("test-namespace", "vec1")
	if err == nil {
		t.Error("GetVector() should fail for deleted vector")
	}
}

func TestIndex_GetIndexStats(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)

	// Insert some vectors
	vectors := map[string][]float32{
		"vec1": make([]float32, 128),
		"vec2": make([]float32, 128),
	}

	for id, vector := range vectors {
		index.UpsertVector(context.Background(), "test-namespace", id, vector, nil)
	}

	stats, err := index.GetIndexStats("test-namespace")
	if err != nil {
		t.Errorf("GetIndexStats() error = %v", err)
		return
	}

	size, ok := stats["size"].(int)
	if !ok || size != 2 {
		t.Errorf("GetIndexStats() size = %v, want 2", stats["size"])
	}

	dimensions, ok := stats["dimensions"].(int)
	if !ok || dimensions != 128 {
		t.Errorf("GetIndexStats() dimensions = %v, want 128", stats["dimensions"])
	}
}

func TestIndex_ClearAll(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)

	// Create indices in different namespaces
	index.GetOrCreateIndex("ns1")
	index.GetOrCreateIndex("ns2")

	if len(index.ListNamespaces()) != 2 {
		t.Errorf("ListNamespaces() = %v, want 2", len(index.ListNamespaces()))
	}

	index.ClearAll()

	if len(index.ListNamespaces()) != 0 {
		t.Errorf("ClearAll() namespaces = %v, want 0", len(index.ListNamespaces()))
	}
}
