package vec

import (
	"context"
	"testing"
)

func TestHNSW_NewHNSW(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := HNSWConfig{
		M:        16,
		ef:       200,
		MaxLevel: 16,
		Embedder: embedder,
	}

	hnsw := NewHNSW(config)

	if hnsw.M != 16 {
		t.Errorf("NewHNSW() M = %v, want 16", hnsw.M)
	}

	if hnsw.ef != 200 {
		t.Errorf("NewHNSW() ef = %v, want 200", hnsw.ef)
	}

	if hnsw.maxLevel != 16 {
		t.Errorf("NewHNSW() maxLevel = %v, want 16", hnsw.maxLevel)
	}

	if hnsw.dimensions != 128 {
		t.Errorf("NewHNSW() dimensions = %v, want 128", hnsw.dimensions)
	}

	if hnsw.Size() != 0 {
		t.Errorf("NewHNSW() size = %v, want 0", hnsw.Size())
	}
}

func TestHNSW_Insert(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := HNSWConfig{
		M:        16,
		ef:       200,
		MaxLevel: 16,
		Embedder: embedder,
	}

	hnsw := NewHNSW(config)

	vector := &Vector{
		ID:     "test-vector",
		Vector: make([]float32, 128),
	}

	// Set some non-zero values
	vector.Vector[0] = 0.5
	vector.Vector[1] = 0.3

	err := hnsw.Insert(context.Background(), vector)
	if err != nil {
		t.Errorf("Insert() error = %v", err)
		return
	}

	if hnsw.Size() != 1 {
		t.Errorf("Insert() size = %v, want 1", hnsw.Size())
	}

	retrieved, err := hnsw.GetVector("test-vector")
	if err != nil {
		t.Errorf("GetVector() error = %v", err)
		return
	}

	if retrieved.ID != "test-vector" {
		t.Errorf("GetVector() ID = %v, want test-vector", retrieved.ID)
	}
}

func TestHNSW_InsertWrongDimensions(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := HNSWConfig{
		M:        16,
		ef:       200,
		MaxLevel: 16,
		Embedder: embedder,
	}

	hnsw := NewHNSW(config)

	vector := &Vector{
		ID:     "wrong-dimensions",
		Vector: make([]float32, 64), // Wrong dimensions
	}

	err := hnsw.Insert(context.Background(), vector)
	if err == nil {
		t.Error("Insert() should have failed with wrong dimensions")
	}
}

func TestHNSW_Delete(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := HNSWConfig{
		M:        16,
		ef:       200,
		MaxLevel: 16,
		Embedder: embedder,
	}

	hnsw := NewHNSW(config)

	vector := &Vector{
		ID:     "to-delete",
		Vector: make([]float32, 128),
	}

	hnsw.Insert(context.Background(), vector)

	if hnsw.Size() != 1 {
		t.Errorf("Insert() size = %v, want 1", hnsw.Size())
	}

	err := hnsw.Delete("to-delete")
	if err != nil {
		t.Errorf("Delete() error = %v", err)
		return
	}

	if hnsw.Size() != 0 {
		t.Errorf("Delete() size = %v, want 0", hnsw.Size())
	}

	_, err = hnsw.GetVector("to-delete")
	if err == nil {
		t.Error("GetVector() should have failed for deleted vector")
	}
}

func TestHNSW_DeleteNonExistent(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := HNSWConfig{
		M:        16,
		ef:       200,
		MaxLevel: 16,
		Embedder: embedder,
	}

	hnsw := NewHNSW(config)

	err := hnsw.Delete("non-existent")
	if err == nil {
		t.Error("Delete() should have failed for non-existent vector")
	}
}

func TestHNSW_Search(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := HNSWConfig{
		M:        16,
		ef:       200,
		MaxLevel: 16,
		Embedder: embedder,
	}

	hnsw := NewHNSW(config)

	// Insert test vectors
	vectors := []*Vector{
		{ID: "vec1", Vector: make([]float32, 128)},
		{ID: "vec2", Vector: make([]float32, 128)},
		{ID: "vec3", Vector: make([]float32, 128)},
	}

	// Set different values to ensure different similarity scores
	vectors[0].Vector[0] = 1.0
	vectors[1].Vector[0] = 0.8
	vectors[2].Vector[0] = 0.6

	for _, vector := range vectors {
		hnsw.Insert(context.Background(), vector)
	}

	// Search for similar vectors
	queryVector := make([]float32, 128)
	queryVector[0] = 0.9 // Should be most similar to vec2

	options := SearchOptions{
		TopK: 2,
	}

	result, err := hnsw.Search(context.Background(), queryVector, options.TopK, options)
	if err != nil {
		t.Errorf("Search() error = %v", err)
		return
	}

	if len(result.Results) != 2 {
		t.Errorf("Search() results = %v, want 2", len(result.Results))
	}

	// The most similar should be vec2 (0.8 vs 0.9)
	if result.Results[0].ID != "vec2" {
		t.Errorf("Search() first result = %v, want vec2", result.Results[0].ID)
	}
}

func TestHNSW_CosineSimilarity(t *testing.T) {
	hnsw := &HNSW{}

	// Test identical vectors
	a := []float32{1.0, 0.0, 0.0}
	b := []float32{1.0, 0.0, 0.0}
	similarity := hnsw.cosineSimilarity(a, b)
	if similarity != 1.0 {
		t.Errorf("cosineSimilarity() identical = %v, want 1.0", similarity)
	}

	// Test orthogonal vectors
	c := []float32{1.0, 0.0}
	d := []float32{0.0, 1.0}
	similarity = hnsw.cosineSimilarity(c, d)
	if similarity != 0.0 {
		t.Errorf("cosineSimilarity() orthogonal = %v, want 0.0", similarity)
	}

	// Test opposite vectors
	e := []float32{1.0, 0.0}
	f := []float32{-1.0, 0.0}
	similarity = hnsw.cosineSimilarity(e, f)
	if similarity != -1.0 {
		t.Errorf("cosineSimilarity() opposite = %v, want -1.0", similarity)
	}
}

func TestHNSW_Clear(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := HNSWConfig{
		M:        16,
		ef:       200,
		MaxLevel: 16,
		Embedder: embedder,
	}

	hnsw := NewHNSW(config)

	// Insert some vectors
	vectors := []*Vector{
		{ID: "vec1", Vector: make([]float32, 128)},
		{ID: "vec2", Vector: make([]float32, 128)},
	}

	for _, vector := range vectors {
		hnsw.Insert(context.Background(), vector)
	}

	if hnsw.Size() != 2 {
		t.Errorf("Insert() size = %v, want 2", hnsw.Size())
	}

	hnsw.Clear()

	if hnsw.Size() != 0 {
		t.Errorf("Clear() size = %v, want 0", hnsw.Size())
	}
}

func TestHNSW_ListVectors(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := HNSWConfig{
		M:        16,
		ef:       200,
		MaxLevel: 16,
		Embedder: embedder,
	}

	hnsw := NewHNSW(config)

	// Insert test vectors
	vectors := []*Vector{
		{ID: "vec1", Vector: make([]float32, 128)},
		{ID: "vec2", Vector: make([]float32, 128)},
	}

	for _, vector := range vectors {
		hnsw.Insert(context.Background(), vector)
	}

	ids := hnsw.ListVectors()
	if len(ids) != 2 {
		t.Errorf("ListVectors() = %v, want 2", len(ids))
	}

	// Check that both IDs are present
	idSet := make(map[string]bool)
	for _, id := range ids {
		idSet[id] = true
	}

	if !idSet["vec1"] || !idSet["vec2"] {
		t.Error("ListVectors() missing expected vectors")
	}
}
