package vec

import (
	"context"
	"testing"
)

func TestUpsert_NewUpsert(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)
	upsert := NewUpsert(index)

	if upsert.GetIndex() != index {
		t.Error("NewUpsert() index not set correctly")
	}
}

func TestUpsert_UpsertVectors(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)
	upsert := NewUpsert(index)

	req := UpsertRequest{
		Namespace: "test-namespace",
		Vectors: []VectorUpsert{
			{
				ID:     "vec1",
				Vector: make([]float32, 128),
			},
			{
				ID:   "vec2",
				Text: "test document",
			},
		},
		Options: UpsertOptions{
			EmbedTexts: true,
		},
	}

	// Set some non-zero values for vec1
	req.Vectors[0].Vector[0] = 0.5

	response, err := upsert.UpsertVectors(context.Background(), req)
	if err != nil {
		t.Errorf("UpsertVectors() error = %v", err)
		return
	}

	if response.Upserted != 2 {
		t.Errorf("UpsertVectors() upserted = %v, want 2", response.Upserted)
	}

	if response.Skipped != 0 {
		t.Errorf("UpsertVectors() skipped = %v, want 0", response.Skipped)
	}

	// Verify vectors were inserted
	retrieved, err := index.GetVector("test-namespace", "vec1")
	if err != nil {
		t.Errorf("GetVector() error = %v", err)
		return
	}

	if retrieved.Vector[0] != 0.5 {
		t.Errorf("GetVector() vector[0] = %v, want 0.5", retrieved.Vector[0])
	}
}

func TestUpsert_UpsertFromTexts(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)
	upsert := NewUpsert(index)

	texts := map[string]string{
		"doc1": "machine learning algorithms",
		"doc2": "artificial intelligence systems",
	}

	metadata := map[string]map[string]interface{}{
		"doc1": {"category": "tech"},
		"doc2": {"category": "ai"},
	}

	response, err := upsert.UpsertFromTexts(context.Background(), "test-namespace", texts, metadata)
	if err != nil {
		t.Errorf("UpsertFromTexts() error = %v", err)
		return
	}

	if response.Upserted != 2 {
		t.Errorf("UpsertFromTexts() upserted = %v, want 2", response.Upserted)
	}

	// Verify metadata was stored
	retrieved, err := index.GetVector("test-namespace", "doc1")
	if err != nil {
		t.Errorf("GetVector() error = %v", err)
		return
	}

	if retrieved.Metadata["category"] != "tech" {
		t.Errorf("GetVector() metadata = %v, want tech", retrieved.Metadata["category"])
	}
}

func TestUpsert_UpsertFromVectors(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)
	upsert := NewUpsert(index)

	vectors := map[string][]float32{
		"vec1": make([]float32, 128),
		"vec2": make([]float32, 128),
	}

	vectors["vec1"][0] = 0.8
	vectors["vec2"][0] = 0.6

	metadata := map[string]map[string]interface{}{
		"vec1": {"type": "feature"},
		"vec2": {"type": "embedding"},
	}

	response, err := upsert.UpsertFromVectors(context.Background(), "test-namespace", vectors, metadata)
	if err != nil {
		t.Errorf("UpsertFromVectors() error = %v", err)
		return
	}

	if response.Upserted != 2 {
		t.Errorf("UpsertFromVectors() upserted = %v, want 2", response.Upserted)
	}

	// Verify vector values
	retrieved, err := index.GetVector("test-namespace", "vec1")
	if err != nil {
		t.Errorf("GetVector() error = %v", err)
		return
	}

	if retrieved.Vector[0] != 0.8 {
		t.Errorf("GetVector() vector[0] = %v, want 0.8", retrieved.Vector[0])
	}
}

func TestValidateUpsertRequest(t *testing.T) {
	tests := []struct {
		name    string
		req     UpsertRequest
		wantErr bool
	}{
		{
			name: "valid request",
			req: UpsertRequest{
				Namespace: "test",
				Vectors: []VectorUpsert{
					{ID: "vec1", Vector: make([]float32, 128)},
				},
			},
			wantErr: false,
		},
		{
			name: "missing namespace",
			req: UpsertRequest{
				Vectors: []VectorUpsert{
					{ID: "vec1", Vector: make([]float32, 128)},
				},
			},
			wantErr: true,
		},
		{
			name: "empty vectors",
			req: UpsertRequest{
				Namespace: "test",
				Vectors:   []VectorUpsert{},
			},
			wantErr: true,
		},
		{
			name: "missing vector ID",
			req: UpsertRequest{
				Namespace: "test",
				Vectors: []VectorUpsert{
					{Vector: make([]float32, 128)},
				},
			},
			wantErr: true,
		},
		{
			name: "no data provided",
			req: UpsertRequest{
				Namespace: "test",
				Vectors: []VectorUpsert{
					{ID: "vec1"},
				},
			},
			wantErr: true,
		},
		{
			name: "both text and vector provided",
			req: UpsertRequest{
				Namespace: "test",
				Vectors: []VectorUpsert{
					{ID: "vec1", Text: "text", Vector: make([]float32, 128)},
				},
			},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := ValidateUpsertRequest(tt.req)
			if (err != nil) != tt.wantErr {
				t.Errorf("ValidateUpsertRequest() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestUpsert_SetIndex(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index1 := NewIndex(config)
	index2 := NewIndex(config)

	upsert := NewUpsert(index1)

	if upsert.GetIndex() != index1 {
		t.Error("GetIndex() should return initially set index")
	}

	upsert.SetIndex(index2)

	if upsert.GetIndex() != index2 {
		t.Error("SetIndex() should update the index")
	}
}

func TestUpsert_upsertSingleVector(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)
	upsert := NewUpsert(index)

	// Create HNSW index for testing
	hnsw, err := index.GetOrCreateIndex("test-namespace")
	if err != nil {
		t.Errorf("GetOrCreateIndex() error = %v", err)
		return
	}

	// Test upserting with vector data
	vectorUpsert := VectorUpsert{
		ID:     "vec1",
		Vector: make([]float32, 128),
	}

	vectorUpsert.Vector[0] = 0.7

	err = upsert.upsertSingleVector(context.Background(), hnsw, vectorUpsert, UpsertOptions{
		EmbedTexts: false,
	})

	if err != nil {
		t.Errorf("upsertSingleVector() error = %v", err)
		return
	}

	// Verify vector was inserted
	retrieved, err := hnsw.GetVector("vec1")
	if err != nil {
		t.Errorf("GetVector() error = %v", err)
		return
	}

	if retrieved.Vector[0] != 0.7 {
		t.Errorf("GetVector() vector[0] = %v, want 0.7", retrieved.Vector[0])
	}
}

func TestUpsert_upsertSingleVectorWithText(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)
	upsert := NewUpsert(index)

	// Create HNSW index for testing
	hnsw, err := index.GetOrCreateIndex("test-namespace")
	if err != nil {
		t.Errorf("GetOrCreateIndex() error = %v", err)
		return
	}

	// Test upserting with text data
	vectorUpsert := VectorUpsert{
		ID:   "doc1",
		Text: "test document content",
	}

	err = upsert.upsertSingleVector(context.Background(), hnsw, vectorUpsert, UpsertOptions{
		EmbedTexts: true,
	})

	if err != nil {
		t.Errorf("upsertSingleVector() with text error = %v", err)
		return
	}

	// Verify vector was inserted (it should have been embedded)
	retrieved, err := hnsw.GetVector("doc1")
	if err != nil {
		t.Errorf("GetVector() error = %v", err)
		return
	}

	if len(retrieved.Vector) != 128 {
		t.Errorf("GetVector() vector length = %v, want 128", len(retrieved.Vector))
	}
}

func TestUpsert_upsertSingleVectorNoData(t *testing.T) {
	embedder := NewHashEmbedder(128)
	config := IndexConfig{
		DefaultDimensions: 128,
		DefaultEmbedder:   embedder,
	}

	index := NewIndex(config)
	upsert := NewUpsert(index)

	// Create HNSW index for testing
	hnsw, err := index.GetOrCreateIndex("test-namespace")
	if err != nil {
		t.Errorf("GetOrCreateIndex() error = %v", err)
		return
	}

	// Test upserting with no data
	vectorUpsert := VectorUpsert{
		ID: "empty",
	}

	err = upsert.upsertSingleVector(context.Background(), hnsw, vectorUpsert, UpsertOptions{
		EmbedTexts: false,
	})

	if err == nil {
		t.Error("upsertSingleVector() should fail with no data")
	}
}
