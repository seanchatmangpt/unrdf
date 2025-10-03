// Package vec provides tests for Gonum-enhanced vector operations.
package vec

import (
	"context"
	"math"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestGonumVectorOperations tests the enhanced vector operations using Gonum library.
func TestGonumVectorOperations(t *testing.T) {
	// Create embedder and HNSW index with smaller dimensions for testing
	embedder := NewHashEmbedder(3)
	config := HNSWConfig{
		M:        16,
		ef:       200,
		MaxLevel: 16,
		Embedder: embedder,
	}
	hnsw := NewHNSW(config)

	t.Run("VectorNormalization", func(t *testing.T) {
		testVector := []float32{3.0, 4.0, 0.0, 0.0}
		normalized := hnsw.normalizeVector(testVector)

		// Check that normalization produces unit vector
		var normSquared float32
		for _, v := range normalized {
			normSquared += v * v
		}

		assert.InDelta(t, 1.0, normSquared, 0.001, "Normalized vector should have norm 1")
		assert.Equal(t, len(testVector), len(normalized), "Normalization should preserve length")
	})

	t.Run("CosineSimilarityAccuracy", func(t *testing.T) {
		// Test cosine similarity with known vectors
		a := []float32{1.0, 0.0, 0.0}
		b := []float32{1.0, 0.0, 0.0} // Same vector

		similarity := hnsw.cosineSimilarity(a, b)
		assert.InDelta(t, 1.0, similarity, 0.001, "Identical vectors should have similarity 1")

		// Orthogonal vectors
		c := []float32{0.0, 1.0, 0.0}
		similarity = hnsw.cosineSimilarity(a, c)
		assert.InDelta(t, 0.0, similarity, 0.001, "Orthogonal vectors should have similarity 0")

		// Opposite vectors
		d := []float32{-1.0, 0.0, 0.0}
		similarity = hnsw.cosineSimilarity(a, d)
		assert.InDelta(t, -1.0, similarity, 0.001, "Opposite vectors should have similarity -1")
	})

	t.Run("EuclideanDistanceAccuracy", func(t *testing.T) {
		a := []float32{0.0, 0.0, 0.0}
		b := []float32{3.0, 4.0, 0.0}

		distance := hnsw.euclideanDistance(a, b)
		assert.InDelta(t, 5.0, distance, 0.001, "Euclidean distance should be 5 for (0,0,0) to (3,4,0)")
	})

	t.Run("ManhattanDistanceAccuracy", func(t *testing.T) {
		a := []float32{1.0, 2.0}
		b := []float32{4.0, 6.0}

		distance := hnsw.manhattanDistance(a, b)
		expected := 3.0 + 4.0 // |4-1| + |6-2|
		assert.InDelta(t, expected, distance, 0.001, "Manhattan distance should be sum of absolute differences")
	})

	t.Run("VectorSearchWithGonumOptimization", func(t *testing.T) {
		ctx := context.Background()

		// Insert test vectors
		vectors := []*Vector{
			{ID: "doc1", Vector: []float32{1.0, 0.0, 0.0}, Metadata: map[string]interface{}{"title": "Document 1"}},
			{ID: "doc2", Vector: []float32{0.0, 1.0, 0.0}, Metadata: map[string]interface{}{"title": "Document 2"}},
			{ID: "doc3", Vector: []float32{0.0, 0.0, 1.0}, Metadata: map[string]interface{}{"title": "Document 3"}},
		}

		for _, vector := range vectors {
			err := hnsw.Insert(ctx, vector)
			require.NoError(t, err)
		}

		// Search for similar vector
		queryVector := []float32{1.0, 0.1, 0.0} // Close to doc1
		options := SearchOptions{TopK: 3, Threshold: 0.0}

		result, err := hnsw.Search(ctx, queryVector, 3, options)
		require.NoError(t, err)

		assert.NotEmpty(t, result.Results, "Search should return results")
		assert.Equal(t, "doc1", result.Results[0].ID, "Most similar should be doc1")
		assert.Greater(t, result.Results[0].Distance, result.Results[1].Distance, "Results should be ordered by similarity")
	})

	t.Run("IndexOperationsWithGonum", func(t *testing.T) {
		indexConfig := IndexConfig{
			DefaultDimensions: 3,
			DefaultEmbedder:   NewHashEmbedder(3),
		}
		index := NewIndex(indexConfig)

		// Test vector upsert and search (3 dimensions to match embedder)
		testVector := []float32{0.1, 0.2, 0.3}
		metadata := map[string]interface{}{"title": "Test Document"}

		ctx := context.Background()
		err := index.UpsertVector(ctx, "test-ns", "test-id", testVector, metadata)
		require.NoError(t, err)

		// Search within namespace
		queryVector := []float32{0.11, 0.21, 0.31} // Similar vector
		options := SearchOptions{TopK: 5, Threshold: 0.8}

		result, err := index.SearchVectors(ctx, "test-ns", queryVector, options)
		require.NoError(t, err)

		assert.NotEmpty(t, result.Results, "Search should find the inserted vector")

		// Test text embedding and search
		textResult, err := index.EmbedAndSearch(ctx, "test-ns", "test query", options)
		require.NoError(t, err)

		assert.NotNil(t, textResult, "Embed and search should return results")
	})

	t.Run("PerformanceComparison", func(t *testing.T) {
		// Large vectors for performance test
		largeVectorA := make([]float32, 1000)
		largeVectorB := make([]float32, 1000)

		for i := range largeVectorA {
			largeVectorA[i] = float32(i)
			largeVectorB[i] = float32(i + 1)
		}

		// Test Gonum-optimized operations
		similarity := hnsw.cosineSimilarity(largeVectorA, largeVectorB)
		distance := hnsw.euclideanDistance(largeVectorA, largeVectorB)

		assert.NotZero(t, similarity, "Cosine similarity should be calculated")
		assert.NotZero(t, distance, "Euclidean distance should be calculated")

		// Test normalization performance
		normalized := hnsw.normalizeVector(largeVectorA)
		assert.Equal(t, len(largeVectorA), len(normalized), "Normalization should preserve vector length")

		// Verify normalization created unit vector
		var norm float32
		for _, v := range normalized {
			norm += v * v
		}
		assert.InDelta(t, 1.0, norm, 0.001, "Normalized large vector should have unit norm")
	})

	t.Run("ErrorHandling", func(t *testing.T) {
		// Test dimension mismatch
		a := []float32{1.0, 2.0}
		b := []float32{1.0, 2.0, 3.0}

		similarity := hnsw.cosineSimilarity(a, b)
		assert.Equal(t, float32(0), similarity, "Dimension mismatch should return 0 similarity")

		distance := hnsw.euclideanDistance(a, b)
		assert.Equal(t, float32(math.MaxFloat32), distance, "Dimension mismatch should return max distance")

		// Test empty vector normalization
		empty := hnsw.normalizeVector([]float32{})
		assert.Empty(t, empty, "Empty vector normalization should return empty vector")

		zero := hnsw.normalizeVector([]float32{0.0, 0.0, 0.0})
		assert.Equal(t, []float32{0.0, 0.0, 0.0}, zero, "Zero vector normalization should return original")
	})
}

// BenchmarkVectorOperations benchmarks the Gonum-enhanced vector operations.
func BenchmarkVectorOperations(b *testing.B) {
	embedder := NewHashEmbedder(384)
	config := HNSWConfig{
		M:        16,
		ef:       200,
		MaxLevel: 16,
		Embedder: embedder,
	}
	hnsw := NewHNSW(config)

	// Create test vectors
	vectorA := make([]float32, 384)
	vectorB := make([]float32, 384)
	for i := range vectorA {
		vectorA[i] = float32(i) / 384.0
		vectorB[i] = float32(i+1) / 384.0
	}

	b.Run("CosineSimilarity", func(b *testing.B) {
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			hnsw.cosineSimilarity(vectorA, vectorB)
		}
	})

	b.Run("EuclideanDistance", func(b *testing.B) {
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			hnsw.euclideanDistance(vectorA, vectorB)
		}
	})

	b.Run("VectorNormalization", func(b *testing.B) {
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			hnsw.normalizeVector(vectorA)
		}
	})
}
