package vec

import (
	"context"
	"testing"
)

func TestHashEmbedder_EmbedText(t *testing.T) {
	embedder := NewHashEmbedder(384)

	vector, err := embedder.EmbedText(context.Background(), "test text")
	if err != nil {
		t.Errorf("EmbedText() error = %v", err)
		return
	}

	if len(vector) != 384 {
		t.Errorf("EmbedText() vector length = %v, want 384", len(vector))
	}

	// Check that all values are in [0, 1] range
	for i, v := range vector {
		if v < 0 || v > 1 {
			t.Errorf("EmbedText() vector[%d] = %v, not in [0, 1] range", i, v)
		}
	}
}

func TestHashEmbedder_EmbedTexts(t *testing.T) {
	embedder := NewHashEmbedder(128)

	texts := []string{"text1", "text2", "text3"}
	vectors, err := embedder.EmbedTexts(context.Background(), texts)
	if err != nil {
		t.Errorf("EmbedTexts() error = %v", err)
		return
	}

	if len(vectors) != 3 {
		t.Errorf("EmbedTexts() vectors length = %v, want 3", len(vectors))
	}

	for i, vector := range vectors {
		if len(vector) != 128 {
			t.Errorf("EmbedTexts() vector[%d] length = %v, want 128", i, len(vector))
		}
	}
}

func TestHashEmbedder_GetDimensions(t *testing.T) {
	embedder := NewHashEmbedder(256)
	if embedder.GetDimensions() != 256 {
		t.Errorf("GetDimensions() = %v, want 256", embedder.GetDimensions())
	}
}

func TestHashEmbedder_GetModelName(t *testing.T) {
	embedder := NewHashEmbedder(384)
	name := embedder.GetModelName()
	if name != "hash-embedder-384" {
		t.Errorf("GetModelName() = %v, want hash-embedder-384", name)
	}
}

func TestHashEmbedder_Consistency(t *testing.T) {
	embedder := NewHashEmbedder(128)

	text := "consistent test text"

	vector1, err := embedder.EmbedText(context.Background(), text)
	if err != nil {
		t.Errorf("EmbedText() error = %v", err)
		return
	}

	vector2, err := embedder.EmbedText(context.Background(), text)
	if err != nil {
		t.Errorf("EmbedText() error = %v", err)
		return
	}

	// Same input should produce same output
	for i := range vector1 {
		if vector1[i] != vector2[i] {
			t.Errorf("EmbedText() inconsistent results for same input")
			break
		}
	}
}

func TestHashEmbedder_DifferentInputs(t *testing.T) {
	embedder := NewHashEmbedder(128)

	vector1, err := embedder.EmbedText(context.Background(), "text1")
	if err != nil {
		t.Errorf("EmbedText() error = %v", err)
		return
	}

	vector2, err := embedder.EmbedText(context.Background(), "text2")
	if err != nil {
		t.Errorf("EmbedText() error = %v", err)
		return
	}

	// Different inputs should produce different outputs (with high probability)
	different := false
	for i := range vector1 {
		if vector1[i] != vector2[i] {
			different = true
			break
		}
	}

	if !different {
		t.Error("EmbedText() produced same vectors for different inputs")
	}
}
