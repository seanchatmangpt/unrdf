package vec

import (
	"context"
	"fmt"
)

// Upsert manages vector upsert operations for the vector index.
type Upsert struct {
	index *Index
}

// UpsertRequest represents a request to upsert vectors.
type UpsertRequest struct {
	Namespace string                   `json:"namespace"`
	Vectors   []VectorUpsert           `json:"vectors"`
	Options   UpsertOptions           `json:"options"`
}

// VectorUpsert represents a single vector to upsert.
type VectorUpsert struct {
	ID       string                 `json:"id"`
	Text     string                 `json:"text,omitempty"`     // Text to embed
	Vector   []float32              `json:"vector,omitempty"`   // Pre-computed vector
	Metadata map[string]interface{} `json:"metadata,omitempty"`
}

// UpsertOptions configures upsert behavior.
type UpsertOptions struct {
	EmbedTexts    bool    `json:"embed_texts"`     // Whether to embed text fields
	SkipExisting  bool    `json:"skip_existing"`   // Skip vectors that already exist
	BatchSize     int     `json:"batch_size"`      // Batch size for processing
}

// UpsertResponse represents the response from an upsert operation.
type UpsertResponse struct {
	Upserted   int                    `json:"upserted"`
	Skipped    int                    `json:"skipped"`
	Errors     []string              `json:"errors"`
	Namespaces map[string]interface{} `json:"namespaces"`
}

// NewUpsert creates a new vector upsert manager.
func NewUpsert(index *Index) *Upsert {
	return &Upsert{
		index: index,
	}
}

// UpsertVectors upserts vectors into the index.
func (u *Upsert) UpsertVectors(ctx context.Context, req UpsertRequest) (*UpsertResponse, error) {
	response := &UpsertResponse{
		Namespaces: make(map[string]interface{}),
		Errors:     []string{},
	}

	// Group vectors by namespace
	namespaceVectors := make(map[string][]VectorUpsert)
	for _, vector := range req.Vectors {
		namespaceVectors[req.Namespace] = append(namespaceVectors[req.Namespace], vector)
	}

	// Process each namespace
	for namespace, vectors := range namespaceVectors {
		namespaceResponse := u.upsertNamespaceVectors(ctx, namespace, vectors, req.Options)
		response.Upserted += namespaceResponse.Upserted
		response.Skipped += namespaceResponse.Skipped
		response.Errors = append(response.Errors, namespaceResponse.Errors...)
		response.Namespaces[namespace] = map[string]interface{}{
			"upserted": namespaceResponse.Upserted,
			"skipped":  namespaceResponse.Skipped,
		}
	}

	return response, nil
}

// upsertNamespaceVectors upserts vectors for a specific namespace.
func (u *Upsert) upsertNamespaceVectors(ctx context.Context, namespace string, vectors []VectorUpsert, options UpsertOptions) *UpsertResponse {
	response := &UpsertResponse{
		Errors: []string{},
	}

	// Get or create index for namespace
	index, err := u.index.GetOrCreateIndex(namespace)
	if err != nil {
		response.Errors = append(response.Errors, fmt.Sprintf("failed to get index for namespace %s: %v", namespace, err))
		return response
	}

	// Process vectors
	for _, vectorUpsert := range vectors {
		err := u.upsertSingleVector(ctx, index, vectorUpsert, options)
		if err != nil {
			response.Errors = append(response.Errors, fmt.Sprintf("failed to upsert vector %s: %v", vectorUpsert.ID, err))
			continue
		}

		// Check if we should skip existing vectors
		if options.SkipExisting {
			if _, err := index.GetVector(vectorUpsert.ID); err == nil {
				response.Skipped++
				continue
			}
		}

		response.Upserted++
	}

	return response
}

// upsertSingleVector upserts a single vector.
func (u *Upsert) upsertSingleVector(ctx context.Context, index *HNSW, vectorUpsert VectorUpsert, options UpsertOptions) error {
	var vector []float32
	var err error

	// Determine vector data
	if len(vectorUpsert.Vector) > 0 {
		// Use provided vector
		vector = vectorUpsert.Vector
	} else if vectorUpsert.Text != "" && options.EmbedTexts {
		// Embed the text
		vector, err = index.GetEmbedder().EmbedText(ctx, vectorUpsert.Text)
		if err != nil {
			return fmt.Errorf("failed to embed text: %w", err)
		}
	} else {
		return fmt.Errorf("no vector data provided (text: %s, vector: %v, embed_texts: %v)",
			vectorUpsert.Text, len(vectorUpsert.Vector) > 0, options.EmbedTexts)
	}

	// Validate vector dimensions
	if len(vector) != index.GetDimensions() {
		return fmt.Errorf("vector dimensions %d don't match index dimensions %d", len(vector), index.GetDimensions())
	}

	// Create vector object
	vectorObj := &Vector{
		ID:       vectorUpsert.ID,
		Vector:   vector,
		Metadata: vectorUpsert.Metadata,
	}

	// Insert into index
	return index.Insert(ctx, vectorObj)
}

// UpsertFromTexts upserts vectors from text data.
func (u *Upsert) UpsertFromTexts(ctx context.Context, namespace string, texts map[string]string, metadata map[string]map[string]interface{}) (*UpsertResponse, error) {
	var vectors []VectorUpsert

	for id, text := range texts {
		vector := VectorUpsert{
			ID:       id,
			Text:     text,
			Metadata: metadata[id],
		}
		vectors = append(vectors, vector)
	}

	req := UpsertRequest{
		Namespace: namespace,
		Vectors:   vectors,
		Options: UpsertOptions{
			EmbedTexts: true,
		},
	}

	return u.UpsertVectors(ctx, req)
}

// UpsertFromVectors upserts vectors from pre-computed vector data.
func (u *Upsert) UpsertFromVectors(ctx context.Context, namespace string, vectors map[string][]float32, metadata map[string]map[string]interface{}) (*UpsertResponse, error) {
	var vectorUpserts []VectorUpsert

	for id, vector := range vectors {
		vectorUpsert := VectorUpsert{
			ID:       id,
			Vector:   vector,
			Metadata: metadata[id],
		}
		vectorUpserts = append(vectorUpserts, vectorUpsert)
	}

	req := UpsertRequest{
		Namespace: namespace,
		Vectors:   vectorUpserts,
		Options: UpsertOptions{
			EmbedTexts: false,
		},
	}

	return u.UpsertVectors(ctx, req)
}

// ValidateUpsertRequest validates an upsert request.
func ValidateUpsertRequest(req UpsertRequest) error {
	if req.Namespace == "" {
		return fmt.Errorf("namespace is required")
	}

	if len(req.Vectors) == 0 {
		return fmt.Errorf("at least one vector must be provided")
	}

	for i, vector := range req.Vectors {
		if vector.ID == "" {
			return fmt.Errorf("vector %d: ID is required", i)
		}

		// Check if either text or vector is provided
		hasText := vector.Text != ""
		hasVector := len(vector.Vector) > 0

		if !hasText && !hasVector {
			return fmt.Errorf("vector %d (%s): either text or vector must be provided", i, vector.ID)
		}

		if hasText && hasVector {
			return fmt.Errorf("vector %d (%s): cannot provide both text and vector", i, vector.ID)
		}
	}

	return nil
}

// GetIndex returns the underlying vector index.
func (u *Upsert) GetIndex() *Index {
	return u.index
}

// SetIndex sets the vector index to use for upserts.
func (u *Upsert) SetIndex(index *Index) {
	u.index = index
}
