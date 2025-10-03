// Package store provides storage implementations for knowd.
package store

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"sync"
)

// MemoryStore implements an in-memory RDF quad store.
type MemoryStore struct {
	mu     sync.RWMutex
	quads  map[string]*Quad
	config Config
}

// NewMemoryStore creates a new in-memory store.
func NewMemoryStore(config Config) (Interface, error) {
	return &MemoryStore{
		quads:  make(map[string]*Quad),
		config: Config{MaxQuads: config.MaxQuads},
	}, nil
}

// AddQuad adds a quad to the memory store.
func (m *MemoryStore) AddQuad(ctx context.Context, quad Quad) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	// Check limits
	if m.config.MaxQuads > 0 && len(m.quads) >= m.config.MaxQuads {
		return fmt.Errorf("store limit reached: %d quads", m.config.MaxQuads)
	}

	// Generate a unique ID for the quad
	quadID := m.quadID(&quad)

	// Create a copy to store
	quadCopy := &Quad{
		Subject:   quad.Subject,
		Predicate: quad.Predicate,
		Object:    quad.Object,
		Graph:     quad.Graph,
	}

	m.quads[quadID] = quadCopy
	return nil
}

// RemoveQuad removes a quad from the memory store.
func (m *MemoryStore) RemoveQuad(ctx context.Context, quad Quad) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	quadID := m.quadID(&quad)
	delete(m.quads, quadID)
	return nil
}

// HasQuad checks if a quad exists in the memory store.
func (m *MemoryStore) HasQuad(ctx context.Context, quad Quad) (bool, error) {
	m.mu.RLock()
	defer m.mu.RUnlock()

	quadID := m.quadID(&quad)
	_, exists := m.quads[quadID]
	return exists, nil
}

// FindQuads finds quads matching a pattern.
func (m *MemoryStore) FindQuads(ctx context.Context, pattern Quad) ([]Quad, error) {
	m.mu.RLock()
	defer m.mu.RUnlock()

	var results []Quad

	for _, quad := range m.quads {
		if m.matchesPattern(quad, &pattern) {
			results = append(results, *quad)
		}
	}

	return results, nil
}

// GetQuadCount returns the total number of quads in the store.
func (m *MemoryStore) GetQuadCount() int {
	m.mu.RLock()
	defer m.mu.RUnlock()
	return len(m.quads)
}

// Close closes the memory store (no-op for in-memory).
func (m *MemoryStore) Close() error {
	return nil
}

// quadID generates a unique ID for a quad using SHA256 hashing.
func (m *MemoryStore) quadID(quad *Quad) string {
	// Create a deterministic string representation of the quad
	quadStr := fmt.Sprintf("%s|%s|%s|%s", quad.Subject, quad.Predicate, quad.Object, quad.Graph)
	hash := sha256.Sum256([]byte(quadStr))
	return hex.EncodeToString(hash[:])
}

// matchesPattern checks if a quad matches a pattern.
// Empty pattern fields match any value.
func (m *MemoryStore) matchesPattern(quad *Quad, pattern *Quad) bool {
	if pattern.Subject != "" && pattern.Subject != quad.Subject {
		return false
	}
	if pattern.Predicate != "" && pattern.Predicate != quad.Predicate {
		return false
	}
	if pattern.Object != "" && pattern.Object != quad.Object {
		return false
	}
	if pattern.Graph != "" && pattern.Graph != quad.Graph {
		return false
	}
	return true
}
