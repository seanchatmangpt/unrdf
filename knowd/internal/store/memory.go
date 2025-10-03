// Package store provides storage implementations for knowd.
package store

import (
	"context"
	"encoding/json"
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
func NewMemoryStore(config store.Config) (store.Interface, error) {
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
func (m *MemoryStore) FindQuads(ctx context.Context, pattern Quad) []Quad {
	m.mu.RLock()
	defer m.mu.RUnlock()

	var results []Quad
	
	for _, quad := range m.quads {
		if m.matchesPattern(quad, &pattern) {
			results = append(results, *quad)
		}
	}
	
	return results
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

// quadID generates a unique ID for a quad.
func (m *MemoryStore) quadID(quad *Quad) string {
	data, _ := json.Marshal(quad)
	return fmt.Sprintf("%x", data)
}

// matchesPattern checks if a quad matches a pattern.
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